#REVENUE TOTAL

#Input Data Subdistrict 2
library(readxl)
library(dplyr)
data=read_excel("D:/S2/Statistical Machine Learning/dataset_asml_2023.xlsx",
                sheet = "dataset_sml_2023")
sub2=subset(data, substr(data$subdistrict, 13, 13) == '2')

#Data Pre-Processing
varians=sapply(sub2, function(x) length(unique(x)) == 1)
View(data.frame(varians)) #Varians 0 (variabel konstan)

#Menghapus variabel konstan dan ID
subdistrict2=select(sub2, -dt_id,
                    -revenue_category,
                    -eNodebid_x,
                    -video_streaming_start_delay,
                    -bad_MR_UMTS,
                    -good_MR_UMTS,
                    -normal_MR_UMTS,
                    -subdistrict)
subdistrict2=data.frame(subdistrict2)
str(subdistrict2) #Memeriksa Struktur Data
#mengubah variabel kategori ke faktor
subdistrict2$promotion=as.numeric(subdistrict2$promotion)
subdistrict2$revenue_total=as.numeric(subdistrict2$revenue_total)
#promotion berhubungan dg revenue_total
pvalue=rep(0,ncol(subdistrict2)-2)
data=subdistrict2[,!names(subdistrict2) %in% c("revenue_total","promotion")]
names(pvalue)=colnames(data)
for(i in 1:length(pvalue)){
  pvalue[i]=cor.test(data[,i],subdistrict2[,"revenue_total"])$p.value
}
pvalue[which(pvalue>0.05)]
# MULTIKOLINIERITAS
cor=cor(data)
k=c(0,0)
for(i in 1:nrow(cor)){
  j <- i+1
  while(j <= ncol(cor)){
    n <- rep(0,2)
    if(abs(cor[i,j]) > 0.9) {
      n[1] <- rownames(cor)[i]
      n[2] <- colnames(cor)[j]
      k <- rbind(k,n)
    }
    j <- j+1
  }
}; k[-1,]
# banyak variabel prediktor numerik saling berhubungan satu sama lain
pvalue = rep(0,ncol(subdistrict2)-2)
data = subdistrict2[,!names(subdistrict2) %in% c("revenue_total","promotion")]
names(pvalue) = colnames(data)
for(i in 1:length(pvalue)){
  pvalue[i] <- anova(lm(data[,i]~subdistrict2[,"promotion"]))$"Pr(>F)"[1]
}
pvalue[which(pvalue>0.05)]
#MISSING VALUE
sum(is.na(subdistrict2))
#tidak ada missing value
#Membagi Data Train dan Test
library(caret)
set.seed(1000)
sampel=sample(1:nrow(sub2),0.80*nrow(subdistrict2))
train=data.frame(subdistrict2)[sampel,]
test=data.frame(subdistrict2)[-sampel,]
# Memisahkan variabel prediktor dan variabel target pada data training
train_x <- train[, !names(train) %in% "revenue_total"]
train_y <- train$revenue_total
# Memisahkan variabel prediktor dan variabel target pada data testing
test_x <- test[, !names(test) %in% "revenue_total"]
test_y <- test$revenue_total
# SCALING/Normalisasi
library(scales)
scale_trainx = train_x
scale = names(scale_trainx)[!names(scale_trainx) %in% "promotion"]
scale_trainx[, scale] <- scale(scale_trainx[, scale])
#rata-rata dari setiap kolom kecuali kolom 'promotion'
center = colMeans(train_x[, !colnames(train_x) %in% "promotion"])
#Standar deviasi dari setiap kolom kecuali kolom 'promotion'
std = sapply(train_x[,!names(train_x) %in% "promotion"], sd)
scale_testx = test_x
for(i in 1:ncol(scale_testx)){
  for(j in 1:ncol(train_x[,!names(train_x) %in% "promotion"])){
    if(colnames(scale_testx)[i]==names(center)[j]){scale_testx[,i] = (scale_testx[,i]-center[j])/std[j]}
  }
}
scale_train = train
scale_train[,!names(scale_train) %in% c("promotion","revenue_total")] = scale(train[,!names(train) %in% c("promotion","revenue_total")])
#PEMODELAN
######## GLM Elastic Net #######3
library(glmnet)
library(caret)
tr_ctrl = trainControl(method = "cv",number=10)
Model1 = train(revenue_total~., data = scale_train, 
               method = "glmnet",
               family = "gaussian",
               trControl=tr_ctrl,
               tuneGrid = data.frame(alpha=seq(0.05,0.5,0.05),
                                     lambda=seq(0.05,0.5,0.05)),
               importance = TRUE)
fit1 = predict(Model1,scale_trainx)
predict1 = predict(Model1,scale_testx)
library(Metrics)
rmse_train1 = rmse(train_y,fit1); rmse_train1
rmse_test1 = rmse(test_y,predict1); rmse_test1
rsq1 = sum((fit1-mean(train_y))^2)/sum((train_y-mean(train_y))^2); rsq1
importance1 = as.data.frame(varImp(Model1)$importance)
importance1 = tibble::rownames_to_column(importance1,"Variable")
importance1 = importance1[order(importance1$Overall, decreasing = T),]
importance1 = data.frame(importance1[1:15,"Variable"],importance1[1:15,"Overall"])
colnames(importance1) = c("variable","importance"); importance1
win.graph()
ggplot(importance1[1:15,], aes(x = importance, y = reorder(variable, importance))) + geom_col(aes(fill = -importance)) +
  geom_text(aes(label = round(importance,2)), hjust = -0.1) + ylab(element_blank()) 

######## CART (Classification and Regression Trees) #########
library(rpart)
library(rpart.plot)
Model2 = rpart(revenue_total~.,data = scale_train, method = "anova")
fit2 = predict(Model2,scale_trainx)
predict2 = predict(Model2,scale_testx)
rmse_train2 = rmse(train_y,fit2); rmse_train2
rmse_test2 = rmse(test_y,predict2); rmse_test2
rsq2 = sum((fit2-mean(train_y))^2)/sum((train_y-mean(train_y))^2); rsq2
importance2 = varImp(Model2)
importance2 = tibble::rownames_to_column(importance2,"Variable")
importance2 = importance2[order(importance2$Overall, decreasing = T),]
importance2 = data.frame(importance2[1:15,"Variable"],importance2[1:15,"Overall"])
colnames(importance2) = c("variable","importance"); importance2
importance2[,2] = (importance2[,2]/importance2[1,2])*100; importance2
win.graph()
ggplot(importance2[1:15,], aes(x = importance, y = reorder(variable, importance))) + geom_col(aes(fill = -importance)) +
  geom_text(aes(label = round(importance,2)), hjust = -0.1) + ylab(element_blank()) 
win.graph()
rpart.plot(Model2)

####### GRADIENT BOOSTING ########
library(gbm)
Grid = expand.grid(shrinkage = 0.1, n.minobsinnode = 50, n.trees = 100, 
                   interaction.depth = 2)
Model3 = train(revenue_total~.,data = scale_train,
               method = "gbm", tuneGrid = Grid)
fit3 = predict(Model3,scale_trainx)
predict3 = predict(Model3,scale_testx)
rmse_train3 = rmse(train_y,fit3); rmse_train3
rmse_test3 = rmse(test_y,predict3); rmse_test3
rsq3 = sum((fit3-mean(train_y))^2)/sum((train_y-mean(train_y))^2); rsq3
importance3 = as.data.frame(varImp(Model3)$importance)
importance3 = tibble::rownames_to_column(importance3,"Variable")
importance3 = importance3[order(importance3$Overall, decreasing = T),]
importance3 = data.frame(importance3[1:15,"Variable"],importance3[1:15,"Overall"])
colnames(importance3) <- c("variable","importance"); importance3
win.graph()
ggplot(importance3[1:15,], aes(x = importance, y = reorder(variable, importance))) + geom_col(aes(fill = -importance)) +
  geom_text(aes(label = round(importance,2)), hjust = -0.1) + ylab(element_blank()) 

#Perbandingan RMSE
RMSE_train=data.frame(id = seq(1,3),method = c("Elastic Net","CART",
                                               "Gradient Boosting"),
                      value = c(rmse_train1,rmse_train2 ,rmse_train3))
RMSE_test=data.frame(id = seq(1,3),method = c("Elastic Net","CART",
                                              "Gradient Boosting"),
                     value = c(rmse_test1,rmse_test2,rmse_test3))                     
win.graph()
plot_train = ggplot(RMSE_train, aes(x = value, y = reorder(method, value))) + geom_col(aes(fill = -value)) +
  geom_text(aes(label = round(value)), hjust = -0.000000001) + ylab(element_blank()) +
  labs(title = "Regression Performance on Training Data")
win.graph()
plot_test = ggplot(RMSE_test, aes(x = value, y = reorder(method, value))) + geom_col(aes(fill = -value)) +
  geom_text(aes(label = round(value)), hjust = -0.000000001) + ylab(element_blank()) +
  labs(title = "Regression Performance on Testing Data")
library("ggpubr")
win.graph()
ggarrange(plot_train, plot_test,
          ncol = 1, nrow = 2)


#------------ DATA KESELURUHAN -------------------#
library(readxl)
library(dplyr)
data2=read_excel("D:/S2/Statistical Machine Learning/dataset_asml_2023.xlsx",
                 sheet = "dataset_sml_2023")
#Data Pre-Processing
varians=sapply(data2, function(x) length(unique(x)) == 1)
View(data.frame(varians)) #Varians 0 (variabel konstan)
#Menghapus variabel konstan dan ID
reg_data = select(data2, -dt_id,
                  -revenue_category,
                  -eNodebid_x,
                  -video_streaming_start_delay,
                  -bad_MR_UMTS,
                  -good_MR_UMTS,
                  -normal_MR_UMTS,
                  -subdistrict)
reg_data=data.frame(reg_data)
str(reg_data) #Memeriksa Struktur Data
#mengubah variabel kategori ke faktor
reg_data$promotion=as.factor(reg_data$promotion)
reg_data = reg_data[-which(reg_data$TCP_SR<0),]
reg_data = reg_data[-which(reg_data$volte_traffic<0),]
reg_data = reg_data[-which(reg_data$TTI<0),]
# FEATURE SELECTION
anova(lm(reg_data$revenue_total~reg_data$promotion))$"Pr(>F)"[1]
# promotion berhubungan dg revenue_total
pvalue = rep(0,ncol(reg_data)-2)
data = reg_data[,!names(reg_data) %in% c("revenue_total","promotion")]
names(pvalue) = colnames(data)
for(i in 1:length(pvalue)){
  pvalue[i] = cor.test(data[,i],reg_data[,"revenue_total"])$p.value
}
pvalue[which(pvalue>0.05)]
# AVAIL<98, im_interactive_delay_ms, bad_Voice tidak berhubungan dengan revenue_total
reg_data=reg_data[,!names(reg_data) %in% c("AVAIL<98",
                                           "im_interactive_delay_ms",
                                           "bad_Voice")]
# banyak variabel prediktor numerik berhubungan satu sama lain
pvalue = rep(0,ncol(reg_data)-2)
data = reg_data[,!names(reg_data) %in% c("revenue_total","promotion")]
names(pvalue) = colnames(data)
for(i in 1:length(pvalue)){
  pvalue[i] = anova(lm(data[,i]~reg_data[,"promotion"]))$"Pr(>F)"[1]
}
pvalue[which(pvalue>0.05)]
# mayoritas variabel prediktor numerik berhubungan dengan "promotion"
# MISSING VALUE
sum(is.na(reg_data))
#tidak ada missing value
#Membagi Data Train dan Test
library("caret")
set.seed(1000)
sampel=sample(1:nrow(reg_data),0.80*nrow(reg_data))
train=data.frame(reg_data)[sampel,]
test=data.frame(reg_data)[-sampel,]
# Memisahkan variabel prediktor dan variabel target pada data training
train_x <- train[, !names(train) %in% "revenue_total"]
train_y <- train$revenue_total
# Memisahkan variabel prediktor dan variabel target pada data testing
test_x <- test[, !names(test) %in% "revenue_total"]
test_y <- test$revenue_total
# SCALING
library("scales")
scale_trainx = train_x
scale_trainx[,!names(scale_trainx) %in% "promotion"] = scale(train_x[,!names(train_x) %in% "promotion"])
center = colMeans(train_x[,!names(train_x) %in% "promotion"])
std = sapply(train_x[,!names(train_x) %in% "promotion"], sd)
scale_testx = test_x
for(i in 1:ncol(scale_testx)){
  for(j in 1:ncol(train_x[,!names(train_x) %in% "promotion"])){
    if(colnames(scale_testx)[i]==names(center)[j]){scale_testx[,i] = (scale_testx[,i]-center[j])/std[j]}
  }
}
scale_train = train
scale_train[,!names(scale_train) %in% c("promotion","revenue_total")] = scale(train[,!names(train) %in% c("promotion","revenue_total")])
# PEMODELAN
# LINEAR REGRESSION WITH ELASTIC NET
library("glmnet")
library("caret")
tr_ctrl = trainControl(method = "cv",number=10)
Model = train(revenue_total~., data = scale_train, 
              method = "glmnet",
              family = "gaussian",
              trControl=tr_ctrl,
              tuneGrid = data.frame(alpha=seq(0.05,0.5,0.05),
                                    lambda=seq(0.05,0.5,0.05)),
              importance = TRUE)
fit = predict(Model,scale_trainx)
predict = predict(Model,scale_testx)
library("Metrics")
rmse_train = rmse(train_y,fit); rmse_train
rmse_test = rmse(test_y,predict); rmse_test
rsq = sum((fit-mean(train_y))^2)/sum((train_y-mean(train_y))^2); rsq
importance = as.data.frame(varImp(Model)$importance)
importance = tibble::rownames_to_column(importance,"Variable")
importance = importance[order(importance$Overall, decreasing = T),]
importance = data.frame(importance[1:15,"Variable"],importance[1:15,"Overall"])
colnames(importance) = c("variable","importance"); importance
win.graph()
ggplot(importance[1:15,], aes(x = importance, y = reorder(variable, importance))) + geom_col(aes(fill = -importance)) +
  geom_text(aes(label = round(importance,2)), hjust = -0.1) + ylab(element_blank())
