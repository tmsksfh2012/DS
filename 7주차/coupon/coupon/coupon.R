library(caret)
library(caretEnsemble)
library(psych)
library(rpart)
library(ggplot2)
library(rattle)
library(rpart.plot)
library(Hmisc)
library(Amelia)
library(GGally)
library(MASS)
library(klaR)
library(mice)
library(randomForest)
library(tidyverse)

# 데이터 읽기
ds <- read.csv (
  file = "coupon.csv",
  header = TRUE
)
# 간단한 데이터 EDA
str(ds)
describe(ds)

colSums(is.na(ds))

ds$destination<-factor(ds$destination, levels = c("Home", "No Urgent Place", "Work"))
ds$weather<-factor(ds$weather, levels = c("Rainy", "Snowy", "Sunny"))
ds$gender<-factor(ds$gender, levels = c("Male", "Female"))
ds$has_children<-factor(ds$has_children, levels = c("0", "1"), labels = c("Has, Has not"))
ds$Y<-factor(ds$Y, levels = c(0, 1), labels = c("Not Used", "Used"))
ds$passenger<-as.factor(ds$passenger)
ds$time<-as.factor(ds$time)
ds$expiration<-as.factor(ds$expiration)
ds$maritalStatus<-as.factor(ds$maritalStatus)
ds$CarryAway<-as.factor(ds$CarryAway)
ds$toCoupon_GEQ15min<-factor(ds$toCoupon_GEQ15min, levels = c(0, 1), labels = c("False", "True"))
ds$toCoupon_GEQ25min<-factor(ds$toCoupon_GEQ25min, levels = c(0, 1), labels = c("False", "True"))
ds$direction_same<-factor(ds$direction_same, levels = c(0, 1), labels = c("False", "True"))
ds$income <-as.factor(ds$income)
ds$education <-as.factor(ds$education)
ds$coupon <-as.factor(ds$coupon)

ds$age <-as.numeric(ds$age)
ds$temperature <-as.numeric(ds$temperature)

missmap(ds)

#데이터 정규화
preProcValues = preProcess(ds)
ds <- predict(preProcValues, ds)
summary(ds)

#층화 추출(tr:ts=7:3)
set.seed(10)
idx = createDataPartition(ds$Y, p = .7, list = F)
train <- ds[idx, ]
test <- ds[-idx, ]

prop.table(table(train$Y))*100
prop.table(table(test$Y))*100

control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)

svmLinear_model = train(Y~., data = train, method = 'svmLinear2', metric = 'Accuracy', trControl = control)

predict <- predict(svmLinear_model, test)

confusionMatrix(predict, test$Y)

acc_svmLinear <- confusionMatrix(predict, test$Y)$overall['Accuracy']

