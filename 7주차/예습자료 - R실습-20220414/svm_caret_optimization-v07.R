
# nnet model optimization by caret
# source
# https://medium.com/@yolandawiyono98/ann-classification-with-nnet-package-in-r-3c4dc14d1f14
# https://rpubs.com/uky994/593668

# reference:
# http://r-bong.blogspot.com/p/4_25.html

# dataset:
# Recreational Fishing Data 
# download: https://vincentarelbundock.github.io/Rdatasets/articles/data.html
# doc: https://vincentarelbundock.github.io/Rdatasets/doc/Ecdat/Fishing.html

# Source code listing.
library(tidyverse)
library(rpart)
library(e1071) # svmLinear
library(caret)
library(kernlab) # svmLinear2


set.seed(100) # Reproducibility setting

names(getModelInfo("svm"))
modelLookup("svmLinear2")

# read the data
getwd()
# setwd("L:/내 드라이브/Rmarkdown")
fishing<- read.csv("Fishing.csv") 
str(fishing)
data <- fishing

# select variables
mode<- factor(data$mode, levels = c("beach", "boat", "charter", "pier" ))
price <- as.numeric(data$price) 
catch<- as.numeric(data$catch)
income<- as.numeric(data$income)
ds<- data.frame(mode, price, catch, income)
str(ds)

# data preprocessing using caret::preProcess
# center and scale 
preProcValues = preProcess(ds) 
#The function preProcess estimates the required parameters for each operation and predict.preProcess is used to apply them to specific data sets. This function can also be interfaces when calling the train function.
ds <- predict(preProcValues, ds)

summary(ds)

# split the data into tr and ts sets 
set.seed(4) # random seed
indexes = createDataPartition(ds$mode, p = .6, list = F)
train = ds[indexes, ]
test = ds[-indexes, ]
train
test

# base model rpart

fit = rpart(mode~., 
            data = train
            )

# printcp(fit)
library(rattle)
# fancyRpartPlot(fit)

pred = predict(fit, test, type="class")
# print(data.frame(test, pred))
confusionMatrix(pred, test$mode)
acc_rpart <- confusionMatrix(pred, test$mode)$overall['Accuracy']
acc_rpart

# caret train method
# SVM optimizaton
# optimize model - rough

names(getModelInfo("svm"))
modelLookup("svmLinear2")

# C for cost 

# optimization
trControl <- trainControl(method='repeatedcv', number = 10, repeats = 2)
model1 <- train(mode ~.,
              data = train,
              method = 'svmLinear2',
              metric = 'Accuracy',
              tuneLength = 10,
              trControl = trControl
)

# show model 
model1

# plot(model1)
# show final model
model1$finalModel
model1$bestTune

# Model Evaluation
# Predict testing set
pred <- predict(model1, test) 
# pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
pred
# print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)
acc1 <- confusionMatrix(pred, test$mode)$overall['Accuracy']
acc1

# for more optimization
# grid values
tuneGrid = expand.grid(cost = 10**(-4:0))
tuneGrid
trControl <- trainControl(method = 'repeatedcv', 
                         number = 5, 
                         repeats = 2, 
                         returnResamp = 'final')
model2 <- train(mode ~.,
              data = train,
              method = 'svmLinear2',
              metric = 'Accuracy',
              trControl = trControl,
              tuneGrid = tuneGrid
)
model2
model2$finalModel
model2$bestTune

# plot(model2)

# Model Evaluation

#Model Evaluation
#Predict test set
pred <- predict(model2, test) 
#pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
pred
# print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)
acc2 <- confusionMatrix(pred, test$mode)$overall['Accuracy']
acc2
# Non-linear kernel - Radial

modelLookup("svmRadial")

# optimization
trControl <- trainControl(method='repeatedcv', number = 10, repeats = 2)
model3 <- train(mode ~.,
                data = train,
                method = 'svmRadial',
                metric = 'Accuracy',
                trControl = trControl,
                #tuneGrid=grid,
                tuneLength = 5
)

# show model 
model3

# plot(model3)
# show final model
model3$finalModel
model3$bestTune

# Model Evaluation
# Predict testing set
pred <- predict(model3, newdata = test) 
pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
pred
# print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)
acc3 <- confusionMatrix(pred, test$mode)$overall['Accuracy']
acc3
# Non-linear kernel - Polynomial

modelLookup("svmPoly")

# C for cost 

# optimization
trControl <- trainControl(method='repeatedcv', number = 10, repeats = 2)
model4 <- train(mode ~.,
                data = train,
                method = 'svmPoly',
                metric = 'Accuracy',
                trControl = trControl,
                #tuneGrid=grid,
                tuneLength = 5
)

# show model 
model4

# plot(model4)
# show final model
model4$finalModel
model4$bestTune

# Model Evaluation
# Predict testing set
pred <- predict(model4, newdata = test) 
pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
pred
# print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)
acc4 <- confusionMatrix(pred, test$mode)$overall['Accuracy']

result <- tibble(Model = c('SVM Linear', 
                           'SVM Linear w/choice of cost',
                           'SVM Radial',
                           'SVM POly',
                           'RPART'),
                 Accuracy = c(acc1, 
                              acc2, 
                              acc3,
                              acc4,
                              acc_rpart)
)

result %>% arrange(desc(Accuracy))
                 

  