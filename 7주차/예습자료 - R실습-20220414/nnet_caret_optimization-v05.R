
# nnet model optimization by caret
# source
# https://medium.com/@yolandawiyono98/ann-classification-with-nnet-package-in-r-3c4dc14d1f14

# reference:
# https://stackoverflow.com/questions/42417948/how-to-use-size-and-decay-in-nnet
# https://stats.stackexchange.com/questions/65877/convergence-of-neural-network-weights
# https://stats.stackexchange.com/questions/209678/nnet-package-is-it-neccessary-to-scale-data
# weight decay --> https://towardsdatascience.com/this-thing-called-weight-decay-a7cd4bcfccab

# dataset:
# Recreational Fishing Data 
# download: https://vincentarelbundock.github.io/Rdatasets/articles/data.html
# doc: https://vincentarelbundock.github.io/Rdatasets/doc/Ecdat/Fishing.html

# Source code listing.

library(tidyverse)
library(rpart)
library(e1071)
library(caret)
# library(xlsx)
library(nnet)

set.seed(100) # Reproducibility setting

getModelInfo()
modelLookup("nnet")

# read the data
getwd()
# setwd("L:/내 드라이브/Rmarkdown")
fishing<- read.csv("Data/Fishing.csv") 
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

printcp(fit)
library(rattle)
fancyRpartPlot(fit)

pred = predict(fit, test, type = "class" )
print(data.frame(test, pred))
confusionMatrix(pred, test$mode)

# caret train method

# optimize model - rough
modelLookup("nnet")

# optimization
trControl=trainControl(method='repeatedcv', number = 10, repeats = 2)
model = train(mode ~.,
              data = train,
              method = 'nnet',
              maxit = 500,
              metric = 'Accuracy',
              # preProcess = c('center', 'scale'), # data normalization
              # We dont need to this, because the data is already scaled
              
              trControl = trControl,
              #tuneGrid=grid
              tuneLength = 3
)

# show model 
model

# show final model
model$finalModel

# check the convergence of the final model 
model$finalModel$convergence 
# 1 if maxit reached, 0 otherwise

# Model Evaluation
# Predict testing set
pred <- predict(model$finalModel, newdata = test, type = "class") 
pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
pred
print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)

# for more optimization
# grid values
tuneGrid = expand.grid(size = 16:18, decay = 10 ** (-5:-3))
tuneGrid
trControl = trainControl(method = 'repeatedcv', 
                         number = 5, 
                         repeats = 2, 
                         returnResamp = 'final')
model = train(mode ~.,
              data = train,
              method = 'nnet',
              maxit = 1000,
              metric = 'Accuracy',
              trControl = trControl,
              tuneGrid=tuneGrid
)
model

# Step 7: Model Evaluation

#Model Evaluation
#Predict testing set
pred <- predict(model$finalModel, newdata = test, type = "class") 
pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
pred
print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)

#Plot Variable performance
X <- varImp(model$finalModel)
X
plot(X)

names(getModelInfo("svm"))

# optimization
trControl=trainControl(method='repeatedcv', number = 10, repeats = 2)
model = train(mode ~.,
              data = train,
              method = 'nnet',
              maxit = 500,
              metric = 'Accuracy',
              # preProcess = c('center', 'scale'), # data normalization
              # We dont need to this, because the data is already scaled
              
              trControl = trControl,
              #tuneGrid=grid
              tuneLength = 3
)

# show model 
model

# show final model
model$finalModel

# check the convergence of the final model 
model$finalModel$convergence 
# 1 if maxit reached, 0 otherwise
# if maxit is reached, train more(with larger maxit) with the same parameter.

# Model Evaluation
# Predict testing set
pred <- predict(model$finalModel, newdata = test, type = "class") 
pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
pred
print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)

# for more optimization
# grid values
tuneGrid = expand.grid(size = 16:18, decay = 10 ** (-5:-3))
tuneGrid
trControl = trainControl(method = 'repeatedcv', 
                         number = 5, 
                         repeats = 2, 
                         returnResamp = 'final')
model = train(mode ~.,
              data = train,
              method = 'nnet',
              maxit = 1000,
              metric = 'Accuracy',
              trControl = trControl,
              tuneGrid=tuneGrid
)
model

# Step 7: Model Evaluation

#Model Evaluation
#Predict testing set
pred <- predict(model$finalModel, newdata = test, type = "class") 
pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
pred
print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$mode)

  