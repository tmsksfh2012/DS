# rpart tree pruning

# Source 
# https://rstudio-pubs-static.s3.amazonaws.com/332255_d2b0ffd26ff64704a65573ba82a90ad6.html

# Data
# heart.csv 
# https://www.kaggle.com/zhaoyingzhu/Heartcsv
# Heart desease data 
# Target var : AHD with two values Yes and No

library(rpart)
library(caret)
library(e1071)
library(rattle)
library(tidyverse)

df<-read.csv('Heart.csv')
str(df)
head(df)
df$AHD <- as.factor(df$AHD)

# data partition

set.seed(10) # reproducability setting
intrain<-createDataPartition(y=df$AHD, p=0.7, list=FALSE) 
train<-df[intrain, ]
test<-df[-intrain, ]

# full tree model

rpartmod<-rpart(AHD~. , data=train, method="class")
fancyRpartPlot(rpartmod)
rpartpred<-predict(rpartmod, test, type='class')
confusionMatrix(rpartpred, test$AHD)

printcp(rpartmod)
plotcp(rpartmod)

# choose the model (cp= 041) leftmost under the horizontal line 
# which xerror is within the range of standard error 
# from min xerror of the model with cp = 0.18

ptree<-prune(rpartmod, cp = 0.041)
fancyRpartPlot(ptree)

rpartpred<-predict(ptree, test, type='class')
confusionMatrix(rpartpred, test$AHD)
