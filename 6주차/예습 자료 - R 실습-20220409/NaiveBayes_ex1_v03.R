# Naive Bayes example for nb 

# source: 
# https://www.edureka.co/blog/naive-bayes-in-r/

# Data:
# diabetes.csv

# note:
# use nb function in klaR package NOT the one in e1017 
# it is written from naive bayesian from e1017 package.

# reference
# https://uc-r.github.io/naive_bayes

# Step 1: Install and load the requires packages
#Loading required packages

library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)
library(klaR) # for NaiveBayes (=nb) function

# 
# library(e1071)
# Reproducibility setting 
set.seed(100)

# Step 2: Import the data set
#Reading data into R
data<- read.csv("Data/diabetes.csv")

#Setting outcome variables as categorical
data$Outcome <- factor(data$Outcome, levels = c(0,1), labels = c("False", "True"))

# Step 3: Studying the Data Set
#Studying the structure of the data
str(data)
head(data)

# Step 4: Data Cleaning
#Convert '0' values into NA
data[, 2:7][data[, 2:7] == 0] <- NA

#visualize the missing data
missmap(data)
#Use mice package to predict missing values
mice_mod <- mice(data[, c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")], method='rf')
mice_complete <- complete(mice_mod)

#Transfer the predicted missing values into the main data set
data$Glucose <- mice_complete$Glucose
data$BloodPressure <- mice_complete$BloodPressure
data$SkinThickness <- mice_complete$SkinThickness
data$Insulin<- mice_complete$Insulin
data$BMI <- mice_complete$BMI

# check again
missmap(data)

# Step 5: Exploratory Data Analysis

#Data Visualization
#Visual 1
ggplot(data, aes(Age, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by Outcome")

#visual 2
c <- ggplot(data, aes(x=Pregnancies, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="Pregnancy Distribution by Outcome")
c + theme_bw()

#visual 3
P <- ggplot(data, aes(x=BMI, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="BMI Distribution by Outcome")
P + theme_bw()

#visual 4
ggplot(data, aes(Glucose, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Glucose Distribution by Outcome")


#visual 5
ggpairs(data)

# Step 6: Data Modelling
#Building a model
#split data into training and test data sets
indxTrain <- createDataPartition(y = data$Outcome,p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,] #Check dimensions of the split > prop.table(table(data$Outcome)) * 100

# check the class ratioes
prop.table(table(training$Outcome)) * 100
prop.table(table(testing$Outcome)) * 100

#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-9]
y = training$Outcome

# optimize model 
modelLookup("nb")

# set up tune grid values
fL <- seq(0, 1, 0.2)
usekernel <- c(TRUE, FALSE)
adjust <- seq(0, 2, 0.5)
grid <- expand.grid(fL=fL, usekernel=usekernel, adjust=adjust)
grid

# optimization
model = train(x,
              y,
              'nb',
              trControl=trainControl(method='cv',number=10),
              tuneGrid=grid
              )
model

# Step 7: Model Evaluation

#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = testing ) 
Predict

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(Predict, testing$Outcome)

#Plot Variable performance
X <- varImp(model) # Variable importance - RF
plot(X)

