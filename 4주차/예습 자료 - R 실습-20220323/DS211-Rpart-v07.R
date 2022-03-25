
# Classification with CART model in R
# Source: https://www.datatechnotes.com/2018/11/classification-with-cart-model-in-r.html 

# Classification and Regression Trees (CART) models can be 
# implemented through the rpart package. 
# In this post, we will learn how to classify data with a CART model in R. 
# It covers two types of implementation of CART classification 
# and source code listing.

# 1. Using the rpart() function of 'rpart' package.
# 2. Applying the 'caret' package's the train() with the 'rpart' method.
# 3. Source code listing

# Source code listing.

library(rpart)
library(e1071)
library(caret)
library(tidyverse)
library(rattle)
library(rpart.plot)

Golf_ds <- read.csv(file = "Data/Golf-all.csv", 
                    header = TRUE)
class(Golf_ds)
head(Golf_ds)

# change types of vars and select vars

ds <- data.frame(Outlook = Golf_ds$Outlook, 
                 Temperature = Golf_ds$Temperature, 
                 Humidity = Golf_ds$Humidity, 
                 Play = as.factor(Golf_ds$Play)
                 )
class(ds$Play)

# split the data into tr and ts sets 
# set.seed(4) # random seed

# ds:Golf dataset, 
# 28 obs. of 6 varibles,  remove ID variable. 
indexes = createDataPartition(ds$Play, p = .6, list = F) 
# tr set: 0.6
train = ds[indexes, ]
test = ds[-indexes, ]

# 1st experiment  
# Full tree

# Fit a prediction model
# rpart parameters 
# minsplit: 
# minbucket:
# cp:
fit <- rpart(Play~., 
             data = ds,
             cp = -1, 
             minsplit = 2,
             minbucket = 1 ) 

# prediction model 
printcp(fit)

# plot the tree 
windows()
plot(fit)
text(fit, cex = 0.9, xpd = TRUE)
readline('Enter to resume ')
dev.off() # close window

windows()
fancyRpartPlot(fit)
readline('Enter to resume ')
dev.off() # close window

windows()
rpart.plot(fit)
readline('Enter to resume ')
dev.off() # close window

# Evaluate the performance of the prediction model
pred = predict(fit, test, type = "class", )
print(data.frame(test, pred))
confusionMatrix(test$Play, pred, positive = 'yes')
readline('Enter to resume ')

# 2nd experiment  
# prepruning

# Fit a prediction model
# rpart parameters 
# minsplit: 
# minbucket:
# cp:
fit <- rpart(Play~., 
             data = ds,
             # cp = -1, 
             minsplit = 4,
             minbucket = 2 ) 

# prediction model 
printcp(fit)

# plot the tree 
windows()
plot(fit)
text(fit, cex = 0.9, xpd = TRUE)
readline('Enter to resume ')
dev.off() # close window

windows()
fancyRpartPlot(fit)
readline('Enter to resume ')
dev.off() # close window

windows()
rpart.plot(fit)
readline('Enter to resume ')
dev.off() # close window

# Evaluate the performance of the prediction model
pred = predict(fit, test, type = "class", )
print(data.frame(test, pred))
confusionMatrix(test$Play, pred, positive = 'yes')
