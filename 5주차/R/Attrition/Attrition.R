library(caret)
library(rpart)
library(ggplot2)
library(rattle)
library(rpart.plot)
## Load dataset
data <- read.csv("Attrition.csv")
names(data)[1] = c("Age")

head(data)
tail(data)

data$Attrition = as.factor(data$Attrition)
data$BusinessTravel = as.factor(data$BusinessTravel)
data$Department = as.factor(data$Department)
data$Education = as.factor(data$Education)
data$EducationField = as.factor(data$EducationField)
data$EmployeeCount = as.factor(data$EmployeeCount)
data$EnvironmentSatisfaction = as.factor(data$EnvironmentSatisfaction)
data$Gender = as.factor(data$Gender)
data$JobInvolvement = as.factor(data$JobInvolvement)
data$JobLevel = as.factor(data$JobLevel)
data$JobRole = as.factor(data$JobRole)
data$JobSatisfaction = as.factor(data$JobSatisfaction)
data$MaritalStatus = as.factor(data$MaritalStatus)
data$Over18 = as.factor(data$Over18)
data$OverTime = as.factor(data$OverTime)
data$PerformanceRating = as.factor(data$PerformanceRating)
data$RelationshipSatisfaction = as.factor(data$RelationshipSatisfaction)
data$StandardHours = as.factor(data$StandardHours)
data$StockOptionLevel = as.factor(data$StockOptionLevel)
data$WorkLifeBalance = as.factor(data$WorkLifeBalance)

## Partitioning
idx <- createDataPartition(y = data$Attrition, p = 0.7, list = FALSE)
training <- data[idx, ]
testing <- data[-idx, ]

## Train model
fullTree <- rpart(Attrition~., training, cp = -1) #cp = -1 -> full tree 출력
fullPred <- predict(fullTree, newdata = testing, type='class')
fancyRpartPlot(fullTree)
confusionMatrix(testing$Attrition, fullPred, positive = 'Yes')

preTree <- rpart(Attrition~., training, maxdepth = 3)
prePred <- predict(preTree, newdata = testing, type='class')
fancyRpartPlot(preTree)
confusionMatrix(testing$Attrition, prePred, positive = 'Yes')

plotcp(fullTree)
printcp(fullTree)

postTree <- prune(fullTree, cp = 0.044)
postPred <- predict(postTree, newdata = testing, type = 'class')
fancyRpartPlot(postTree)
confusionMatrix(testing$Attrition, postPred, positive = 'Yes')

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
# 사전 가지치기 learning curve
lda_data <- learning_curve_dat(
  dat = data,
  outcome = "Attrition",
  test_prop = 3/10,
  method = "rpart",
  metric = "ROC",
  trControl = ctrl, control = rpart.control(maxdepth = 3, parms=list(split = 'gini')))

lda_data <- lda_data[!(lda_data$Data == "Resampling"),]
ggplot(lda_data, aes(x = Training_Size, y = ROC, color = Data)) + geom_smooth(method = loess, span = .8) + theme_bw()
# 사전 가지치기 learning curve 완료

ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
# 사후 가지치기 learning curve
lda_data <- learning_curve_dat(
  dat = data,
  outcome = "Attrition",
  test_prop = 3/10,
  method = "rpart",
  metric = "ROC",
  trControl = ctrl, control = rpart.control(cp = 0.044, parms=list(split = 'gini')))

lda_data <- lda_data[!(lda_data$Data == "Resampling"),]
ggplot(lda_data, aes(x = Training_Size, y = ROC, color = Data)) + geom_smooth(method = loess, span = .8) + theme_bw()
# 사후 가지치기 learning curve 완료

# rpart rules
rpart.rules(postTree)
# rpart.rules 완료
