install.packages("dplyr")
install.packages("caret")

library(caret)

fit_rf <- train(지지정당~., trainingset, method="rf")

predict(fit_rf, testset)
