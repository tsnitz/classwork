library(tree)
library(randomForest)
set.seed(1)

calc.RMSE <- function(predicted, actual){
  RMSE <- sqrt(mean((predicted - actual)^2))
}

crime <- read.table("9.1uscrimeSummer2018.txt", header=TRUE)
test_ind <- sample(1:nrow(crime), size=12)
crime_train <- crime[-test_ind,]
crime_test <- crime[test_ind,]

cart_model <- tree(Crime~., data=crime_train, x=TRUE, y=TRUE)
summary(cart_model)
cart_rmse_train <- calc.RMSE(predict(cart_model), cart_model$y)
cart_mean_abs_e_train <- mean(abs((predict(cart_model) - cart_model$y)/cart_model$y))
cat(sprintf("Training RMSE: %s", round(cart_rmse_train,1)))
cat(sprintf("For training data: On average, the model predicted value is within %s percent of the actual value", round(100*cart_mean_abs_e_train,1)))
cart_rmse_test <- calc.RMSE(predict(cart_model, crime_test), crime_test$Crime)
cart_mean_abs_e_test <- mean(abs((predict(cart_model, crime_test) - crime_test$Crime)/crime_test$Crime))
cat(sprintf("Test RMSE: %s", round(cart_rmse_test,1)))
cat(sprintf("For testing data: On average, the model predicted value is within %s percent of the actual value", round(100*cart_mean_abs_e_test,1)))

numpred <- 4
rf_model <- randomForest(Crime~., data=crime_train, mtry=numpred, importance=TRUE)
summary(rf_model)
rf_rmse_train <- calc.RMSE(predict(rf_model), rf_model$y)
rf_mean_abs_e_train <- mean(abs((predict(rf_model) - rf_model$y)/rf_model$y))
cat(sprintf("RMSE: %s", round(rf_rmse_train,1)))
cat(sprintf("On average, the model predicted value is within %s percent of the actual value", round(100*rf_mean_abs_e_train,1)))
rf_rmse_test <- calc.RMSE(predict(rf_model, crime_test), crime_test$Crime)
rf_mean_abs_e_test <- mean(abs((predict(rf_model, crime_test) - crime_test$Crime)/crime_test$Crime))
cat(sprintf("RMSE: %s", round(rf_rmse_test,1)))
cat(sprintf("On average, the model predicted value is within %s percent of the actual value", round(100*rf_mean_abs_e_test,1)))



