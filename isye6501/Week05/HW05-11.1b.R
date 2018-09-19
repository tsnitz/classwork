library(MASS)
library(glmnet)
library(caret)

crime <- read.table("11.1uscrimeSummer2018.txt", header=TRUE)
crime$So <- as.factor(crime$So)

RMSE_stepwise_list <- c()
RMSE_lasso_list <- c()
RMSE_elastic_list_list <- c()
RMSE_ridge_list <- c()

for (trialnum in 1:10){

  train_indices <- sample(1:nrow(crime), round(0.7*nrow(crime)))
  train <- crime[train_indices,]
  test <- crime[-train_indices,]
  
  #Tempting to try a grid search, but with 16 factors, there are 16! different possible combinations.
  
  fit <- lm(Crime~.,data=train)
  step <- stepAIC(fit, direction="both")
  
  RMSE_stepwise <- sqrt(mean((predict(step,test) - test[,'Crime'])^2))
  
  scalefunc <- preProcess(train[,-ncol(train)])
  train_scaled <- cbind(predict(scalefunc, train[,-ncol(train)]), Crime=train[,ncol(train)])
  lassomodel <- cv.glmnet(x=data.matrix(train_scaled[,-ncol(train)]), y=data.matrix(train_scaled[,ncol(train)]), alpha=1)
  test_scaled <- cbind(predict(scalefunc, test[,-ncol(train)]), test[,ncol(train)])
  lassomodel_preds <- predict(lassomodel$glmnet.fit,as.matrix(as.matrix(sapply(test_scaled, as.numeric))[,-ncol(train)]))
  lassomodel_pred_col <- paste("s",which(lassomodel$lambda == lassomodel$lambda.min)[length(which(lassomodel$lambda == lassomodel$lambda.min))] + 1, sep="")
  RMSE_lasso <- sqrt(mean((lassomodel_preds[,lassomodel_pred_col] - test[,'Crime'])^2))
  
  RMSE_elastic_list <- c()
  for (a in c(0.75, 0.5, 0.25)){
    scalefunc <- preProcess(train[,-ncol(train)])
    train_scaled <- cbind(predict(scalefunc, train[,-ncol(train)]), Crime=train[,ncol(train)])
    elasticmodel <- cv.glmnet(x=data.matrix(train_scaled[,-ncol(train)]), y=data.matrix(train_scaled[,ncol(train)]), alpha=a)
    test_scaled <- cbind(predict(scalefunc, test[,-ncol(train)]), test[,ncol(train)])
    elasticmodel_preds <- predict(elasticmodel$glmnet.fit,as.matrix(as.matrix(sapply(test_scaled, as.numeric))[,-ncol(train)]))
    elasticmodel_pred_col <- paste("s",which(elasticmodel$lambda == elasticmodel$lambda.min)[length(which(elasticmodel$lambda == elasticmodel$lambda.min))] + 1, sep="")
    RMSE <- sqrt(mean((elasticmodel_preds[,elasticmodel_pred_col] - test[,'Crime'])^2))
    RMSE_elastic_list <- c(RMSE_elastic_list, RMSE)
  }
  
  scalefunc <- preProcess(train[,-ncol(train)])
  train_scaled <- cbind(predict(scalefunc, train[,-ncol(train)]), Crime=train[,ncol(train)])
  ridgemodel <- cv.glmnet(x=data.matrix(train_scaled[,-ncol(train)]), y=data.matrix(train_scaled[,ncol(train)]), alpha=0)
  test_scaled <- cbind(predict(scalefunc, test[,-ncol(train)]), test[,ncol(train)])
  ridgemodel_preds <- predict(ridgemodel$glmnet.fit,as.matrix(as.matrix(sapply(test_scaled, as.numeric))[,-ncol(train)]))
  ridgemodel_pred_col <- paste("s",which(ridgemodel$lambda == ridgemodel$lambda.min)[length(which(ridgemodel$lambda == ridgemodel$lambda.min))] + 1, sep="")
  RMSE_ridge <- sqrt(mean((ridgemodel_preds[,ridgemodel_pred_col] - test[,'Crime'])^2))

  RMSE_stepwise_list <- c(RMSE_stepwise_list, RMSE_stepwise)
  RMSE_lasso_list <- c(RMSE_lasso_list, RMSE_lasso)
  RMSE_elastic_list_list <- c(RMSE_elastic_list_list, RMSE_elastic_list)
  RMSE_ridge_list <- c(RMSE_ridge_list, RMSE_ridge)
}


mean(RMSE_stepwise_list)
mean(RMSE_lasso_list)
mean(RMSE_elastic_list_list[seq(1,length(RMSE_elastic_list_list),3)])
mean(RMSE_elastic_list_list[seq(2,length(RMSE_elastic_list_list),3)])
mean(RMSE_elastic_list_list[seq(3,length(RMSE_elastic_list_list),3)])
mean(RMSE_ridge_list)