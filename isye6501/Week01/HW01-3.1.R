library(kernlab)
library(kknn)

data <- read.table("creditcarddataA.txt", header=TRUE)
attributes(data)

###(a.) Routine which does leave-one-out cross validation of KNN across different sets of model hyperparameters
###to find best model. The resulting best model is then tested against a hold-out test set. 
train_indices <- sample(1:nrow(data), round(.8*nrow(data)))
model <- train.kknn(as.factor(R1)~., data=data[train_indices,], scale=TRUE, kmax=128)
pred <- predict(model, data[-train_indices, -11])
acc <- sum(pred == data[-train_indices, 11]) / nrow(data[-train_indices,])

sprintf("BEST KNN MODEL| Kernel: %s, K: %s, Test Set Accuracy: %s", model$best.parameters$kernel, model$best.parameters$k, acc)

###(b.) Data splitting routine (train, validate, test) with SVM. Here I basically do what I did in in 2.2.1, except now
###the analysis will use the validation data set to determine the best model and will report out the performance
###achieved on the test set. In 2.2.1, the same data was used for training and testing. This usually leads to
###over-estimation of the model's performance. 
train_indices <- sample(1:nrow(data), round(.7*nrow(data)))
val_test_indices <- (1:nrow(data))[-train_indices]
val_indices <- sample(val_test_indices, round(.5*length(val_test_indices)))
test_indices <- val_test_indices[-val_indices]

train_data <- data[train_indices,]
val_data <- data[val_indices,]
test_data <- data[test_indices,]

valaccmax<-0
for (kernel in c("vanilladot", "rbfdot", "anovadot")){
  for (C in c(.01, .1, 1, 10, 100)){
    model <- ksvm(as.matrix(train_data[,1:10]),as.factor(train_data[,11]),type="C-svc",kernel=kernel,C=C,scaled=TRUE)
    pred <- predict(model,val_data[,1:10])
    acc <- sum(pred == val_data[,11]) / nrow(val_data)
    if (acc > valaccmax) {
      bestmodel <- model
      bestkernel <- kernel
      bestC <- C
    }

  }
}

pred <- predict(model,test_data[,1:10])
acc <- sum(pred == test_data[,11]) / nrow(test_data)

sprintf("BEST SVM MODEL| Kernel: %s, C: %s, Test Set Accuracy: %s", bestkernel, bestC, acc)


