library(kernlab)
library(kknn)

data <- read.table("creditcarddataA.txt", header=TRUE)
attributes(data)


###1/2 Doing a sweep over some different kernels and C values to determine the best SVM model and report out 
###its parameters. Using the full dataset for testing and evaluation here.
accmax <- 0
for (kernel in c("vanilladot", "rbfdot", "anovadot")){
  for (C in c(.01, .1, 1, 10, 100)){

    model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel=kernel,C=C,scaled=TRUE)
    pred <- predict(model,data[,1:10])
    acc <- sum(pred == data[,11]) / nrow(data)
    cat(sprintf("Kernel: %s, C: %s, Accuracy: %s", kernel, C, acc))
    if (acc > accmax) {
      accmax <- acc
      bestkernel <- kernel
      bestC <- C
      a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
      a0 <- model@b
    }

  }
}

sprintf("BEST SVM MODEL| Kernel: %s, C: %s, Accuracy: %s", bestkernel, bestC, accmax)
sprintf("BEST SVM MODEL PARAMETERS|")
a
a0

###3 Doing a sweep over some k values for the KNN classifier to determine and report out best KNN model. Again,
###using the same full data set for training and testing the model.

train_indices <- sample(1:nrow(data), round(.8*nrow(data)))
accmax<-0
for (k in c(1, 2, 4, 8, 16, 32, 64, 128)){
  model <- kknn(as.factor(R1)~., data[train_indices,],data[-train_indices,],scale=TRUE, k=k)
  pred <- model$fitted.values
  acc <- sum(pred == data[-train_indices,11]) / nrow(data[-train_indices,])
  cat(sprintf("k: %s, Accuracy: %s", k, acc))
  if (acc > accmax) {
    accmax <- acc
    bestk <- k
  }
}

sprintf("BEST KNN MODEL| K: %s, Accuracy: %s", bestk, accmax)



