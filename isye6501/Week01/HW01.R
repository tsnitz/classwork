library(kernlab)

data <- read.table("creditcarddataA.txt", header=TRUE)
attributes(data)
# model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)
# a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
# a0 <- model@b
# pred <- predict(model,data[,1:10])
# acc <- sum(pred == data[,11]) / nrow(data)
for (kernel in c("vanilladot", "rbfdot", "anovadot")){
  for (C in c(.01, .1, 1, 10, 100, 1000, 10000)){

    model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel=kernel,C=C,scaled=TRUE)
    a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
    a0 <- model@b
    pred <- predict(model,data[,1:10])
    acc <- sum(pred == data[,11]) / nrow(data)
    cat(sprintf("Kernel: %s, C: %s, Accuracy: %s", kernel, C, acc))

  }
}

