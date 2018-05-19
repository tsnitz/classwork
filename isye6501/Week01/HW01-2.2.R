library(kernlab)
library(kknn)

data <- read.table("creditcarddataA.txt", header=TRUE)
attributes(data)

# accmax = 0
# for (kernel in c("vanilladot", "rbfdot", "anovadot")){
#   for (C in c(.01, .1, 1, 10, 100)){
# 
#     model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel=kernel,C=C,scaled=TRUE)
#     pred <- predict(model,data[,1:10])
#     acc <- sum(pred == data[,11]) / nrow(data)
#     cat(sprintf("Kernel: %s, C: %s, Accuracy: %s", kernel, C, acc))
#     if (acc > accmax) {
#       accmax = acc
#       bestkernel = kernel
#       bestC = C
#       a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
#       a0 <- model@b
#     }
# 
#   }
# }
# 
# cat(sprintf("BEST SVM MODEL| Kernel: %s, C: %s, Accuracy: %s", bestkernel, bestC, accmax))
# cat(sprintf("BEST SVM MODEL PARAMETERS|"))
# a
# a0
# 
for (k in c(1, 2, 3, 4, 5)){

  model <- kknn(as.matrix(data[-i,1:10]),as.factor(data[-i,11]),scale=TRUE, k=k)
  pred <- predict(model,data[,1:10])
  acc <- sum(pred == data[,11]) / nrow(data)
  cat(sprintf("k: %s, Accuracy: %s", k, acc))
  # if (acc > accmax) {
  #   accmax = acc
  #   bestkernel = kernel
  #   bestC = C
  #   a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
  #   a0 <- model@b
  # }

}



