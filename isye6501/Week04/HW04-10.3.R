findat <- data.frame(read.table("10.3germancreditSummer2018.txt", header = F))
findat[,21] <- findat[,21] - 1

set.seed(1)

train_indices <- sample(1:nrow(findat), size = round(nrow(findat) * .8))
train = findat[train_indices,]
test = findat[-train_indices,]

func = V21 ~ factor(V1) + V2 + factor(V3) + factor(V4) + V5 + factor(V6) + factor(V7) + V8 + factor(V9) + factor(V10) + V11 + factor(V12) + V13 + factor(V14) + factor(V15) + V16 + factor(V17) + V18 + factor(V19) + factor(V20)
model = glm(func, family = binomial(link = "logit"), data = train)
summary(model)
cv.glm(train, model, K = 10)$delta

pred = predict.glm(model, newdata = test, type = 'response')

thresholdlist <- c(1:50)/50
totalcost = matrix(, length(thresholdlist), ncol = 2)
n = 1
for (testthresh in thresholdlist) {
  answerx = matrix(, nrow(test), ncol = 1)
  for (x in 1:length(pred)) {
    if (pred[x] >= testthresh) {
      answerx[x] = 1
    } else
      answerx[x] = 0
  }
  cm = confusionMatrix(data = as.factor(answerx),
                       reference = as.factor(test$V21),
                       positive = '0')
  cost = cm$table[2, 1] * 1 + cm$table[1, 2] * 5
  totalcost[n, 1] = testthresh
  totalcost[n, 2] = cost
  n = n + 1
}

opt_threshold <- totalcost[which.min(totalcost[, 2]), 1]
cat(sprintf("Optimum threshold is %s\n", opt_threshold))

answerx = matrix(, nrow(test), ncol = 1)
for (x in 1:length(pred)) {
  if (pred[x] >= opt_threshold) {
    answerx[x] = 1
  } else
    answerx[x] = 0
}

cm = confusionMatrix(data = as.factor(answerx),
                     reference = as.factor(test$V21),
                     positive = '0')

