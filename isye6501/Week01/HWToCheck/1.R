#### Scott Foxworth ISYE 6501X 2T2018 HW1 5/16/2018

#Load Libraries
library(kernlab)
library(e1071)
library(ggplot2)
library(kknn)
library(caret)

#Load Data
data <- read.table("creditcarddataA.txt")

#Question 2
#2.1
  #Build Model - vanilladot kernel
    svmmodel <- ksvm(
                    as.matrix(data[,1:10])
                    , as.factor(data[,11])
                    , type = "C-svc"
                    , kernel = "vanilladot"
                    , C = 100
                    , scaled = TRUE
                    )

    #Calculate a1..am
    a <- colSums(svmmodel@xmatrix[[1]] * svmmodel@coef[[1]])
    a

    #Calcuate a0
    a0 <- -svmmodel@b
    a0

    #Predict
    pred <- predict(svmmodel, data[,1:10])
    pred

    #Model Accuracy
    sum(pred == data[,11]) / nrow(data)
  
#2.2  
  #Build Model - bessel kernel
    svmmodel <- ksvm(
                    as.matrix(data[,1:10])
                    , as.factor(data[,11])
                    , type = "C-svc"
                    , kernel = "bessel"
                    , C = 100
                    , scaled = TRUE
                    )
  
    #Calculate a1..am
    a <- colSums(svmmodel@xmatrix[[1]] * svmmodel@coef[[1]])
    a
  
    #Calcuate a0
    a0 <- -svmmodel@b
    a0
  
    #Predict
    pred <- predict(svmmodel, data[,1:10])
    pred
  
    #Model Accuracy
    sum(pred == data[,11]) / nrow(data)

#2.3

    #Build KNN Model
    #Loop Through Data Points
    pred < rep(0,(nrow(data)))
    for (i in 1:nrow(data)){
                        knnmodel <- kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10
                        , data[-i,]                                         
                        , data[i,]
                        , k = 10
                        , scale = TRUE
                        )

    
    pred[i] <- as.integer(fitted(knnmodel) + 0.5)
    }
  
    #Model Accuracy
    sum(pred == data[,11]) / nrow(data)
    
    
#Question 3
#a  

    #Transform response varible to factor to get nibary response  
    data[,11] <- as.factor(data[,11])
    #Split data in test and train
    sample <- sample(1:654, 654*0.7)
    train <- data[sample,]
    test <- data[-sample,]
    
    #Build model on training set
    kknncv <- train.kknn(train[-i,11]~.
                         , data = train[-i,1:10]
                         )
    kknncv
    
    
    #Calculate a1..am
    a <- colSums(kknncvl@xmatrix[[1]] * kknncvmodel@coef[[1]])
    a
    
    #Calcuate a0
    a0 <- -kknncv@b
    a0
    
    #test model mon test set
    pred <- predict(kknncv, test[,-11])
    pred
    
    #Confusion Matrix
    cm <- table(test[,11], pred)
    cm
    
    #Model accuracy
    sum(diag(cm))/sum(cm)
    
    
    ##########
    kknncv <- cv.kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10
                      , data[-i,]
                      , kcv = 10)
    plot(kknncv)
    
    
    ?cv.kknn
    trcontrol <- TrainControl(method = 'cv'
                            , number = 10)
                            
    knnmodelcv <- train.kknn(data[,11]~.
                             , method = 'knn'
                             , tunegrid = expand.grid(k = 1:10)
                             , trcontrol = 'Accuracy'
                             , data = data)
                             
    knnmodel
    
    
    #Test model on test data
    pred <- predict(knnmodel, test)
    pred    
    
    #Confusion Matrix
    cm <- table(test[,11], pred)    
    cm

    #Model Accuracy
    sum(diag(cm))/sum(cm)
    
    
    
  
    