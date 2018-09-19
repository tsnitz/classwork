crime <- read.table("9.1uscrimeSummer2018.txt", header=TRUE)

pca <- prcomp(crime[,1:15], scale. = TRUE)
summary(pca)

# Concatenating the PCA component values to the target value (Crime field in crime df)
pca_crime <- data.frame(cbind(pca$x[,1:15], Crime=crime[,16]))

model <- lm(Crime~., data=pca_crime)
summary(model)

model_pca_coef <- model$coefficients[2:length(model$coefficients)]
pca_x_comp <- t(pca$rotation[,1:length(model$coefficients) - 1])
scaled_coef <- model_pca_coef%*%pca_x_comp
scaled_coef

intercept <- model$coefficients[1] - sum(scaled_coef*sapply(crime[,1:15], mean)/sapply(crime[,1:15],sd))
coef <- scaled_coef/sapply(crime[,1:15],sd)

summary(model)$r.squared
summary(model)$adj.r.squared

M = 14.0
So = 0
Ed = 10.0
Po1 = 12.0
Po2 = 15.5
LF = 0.640
M.F = 94.0
Pop = 150
NW = 1.1
U1 = 0.120
U2 = 3.6
Wealth = 3200
Ineq = 20.1
Prob = 0.04
Time = 39.0
testpoint <- data.frame(M,So,Ed, Po1, Po2, LF, M.F, Pop, NW, U1, U2, Wealth, Ineq, Prob, Time)
ans <- coef%*%t(as.matrix(testpoint)) + intercept
ans
