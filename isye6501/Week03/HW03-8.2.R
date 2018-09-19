crime <- read.table("8.2uscrimeSummer2018.txt", header=TRUE)

linmodel <- lm(Crime~., crime)

summary(linmodel)$coefficients
summary(linmodel)$r.squared
summary(linmodel)$adj.r.squared

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

pred <- predict(linmodel, testpoint)
cat(pred)




