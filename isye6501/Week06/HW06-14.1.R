library(MASS)

bcd_original <- read.table("14.1breast-cancer-wisconsin.dataSummer2018.txt", header=FALSE, sep=",")
bcd_original[,ncol(bcd_original)] <- as.factor(bcd_original[,ncol(bcd_original)])

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Column 7 is the only column with missing data. We will focus on it.
colnum <- 7

# Impute with mean value
bcd_mean_impute <- bcd_original
missingVector <- bcd_mean_impute[,colnum] == "?"
bcd_mean_impute[which(missingVector),colnum] <- NA
bcd_mean_impute[,colnum] <- as.numeric(bcd_mean_impute[,colnum])
avgValue <- mean(bcd_mean_impute[,colnum], na.rm=TRUE)
bcd_mean_impute[which(missingVector),colnum] <- avgValue
bcd_mean_impute[which(missingVector),colnum]

# Impute with mode value
bcd_mode_impute <- bcd_original
missingVector <- bcd_mode_impute[,colnum] == "?"
bcd_mode_impute[which(missingVector),colnum] <- NA
bcd_mode_impute[,colnum] <- as.numeric(bcd_mode_impute[,colnum])
avgValue <- mode(bcd_mode_impute[,colnum])
bcd_mode_impute[which(missingVector),colnum] <- avgValue
bcd_mode_impute[which(missingVector),colnum]

#Impute with regression
bcd_reg_impute <- bcd_original
missingVector <- bcd_reg_impute[,colnum] == "?"
bcd_reg_impute[which(missingVector),colnum] <- NA
bcd_reg_impute[,colnum] <- as.numeric(bcd_reg_impute[,colnum])
linmodel <- lm(V7~.-V11, bcd_reg_impute)
preds <- predict(linmodel, bcd_reg_impute[which(missingVector),])
bcd_reg_impute[which(missingVector),colnum] <- preds
bcd_reg_impute[which(missingVector),colnum]

#Impute with regression + perturbation
bcd_reg_pert_impute <- bcd_original
missingVector <- bcd_reg_pert_impute[,colnum] == "?"
bcd_reg_pert_impute[which(missingVector),colnum] <- NA
bcd_reg_pert_impute[,colnum] <- as.numeric(bcd_reg_pert_impute[,colnum])
linmodel <- lm(V7~.-V11, bcd_reg_pert_impute)
preds <- predict(linmodel, bcd_reg_pert_impute[which(missingVector),])
residualsMean <- mean(summary(linmodel)$residuals)
residualsStd <- sd(summary(linmodel)$residuals)
perturbs <- rnorm(preds, residualsMean, residualsStd)
perturbed_preds <- preds + perturbs
bcd_reg_pert_impute[which(missingVector),colnum] <- perturbed_preds
bcd_reg_pert_impute[which(missingVector),colnum]


