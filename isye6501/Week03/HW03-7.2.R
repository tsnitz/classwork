library(ggplot2)
library(reshape)

temps <- read.table("7.2tempsSummer2018.txt", header=TRUE)

temps$DAY <- as.Date(temps$DAY, '%e-%b')

cusum <- function(data, P, Q){
  ans <- data.frame(S=double(), alarm=integer())
  ans[nrow(ans)+1,] <- c(0,0)
  mu <- mean(data)
  std <- sd(data)
  C <- P * std
  thresh <- Q * std
  for (i in 2:length(data)){
    S <- max(0, ans[[i-1,1]] + (mu - data[i] - C))
    alarm <- S > thresh
    ans[nrow(ans)+1,] <- c(S, alarm)
  }
  ans
}

lm_eqn <- function(linmodel){
  m <- linmodel;
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


### Visual inspection of smoothing on temperatures from 1996. Uses alpha and beta parameters.

  
datchunk <- temps[,2]
smooth <- HoltWinters(datchunk, gamma=FALSE)
smootheddatchunk <- c(datchunk[1],datchunk[2],smooth$fitted[,'xhat'])

comparedat <- data.frame(DAY=temps['DAY'], actual=datchunk, smoothed=smootheddatchunk)

meltedcompare <- melt(comparedat, id = 'DAY')
ggplot(meltedcompare, aes(x = DAY, y = value, colour = variable, group=variable)) +
  geom_line() +
  ggtitle("Plot of actual temperature and smoothed temperature")+
  ylab("Temperature") +
  xlab("Day")

### HoltWinters smoothing and then CUSUM applied to all years. Same parameters for CUSUM as in last 
### lesson. Check the effect on first trip date (the first date where CUSUM crosses the threshold) 
### across all of the years.Both a histogram and a timeseries plot showing this.

P <- 1
Q <- 5
firsttriplist <- c()
for (cnum in 1:(ncol(temps) - 1)){
  tempdat <- temps[,cnum + 1]
  # datchunk <- temps[,2]
  smooth <- HoltWinters(tempdat, gamma=FALSE)
  smootheddatchunk <- c(tempdat[1],tempdat[2],smooth$fitted[,'xhat'])
  ans <- cusum(smootheddatchunk, P, Q)
  firsttrip <- which(ans$alarm == 1)[1]
  firsttriplist <- c(firsttriplist,firsttrip)
}

hist(firsttriplist, xlim=c(40,140), xlab="First CUSUM trip - # of Days after July 1", main=sprintf("Smoothed Input. P: %s, Q: %s, min: %s, max: %s, mean: %s\n", P, Q, min(firsttriplist), max(firsttriplist), mean(firsttriplist)))
firsttripdf <- data.frame(x=seq_along(firsttriplist), y=firsttriplist)

linmodel <- lm(y~x, firsttripdf)
ggplot(firsttripdf, aes(x=x, y=y)) +
geom_point() +
geom_smooth(method='lm') +
geom_text(x = 13, y = 100, label = lm_eqn(linmodel), parse = TRUE)


### This is the same analysis shown in the last lesson for comparison. raw data -> CUSUM

P <- 1
Q <- 5
firsttriplist <- c()
for (cnum in 1:(ncol(temps) - 1)){
  tempdat <- temps[,cnum + 1]
  ans <- cusum(tempdat, P, Q)
  firsttrip <- which(ans$alarm == 1)[1]
  firsttriplist <- c(firsttriplist,firsttrip)
}

hist(firsttriplist, xlim=c(40,140), xlab="First CUSUM trip - # of Days after July 1", main=sprintf("Raw Input. P: %s, Q: %s, min: %s, max: %s, mean: %s\n", P, Q, min(firsttriplist), max(firsttriplist), mean(firsttriplist)))
firsttripdf <- data.frame(x=seq_along(firsttriplist), y=firsttriplist)
linmodel <- lm(y~x, firsttripdf)

ggplot(firsttripdf, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method='lm') +
  geom_text(x = 13, y = 100, label = lm_eqn(linmodel), parse = TRUE)

### Judging from the results above, applying HoltWinters to the raw data doesn't really seem to 
### change anything from the last lesson's analysis. Let's try raw data -> CUSUM -> HoltWinters
### and analyze the HoltWinters parameters to see if we can conclude anything about when summer
### is ending.

smooth <- HoltWinters(firsttriplist, gamma=FALSE)
smoothedfirsttriplist <- c(firsttriplist[1],firsttriplist[2],smooth$fitted[,'xhat'])
firsttripdf[,'smoothed'] <- smoothedfirsttriplist

meltedfirsttripdf <- melt(firsttripdf, id = 'x')
ggplot(meltedfirsttripdf, aes(x = x, y = value, colour = variable, group=variable)) +
  geom_point()

linmodel <- lm(smoothed~x, firsttripdf)
ggplot(firsttripdf, aes(x = x, y = smoothed)) +
  geom_point() + 
  geom_smooth(method='lm') +
  geom_text(x = 13, y = 100, label = lm_eqn(linmodel), parse = TRUE)


