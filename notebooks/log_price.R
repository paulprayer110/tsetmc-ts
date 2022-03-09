setwd('D:/salimi/R')
file_test = read.csv(file.choose() , header = TRUE)
x1 <- na.omit(file_test[ , 4])
x1
#histogram data
hist(x1 , probability = T )
lines(density(x1), col = "red")

#qqplot
qqnorm(x1 , pch = 1)
qqline(x1, col = "red")


#shapiro test
shapiro.test(x1)
##p-value < 0.05 reject normality assumption



#Skewness and Kurtosis - Jarque-Bera
install.packages('moments')
install.packages('nortest')
install.packages('tseries')
library(moments)
library(nortest)
library(tseries)

jarque.test(as.vector(x1))
jarque.bera.test(x1)
##p-value < 0.05 reject normality assumption


anscombe.test(x1)
ad.test(x1)
cvm.test(x1)


colnames(file_test)

for (i in 1:ncol(file_test)) {
  
  result_sh <- shapiro.test(i)
  
  dd <- data.frame('shapiro_test' , result_sh$p-value)
}
