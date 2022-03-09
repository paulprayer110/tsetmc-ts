setwd('C:/Users/cmos/Desktop/salimi/R')
file_test = read.csv(file.choose() , header = TRUE)
x1 <- na.omit(file_test[ , 100])
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


#--------------------------------------------------
list_1 <- list()
for (i in 4:ncol(file_test)) {
  
  alfa <- na.omit(file_test[ , i])
  result_sh <- shapiro.test(alfa)
  list_1 <- append(list_1 , result_sh$p.value)
  df_1 <- data.frame('shapiro_test' , list_1 )
}


columns_name <- colnames(file_test)
columns_name <- columns_name[4:583]
columns_name_all <- c('test' , columns_name)

colnames(df_1) <- columns_name_all

#---------------------------------------------------------

list_2 <- list()

for (c in 4:ncol(file_test)) {
  
  alfa_2 <- na.omit(file_test[ , c])
  result_jb <- jarque.bera.test(alfa_2)
  #result_jb <- jarque.test(as.vector(alfa_2))
  list_2 <- append(list_2 , result_jb$p.value)
  df_2 <- data.frame('jarque_bera' , list_2 )
}

colnames(df_2) <- columns_name_all

#--------------------------------------------------------

list_3 <- list()

for (b in 4:ncol(file_test)) {
  
  alfa_4 <- na.omit(file_test[ , b])
  result_as <- anscombe.test(alfa_4)
  list_3 <- append(list_3 , result_as$p.value)

  df_3 <- data.frame('Anscombe-Glynn' , list_3 )
}

colnames(df_3) <- columns_name_all

#---------------------------------------------------------

list_4 <- list()

for (k in 4:ncol(file_test)) {
  
  alfa_5 <- na.omit(file_test[ , k])
  result_ad <- ad.test(alfa_5)
  list_4 <- append(list_4 , result_ad$p.value)
  
  df_4 <- data.frame('Anderson-Darling' , list_4 )
}

colnames(df_4) <- columns_name_all

#----------------------------------------------------------

list_5 <- list()

for (p in 4:ncol(file_test)) {
  
  alfa_6 <- na.omit(file_test[ , p])
  result_cv <- cvm.test(alfa_6)
  list_5 <- append(list_5 , result_cv$p.value)
  
  df_5 <- data.frame('Anderson-Darling' , list_5 )
}

colnames(df_5) <- columns_name_all

#----------------------------------------------------------

