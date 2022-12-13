library(readxl)
library(forecast)
library(ncvreg)
library(dplyr)
library(glmnet)
library(tidyverse)
library(glmnet)
########################################################################


########################################################################

### Importation de l'environnement ###
#setwd("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/")
setwd("/Users/emma/Desktop/DRiM/BDDs")
#setwd("/Users/este.pcl/Desktop/DrimGame 2022/Script")
#setwd("C:/Users/alexa/OneDrive/Bureau/M2_EKAP/drim_gameParis")

### Importation des bases de données avec et sans y retardé ###

tot_less_diff <- read_excel('tot_less_diff.xlsx')

chr2_ret <- read_excel('chr2_ret.xlsx')
chr8_ret <- read_excel('chr8_ret.xlsx')

matrice2_ret <- read.xlsx('matrice2_ret.xlsx')
matrice8_ret <- read.xlsx('matrice8_ret.xlsx')
matricetot <- read.xlsx('matricetot.xlsx')


### transformation des bases en dataframe ###
chr2_ret <- data.frame(chr2_ret)
chr8_ret <- data.frame(chr8_ret)


########################################################################


########################################################################

##Prévision TOT
#Prévision ARX_1 retard base tot_less_diffarx
y <- tot_less_diff[1:25,1]

training_dlbase <- tot_less_diff[1:25,]
dependvar <- tot_less_diff[,c("p25_7","median_3","p25_8", "p10_2" , "p10_3","tx_retournement",
                              "p90_8","p90_5","p75_7",
                              "p25_2","change_in_stock","p25_1","p90_3","p95_2")]
indepvar <- tot_less_diff$DR
dependvar2 <- data.frame(dependvar)
mX <- data.matrix(dependvar2[1:25,])
y <- indepvar[1:25]

matricetot_xi <-matricetot[,c("p25_7","median_3","p25_8", "p10_2" , "p10_3","tx_retournement",
                                 "p90_8","p90_5","p75_7",
                                 "p25_2","change_in_stock","p25_1","p90_3","p95_2")]

model <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary")
forecast <- predict(model, n.ahead = 12, newmxreg = data.matrix(matricetot_xi))
forecast
### penser a rajouter la tendance sur excel (série stationnarisée)

##Prévision CHR2 
##Lasso AVEC VARIABLES RETARDEES

### LASSO ###
predlasso_ret=NULL
  x <- data.matrix(chr2_ret[1:24,-c(1)])	
  y <- data.matrix(chr2_ret[1:24,c(1)])
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvlasso <- lasso_cv$lambda.min
  lambda_cvlasso
  model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cvlasso, standardize = T)
  which(! coef(model_cv) == 0, arr.ind = TRUE)
  predlasso_ret2 <- predict(model_cv, newx = as.matrix(matrice2_ret))	
  predlasso_ret2


model_cv$beta

which(! coef(model_cv) == 0, arr.ind = TRUE)


predlasso_ret2 <-c(chr2_ret[24,1],predlasso_ret2 )
predlasso_ret2<-ts(predlasso_ret2, start=c(2015,02), frequency=4)
plot.new()
plot(predlasso_ret2)


##Prévision CHR8 AVEC LASSO + VARIABLES RETARDEES
#variable dépendante
indepvar <- data.frame(chr8_ret) %>%
  select(DR) %>%
  as.matrix()
#variables indépendantes
depvar <- data.frame(chr8_ret) %>%
  select(-DR) %>%
  as.matrix()

predlasso_ret=NULL
  x <- data.matrix(depvar[1:24,])	
  y <- data.matrix(indepvar[1:24])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvlasso <- lasso_cv$lambda.min
  model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cvlasso, standardize = T)
  predlasso_ret <-  predict(model_cv, newx = data.matrix(matrice8_ret))

predlasso_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)
