########################################################################

                ### PREVISIONS REGRESSIONS PENALISEES ###

########################################################################

### Importation des libraries ###

library(readxl)
library(forecast)
library(ncvreg)
library(dplyr)
library(glmnet)
library(tidyverse)

########################################################################


########################################################################

### Importation de l'environnement ###

#setwd("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/")
setwd("/Users/emma/Desktop/DRiM/BDDs")
#setwd("/Users/este.pcl/Desktop/DrimGame 2022/Script")
#setwd("C:/Users/alexa/OneDrive/Bureau/M2_EKAP/drim_gameParis")

### Importation des bases de données avec et sans y retardé ###

tot_less_diff <- read_excel('tot_less_diff.xlsx')
chr2_less_diff <- read_excel('chr2_less_diff.xlsx')
chr8_less_diff <- read_excel('chr8_less_diff.xlsx')

tot_ret <- read_excel('Tot_ret.xlsx')
chr2_ret <- read_excel('chr2_ret.xlsx')
chr8_ret <- read_excel('chr8_ret.xlsx')

### transformation des bases en dataframe ###
dlbaseforecasttot <- data.frame(tot_less_diff)
dlbaseforecastchr2 <- data.frame(chr2_less_diff)
dlbaseforecastchr8 <- data.frame(chr8_less_diff)

tot_ret <- data.frame(tot_ret)
chr2_ret <- data.frame(chr2_ret)
chr8_ret <- data.frame(chr8_ret)

########################################################################


########################################################################


### TOT ###

### initialisation de la variable dépendante et les var indep ###
# variable dépendante
indepvar <- data.frame(dlbaseforecasttot) %>%
  select(DR) %>%
  as.matrix()
indepvar <-as.matrix(indepvar)
# variables indépendantes
depvar <- data.frame(dlbaseforecasttot) %>%
  select(-DR) %>%
  as.matrix()
depvar <-as.matrix(depvar)
#str(depvar)

### RIDGE  ###
predridge <- NULL

for (i in 1:8) {
  x <- data.matrix(depvar[1:(24+i),])	
  y <- data.matrix(indepvar[1:(24+i)])
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cv <- ridge_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)
  
  predridge <- c(predridge, predict(model_cv, newx = depvar[(25+i),]))
}
predridge
lambda_cv
model_cv$beta


### LASSO ###
predlasso=NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(24+i),])	
  y <- data.matrix(indepvar[1:(24+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvlasso <- lasso_cv$lambda.min
  model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cvlasso, standardize = T)
  predlasso <- c(predlasso, predict(model_cv, newx = depvar[(25+i),]))	# one-step ahead forecast 
}
predlasso
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### ELASTIC-NET ###
preden <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(24+i),])	
  y <- data.matrix(indepvar[1:(24+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  en_cv <- cv.glmnet(x, y, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cven <- en_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0.5, lambda = lambda_cven, standardize = T)
  preden <- c(preden, predict(model_cv, newx = depvar[(25+i),]))	# one-step ahead forecast 
}
preden
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### SCAD ###
predSCAD <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:25,])	
  y <- data.matrix(indepvar[1:25])
  cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"))
  lambda_SCAD <- cvfit_SCAD$lambda.min
  model_cv=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
  predSCAD <- c(predSCAD, predict(model_cv, depvar[(25+i),]))	# one-step ahead forecast 
}
predSCAD
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

########################################################################


########################################################################

### CHR2 ###
#variable dépendante
indepvar <- data.frame(dlbaseforecastchr2) %>%
  select(DR) %>%
  as.matrix()
#variables indépendantes
depvar <- data.frame(dlbaseforecastchr2) %>%
  select(-DR) %>%
  as.matrix()
#str(depvar)

### RIDGE  ###
predridge <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(24+i),])	
  y <- data.matrix(indepvar[1:(24+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvridge <- ridge_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cvridge, standardize = T)
  
  predridge <- c(predridge, predict(model_cv, newx = depvar[(25+i),]))	# one-step ahead forecast 
}
predridge
model_cv$beta

### LASSO ###
predlasso=NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(24+i),])	
  y <- data.matrix(indepvar[1:(24+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvlasso <- lasso_cv$lambda.min
  model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cvlasso, standardize = T)
  predlasso <- c(predlasso, predict(model_cv, newx = depvar[(25+i),]))	# one-step ahead forecast 
}
predlasso
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### ELASTIC-NET ###
preden <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(24+i),])	
  y <- data.matrix(indepvar[1:(24+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  en_cv <- cv.glmnet(x, y, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cven <- en_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0.5, lambda = lambda_cven, standardize = T)
  preden <- c(preden, predict(model_cv, newx = depvar[(25+i),]))	# one-step ahead forecast 
}
preden
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### SCAD ###
predSCAD <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:25,])	
  y <- data.matrix(indepvar[1:25])
  cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"))
  lambda_SCAD <- cvfit_SCAD$lambda.min
  model_cv=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
  predSCAD <- c(predSCAD, predict(model_cv, depvar[(25+i),]))	# one-step ahead forecast 
}
predSCAD
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)


########################################################################


########################################################################

### CHR8 ###
#variable dépendante
indepvar <- data.frame(dlbaseforecastchr8) %>%
  select(DR) %>%
  as.matrix()
#variables indépendantes
depvar <- data.frame(dlbaseforecastchr8) %>%
  select(-DR) %>%
  as.matrix()
#str(depvar)

### RIDGE  ###
predridge <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(24+i),])	
  y <- data.matrix(indepvar[1:(24+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvridge <- ridge_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cvridge, standardize = T)
  
  predridge <- c(predridge, predict(model_cv, newx = depvar[(25+i),]))	# one-step ahead forecast 
}
predridge
model_cv$beta


### LASSO ###
predlasso=NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(24+i),])	
  y <- data.matrix(indepvar[1:(24+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvlasso <- lasso_cv$lambda.min
  model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cvlasso, standardize = T)
  predlasso <- c(predlasso, predict(model_cv, newx = depvar[(25+i),]))	# one-step ahead forecast 
}
predlasso
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### ELASTIC-NET ###
preden <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(24+i),])	
  y <- data.matrix(indepvar[1:(24+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  en_cv <- cv.glmnet(x, y, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cven <- en_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0.5, lambda = lambda_cven, standardize = T)
  preden <- c(preden, predict(model_cv, newx = depvar[(25+i),]))	# one-step ahead forecast 
}
preden
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### SCAD ###
predSCAD <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:25,])	
  y <- data.matrix(indepvar[1:25])
  cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"))
  lambda_SCAD <- cvfit_SCAD$lambda.min
  model_cv=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
  predSCAD <- c(predSCAD, predict(model_cv, depvar[(25+i),]))	# one-step ahead forecast 
}
predSCAD
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)


########################################################################
########################################################################


########################################################################
########################################################################



########################################################################

### prévisions sur les bases avec y retardé ###

########################################################################

### TOT ###

### initialisation de la variable dépendante et les var indep ###
# variable dépendante
indepvar <- data.frame(tot_ret) %>%
  select(DR) %>%
  as.matrix()
# variables indépendantes
depvar <- data.frame(tot_ret) %>%
  select(-DR) %>%
  as.matrix()
#str(depvar)

### RIDGE  ###
predridge_ret <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(23+i),])	
  y <- data.matrix(indepvar[1:(23+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvridge <- ridge_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cvridge, standardize = T)
  
  predridge_ret <- c(predridge_ret, predict(model_cv, newx = depvar[(24+i),]))	# one-step ahead forecast 
}
predridge_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### LASSO ###
predlasso_ret=NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(23+i),])	
  y <- data.matrix(indepvar[1:(23+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvlasso <- lasso_cv$lambda.min
  model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cvlasso, standardize = T)
  predlasso_ret <- c(predlasso_ret, predict(model_cv, newx = depvar[(24+i),]))	# one-step ahead forecast 
}
predlasso_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### ELASTIC-NET ###
preden_ret <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(23+i),])	
  y <- data.matrix(indepvar[1:(23+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  en_cv <- cv.glmnet(x, y, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cven <- en_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0.5, lambda = lambda_cven, standardize = T)
  preden_ret <- c(preden_ret, predict(model_cv, newx = depvar[(24+i),]))	# one-step ahead forecast 
}
preden_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### SCAD ###
predSCAD_ret <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:23,])	
  y <- data.matrix(indepvar[1:23])
  cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"))
  lambda_SCAD <- cvfit_SCAD$lambda.min
  model_cv=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
  predSCAD_ret <- c(predSCAD_ret, predict(model_cv, depvar[(24+i),]))	# one-step ahead forecast 
}
predSCAD_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

########################################################################


########################################################################

### CHR2 ###

#variable dépendante
indepvar <- data.frame(chr2_ret) %>%
  select(DR) %>%
  as.matrix()
#variables indépendantes
depvar <- data.frame(chr2_ret) %>%
  select(-DR) %>%
  as.matrix()
#str(depvar)

### RIDGE  ###
predridge_ret <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(23+i),])	
  y <- data.matrix(indepvar[1:(23+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvridge <- ridge_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cvridge, standardize = T)
  
  predridge_ret <- c(predridge_ret, predict(model_cv, newx = depvar[(24+i),]))	# one-step ahead forecast 
}
predridge_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### LASSO ###
predlasso_ret=NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(23+i),])	
  y <- data.matrix(indepvar[1:(23+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvlasso <- lasso_cv$lambda.min
  model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cvlasso, standardize = T)
  predlasso_ret <- c(predlasso_ret, predict(model_cv, newx = depvar[(24+i),]))	# one-step ahead forecast 
}
predlasso_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### ELASTIC-NET ###
preden_ret <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(23+i),])	
  y <- data.matrix(indepvar[1:(23+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  en_cv <- cv.glmnet(x, y, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cven <- en_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0.5, lambda = lambda_cven, standardize = T)
  preden_ret <- c(preden_ret, predict(model_cv, newx = depvar[(24+i),]))	# one-step ahead forecast 
}
preden_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### SCAD ###
predSCAD_ret <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:23,])	
  y <- data.matrix(indepvar[1:23])
  cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"))
  lambda_SCAD <- cvfit_SCAD$lambda.min
  model_cv=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
  predSCAD_ret <- c(predSCAD_ret, predict(model_cv, depvar[(24+i),]))	# one-step ahead forecast 
}
predSCAD_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

########################################################################


########################################################################

### CHR8 ###
#variable dépendante
indepvar <- data.frame(chr8_ret) %>%
  select(DR) %>%
  as.matrix()
#variables indépendantes
depvar <- data.frame(chr8_ret) %>%
  select(-DR) %>%
  as.matrix()
#str(depvar)

### RIDGE  ###
predridge_ret <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(23+i),])	
  y <- data.matrix(indepvar[1:(23+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvridge <- ridge_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cvridge, standardize = T)
  
  predridge_ret <- c(predridge_ret, predict(model_cv, newx = depvar[(24+i),]))	# one-step ahead forecast 
}
predridge_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### LASSO ###
predlasso_ret=NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(23+i),])	
  y <- data.matrix(indepvar[1:(23+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cvlasso <- lasso_cv$lambda.min
  model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cvlasso, standardize = T)
  predlasso_ret <- c(predlasso_ret, predict(model_cv, newx = depvar[(24+i),]))	# one-step ahead forecast 
}
predlasso_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### ELASTIC-NET ###
preden_ret <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:(23+i),])	
  y <- data.matrix(indepvar[1:(23+i)])
  
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  en_cv <- cv.glmnet(x, y, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
  lambda_cven <- en_cv$lambda.1se
  model_cv <- glmnet(x, y, alpha = 0.5, lambda = lambda_cven, standardize = T)
  preden_ret <- c(preden_ret, predict(model_cv, newx = depvar[(24+i),]))	# one-step ahead forecast 
}
preden_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)

### SCAD ###
predSCAD_ret <- NULL
for (i in 1:8) {
  x <- data.matrix(depvar[1:23,])	
  y <- data.matrix(indepvar[1:23])
  cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"))
  lambda_SCAD <- cvfit_SCAD$lambda.min
  model_cv=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
  predSCAD_ret <- c(predSCAD_ret, predict(model_cv, depvar[(24+i),]))	# one-step ahead forecast 
}
predSCAD_ret
model_cv$beta
which(! coef(model_cv) == 0, arr.ind = TRUE)
