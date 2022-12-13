########################################################################
#######################      DRIM GAME 2022      ######################

                        ### PREVISION ARIMA ###

########################################################################

### Importation des librairies ###
library(tseries)
library(forecast) #pour auto.arima
library(readxl)

########################################################################


########################################################################

### Importation de l'environnement ###

#setwd("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/")
setwd("/Users/este.pcl/Desktop/DrimGame/PARIS/Scripts/prev_naive_ARIMA_mco/previsions arima")
#setwd("C:/Users/alexa/OneDrive/Bureau/M2_EKAP/drim_game/BDDs")
#setwd("/Users/este.pcl/Desktop/DrimGame 2022/Script")

### Importation des bdd ###
chr2_less_diff <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/chr2_less_diff.xlsx")
chr8_less_diff <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/chr8_less_diff.xlsx")
tot_less_diff <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/tot_less_diff.xlsx")

########################################################################


########################################################################

#########################       AUTO-ARIMA       ######################

### CHR2 ###
yarima2 <-chr2_less_diff$DR[1:25]
yarima2 <-ts(yarima2, start=c(2009,1),frequency=4)
yarima2 <- as.data.frame(yarima2)
out1=auto.arima(yarima2)
out1
#AUTO ARIMA NE TROUVE PAS DE MODELE POUR CHR2

### CHR8 ###
yarima8 <-chr8_less_diff$DR[1:25]
yarima8 <-ts(yarima8, start=c(2009,1),frequency=4)
yarima8 <- as.data.frame(yarima8)
out2=auto.arima(yarima8)
out2
#AUTO ARIMA NE TROUVE PAS DE MODELE POUR CHR8

### TOT ###
yarimat <-tot_less_diff$DR[1:25]
yarimat <-ts(yarimat, start=c(2009,1),frequency=4)
yarimat <- as.data.frame(yarimat)
out3=auto.arima(yarimat)
out3
#MODELE AR(2) RETENU POUR TOT

########################################################################


########################################################################

#########################     CORRELOGRAMMES     #######################

par(mfrow=c(1,2))
### CHR2 ###
result_acf=acf(yarima2)
pacf = pacf(yarima2)
#Modele AR(1) retenu

### CHR8 ###
result_acf=acf(yarima8)
pacf = pacf(yarima8)
#Modele AR(1) retenu
#Modele AR(2) retenu

### TOTALE ###
result_acf=acf(yarimat)
pacf = pacf(yarimat)

########################################################################


########################################################################

#########################          ARIMA         ######################

### CHR2 ###
DR <- ts(yarima2, start=c(2009,01),frequency=4)

arima1 <- arima(DR, order = c(1,0,0))
arima1

pred2 = predict(arima1, n.ahead = 8)
pred2$pred
write(t(pred2$pred),file="forecast_arima2.txt",ncolumn=1,append=FALSE)

### CHR 8 ###
DR <- ts(yarima8, start=c(2009,01),frequency=4)

arima1 <- arima(DR, order = c(1,0,0))
arima2 <- arima(DR, order = c(2,0,0))
arima1 #Modele AR(1) retenu
arima2

pred8 = predict(arima1, n.ahead = 8)
pred8$pred
write(t(pred8$pred),file="forecast_arima8.txt",ncolumn=1,append=FALSE)

### TOT ###
DR <- ts(yarimat, start=c(2009,01),frequency=4)

arima2 <- arima(DR, order = c(2,0,0))
arima2

predt = predict(arima2, n.ahead = 8)
predt$pred
write(t(predt$pred),file="forecast_arimat.txt",ncolumn=1,append=FALSE)
