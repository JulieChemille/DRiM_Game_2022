########################################################################
#######################      DRIM GAME 2022      #######################

                        ### PREVISION NAIVE ###

########################################################################

### Importation des libraries ###
library(readxl)
library(forecast)

########################################################################


########################################################################

### Importation de l'environnement ###

#setwd("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/")
#setwd("/Users/emma/Desktop/DRiM/BDDs")
setwd("/Users/este.pcl/Desktop/DrimGame/PARIS/Scripts/prev_naive_ARIMA_mco/previsions naive")
#setwd("C:/Users/alexa/OneDrive/Bureau/M2_EKAP/drim_gameParis")

### Importation de la base de donnees ###
tot_less_diff <- read_excel('/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/tot_less_diff.xlsx')
chr2_less_diff <- read_excel('/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/chr2_less_diff.xlsx')
chr8_less_diff <- read_excel('/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/chr8_less_diff.xlsx')

########################################################################


########################################################################

############################  Prevision naive ######################### 

### TOTALE ###
ynaivet <-tot_less_diff$DR[1:25]
ynaivet <-ts(ynaivet, start=c(2009,1),frequency=4)
ynaivet
nait <-naive(ynaivet,h=8) 
nait
quartz()
plot(nait)
nait=as.numeric(nait$mean)
nait

### CHR2 ###
ynaive2 <-chr2_less_diff$DR[1:25]
ynaive2 <-ts(ynaive2, start=c(2009,1),frequency=4)
ynaive2
nai2 <-naive(ynaive2,h=8) 
nai2
quartz()
plot(nai2)
nai2=as.numeric(nai2$mean)
nai2
write(t(nai2),file="forecast_nai2.txt",ncolumn=1,append=FALSE)

### CHR8 ###
ynaive8 <-chr8_less_diff$DR[1:25]
ynaive8 <-ts(ynaive8, start=c(2009,1),frequency=4)
ynaive8
nai8 <-naive(ynaive8,h=8) 
nai8
quartz()
plot(nai8)
nai8=as.numeric(nai8$mean)
nai8
write(t(nai8),file="forecast_nai8.txt",ncolumn=1,append=FALSE)

