########################################################################
########################################################################

### Importation des librairies ###
library(seasonal)
library(forecast) 
library(xlsx)

### Importation de l'environnement ###
#setwd("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/")
setwd("/Users/emma/Desktop/DRiM/BDDs")
#setwd("C:/Users/alexa/OneDrive/Bureau/M2_EKAP/drim_game/BDDs")
#setwd("/Users/este.pcl/Desktop/DrimGame 2022/Script")


########################################################################


########################################################################

### Importation des bases de donnees ###
tot_less_diff <-read_excel('tot_less_diff.xlsx')
chr8_ret <-read_excel('chr8_ret.xlsx')
chr2_ret <-read_excel('chr2_ret.xlsx')

########################################################################
#### base tot_less_diff
########################################################################
### prévisions des xi sur 3 ans avec auto.arima ###
x <- ts(data = tot_less_diff, start=c(2009,01),frequency=4)
y <- tot_less_diff[1:12,]
i=1
matricetot <- matrix(0:0, nrow=nrow(y), ncol=ncol(tot_less_diff))
for ( i in 1:72) {
  out <- auto.arima(x[,i], d = 0, D=0)
  fc <- forecast(x[,i],model=out,h=12)
  fc <- fc$mean
  fc = as.numeric(fc)
  matricetot[,i] <- fc
  colnames(matricetot) = colnames (tot_less_diff)
}
matricetot <- as.data.frame(matricetot)
View(matricetot)

### prévisions naives des xi où oas de modèle arima trouvé  ###
j <- 1

for (j in 1:72) {
  if (matricetot[1,j] == 0 ) matricetot[1:9,j] <- tot_less_diff[33,j]
}


########################################################################
#### CHR2_RET
########################################################################

### prévisions des xi sur 3 ans avec auto.arima ###
x <- ts(data = chr2_ret, start=c(2009,02),frequency=4)
y <- chr2_ret[1:12,]
i=1
matrice2_ret <- matrix(0:0, nrow=nrow(y), ncol=ncol(chr2_ret))
for ( i in 1:182) {
  out <- auto.arima(x[,i], d = 0, D=0)
  fc <- forecast(x[,i],model=out,h=12)
  fc <- fc$mean
  fc = as.numeric(fc)
  matrice2_ret[,i] <- fc
  colnames(matrice2_ret) = colnames (chr2_ret)
}
matrice2_ret <- as.data.frame(matrice2_ret)
View(matrice2_ret)

### prévisions naives des xi où oas de modèle arima trouvé  ###
j <- 1

for (j in 1:182) {
  if (matrice2_ret[1,j] == 0 ) matrice2_ret[1:9,j] <- chr2_ret[32,j]
}

View(matrice2_ret)
matrice2_ret <- matrice2_ret[,-c(1)]


########################################################################
#### CHR8_RET
########################################################################

### prévisions des xi sur 3 ans avec auto.arima ###
x <- ts(data = chr8_ret, start=c(2009,02),frequency=4)
y <- chr8_ret[1:12,]
i=1
matrice8_ret <- matrix(0:0, nrow=nrow(y), ncol=ncol(chr8_ret))
for ( i in 1:190) {
  out <- auto.arima(x[,i], d = 0, D=0)
  fc <- forecast(x[,i],model=out,h=12)
  fc <- fc$mean
  fc = as.numeric(fc)
  matrice8_ret[,i] <- fc
  colnames(matrice8_ret) = colnames (chr8_ret)
}
matrice8_ret <- as.data.frame(matrice8_ret)
View(matrice8_ret)

### prévisions naives des xi où oas de modèle arima trouvé  ###
j <- 1

for (j in 1:190) {
  if (matrice8_ret[1,j] == 0 ) matrice8_ret[1:9,j] <- chr8_ret[32,j]
}

View(matrice8_ret)
matrice8_ret <- matrice8_ret[,-c(1)]


### exportation des matrices
write.csv(matrice2_ret, 'matrice2_ret.csv')
write.csv2(matrice8_ret, 'matrice8_ret.csv')
write.csv2(matricetot, 'matricetot.csv')



