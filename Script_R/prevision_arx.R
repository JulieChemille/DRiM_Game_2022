########################################################################
#######################      DRIM GAME 2022      ######################

########################################################################

### Importation des librairies ###
library(readxl)
library(gets)
library(seasonal)
library(forecast)

########################################################################


########################################################################

### Importation de l'environnement ###

#setwd("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/")
setwd("/Users/emma/Desktop/DRiM/BDDs")
#setwd("C:/Users/alexa/OneDrive/Bureau/M2_EKAP/drim_game/BDDs")
#setwd("/Users/este.pcl/Desktop/DrimGame 2022/Script")

### Importation de la base de donnees ###

tot_less_diff <- read_excel('tot_less_diff.xlsx')
chr2_less_diff <- read_excel('chr2_less_diff.xlsx')
chr8_less_diff <- read_excel('chr8_less_diff.xlsx')

tot_less_diffarx <- read_excel('tot_less_diff.xlsx')
chr2_less_diffarx <- read_excel('chr2_less_diff.xlsx')
chr8_less_diffarx <- read_excel('chr8_less_diff.xlsx')

tot_less_diffarx <- tot_less_diffarx[1:25,]
chr2_less_diffarx <- chr2_less_diffarx[1:25,]
chr8_less_diffarx <- chr8_less_diffarx[1:25,]
########################################################################

######################### TOT LESS DIFF ################################

########################################################################

str(xx)

### variables retenues après le stepwize ###
xx <- tot_less_diffarx[,c( "p25_7","median_3","p25_8", "p10_2" , "p10_3","tx_retournement","p90_8","p90_5","p75_7", "CD_PROF_3" , 
  "p25_2","moral_fr","tx_cho", "change_in_stock","p25_1","p90_3","p95_2")]
## transformation en matrice pour que le modele fonctionne
class(xx[,1:17]) # tibble
mX = data.matrix(xx)
str(tot_less_diffarx)
y <- tot_less_diffarx$DR

########################################################################

### ARX SANS AR(1)###
model <- arx(y, mc = TRUE, ar = NULL, mxreg = mX, vcov.type = "ordinary")
model
getsm <- getsm(model) 
getsm
# la procédure gets ne fonctionne pas

#### retrait au fur-et à mesure des var non significatives ####
#CD_PROF_3, moral_fr, tx_cho, p25_1, p25_2, p25_2, p75_7, change_in_stock
#p90_3, p90_5
xx <- tot_less_diffarx[,c("p25_7","median_3","p25_8", "p10_2" , "p10_3","tx_retournement",
                       "p90_8")]

mX = data.matrix(xx)
model <- arx(y, mc = TRUE, ar = NULL, mxreg = mX, vcov.type = "ordinary")
model
AIC(model)
#modèle retenu pour la prévision

### prévision horizon h=8 ###
training_dlbase <- tot_less_diff[1:25,]
testingbase <- tot_less_diff[26:33, 1]
dependvar <- tot_less_diff[,c("p25_7","median_3","p25_8", "p10_2" , "p10_3","tx_retournement",
                              "p90_8")]
indepvar <- tot_less_diff$DR
dependvar2 <- data.frame(dependvar)
mX <- data.matrix(dependvar2[1:25,])
y <- indepvar[1:25]
model <- arx(y, mc = T, ar = NULL, mxreg = mX, vcov.type = "ordinary")
forecast <- predict(model, n.ahead = 8, newmxreg = data.matrix(dependvar2[(26:33),]))
forecast


########################################################################
### ARX AVEC AR(1)###
#variables gardées avec stepwise
y <- tot_less_diffarx$DR
xx <- tot_less_diffarx[,c( "p25_7","median_3","p25_8", "p10_2" , "p10_3","tx_retournement","p90_8","p90_5","p75_7", "CD_PROF_3" , 
                        "p25_2","moral_fr","tx_cho", "change_in_stock","p25_1","p90_3","p95_2")]
class(xx[,1:17]) # tibble
mX = data.matrix(xx)
### modèle ###
Model01 <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary") 
Model01
getsm1 <- getsm(Model01) 
getsm1
# la procédure gets ne fonctionne pas

#### retrait au fur-et à mesure des var non significatives ####
#tx_cho, moral_fr, CD_PROF_3, 
xx <- tot_less_diff[,c("p25_7","median_3","p25_8", "p10_2" , "p10_3","tx_retournement",
                          "p90_8","p90_5","p75_7", 
                       "p25_2","change_in_stock","p25_1","p90_3","p95_2")]
mX = data.matrix(xx)

Model01 <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary") 
Model01
AIC(Model01)
### prévision horizon h=8 ###
training_dlbase <- tot_less_diff[1:25,]
testingbase <- tot_less_diff[26:33, 1]
dependvar <- tot_less_diff[,c("p25_7","median_3","p25_8", "p10_2" , "p10_3","tx_retournement",
"p90_8","p90_5","p75_7",
"p25_2","change_in_stock","p25_1","p90_3","p95_2")]
indepvar <- tot_less_diff$DR
dependvar2 <- data.frame(dependvar)
mX <- data.matrix(dependvar2[1:25,])
y <- indepvar[1:25]
model <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary")
forecast <- predict(model, n.ahead = 8, newmxreg = data.matrix(dependvar2[(26:33),]))
forecast


## PENSER A RAJOUTER LA TENDANCE DANS LA PREVISION


########################################################################


########################################################################


########################## CHR2 LESS DIFF ##############################

########################################################################

### variables retenues après le stepwize ###
xx <- chr2_less_diffarx[,c("p95_4","mean_5","p95_7","p25_3", "mean_1","p90_4","p10_2","p25_7","mean_4","moral_fr","p5_7",
"p90_5", "p25_5","p5_1","p75_2", "CD_MOD_HABI_1","median_7", "PIB", "p10_3")]
# convert tibble in matrix for the function arx
class(xx[,1:19]) # tibble
mX = data.matrix(xx)

### Variable dépendante
y <- chr2_less_diffarx$DR

########################################################################

### ARX SANS AR(1) ###
model <- arx(y, mc = TRUE, ar = NULL,
             mxreg = mX, vcov.type = "ordinary")
model
getsm <- getsm(model) 

#### retrait au fur-et à mesure des var non significatives ####
#p25_3, PIB, p95_4, p10_3, p25_7, p5_1, p90_4, p95_7, p75_2
#moral_fr, CD_MOD_HABI_1, mean_1, mean_5, median_7, p5_7
xx <- chr2_less_diffarx[,c( "p10_2","mean_4",
                        "p90_5", "p25_5")]

# convert tibble in matrix for the function arx
class(xx[,1:4]) # tibble
mX = data.matrix(xx)
str(mX)
model <- arx(y, mc = TRUE, ar = NULL, mxreg = mX, vcov.type = "ordinary")
model
AIC(model)
#modèle retenu pour la prévision

### prévision horizon h=8 ###
training_dlbase <- chr2_less_diff[1:25,]
testingbase <- chr2_less_diff[26:33, 1]
dependvar <- chr2_less_diff[,c( "p10_2","mean_4",
                                "p90_5", "p25_5")]
indepvar <- chr2_less_diff$DR
dependvar2 <- data.frame(dependvar)
mX <- data.matrix(dependvar2[1:25,])
y <- indepvar[1:25]
model <- arx(y, mc = T, ar = NULL, mxreg = mX, vcov.type = "ordinary")
forecast <- predict(model, n.ahead = 8, newmxreg = data.matrix(dependvar2[(26:33),]))
forecast

########################################################################

### ARX avec AR(1) ###
xx <- chr2_less_diffarx[,c("p95_4","mean_5","p95_7","p25_3", "mean_1","p90_4","p10_2","p25_7","mean_4","moral_fr","p5_7",
                           "p90_5", "p25_5","p5_1","p75_2", "CD_MOD_HABI_1","median_7", "PIB", "p10_3")]
class(xx[,1:19]) # tibble
mX = data.matrix(xx)
Model01 <- arx(chr2_less_diffarx$DR, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary") 
Model01
getsm1 <- getsm(Model01) 
getsm1
# la procédure gets ne fonctionne pas

#### retrait au fur-et à mesure des var non significatives ####
#p10_2,median_7, mean_5, p95_4, p90_5, p10_3, p25_7, p25_3
#mean_1, PIB, p5_7, p5_1, p25_5, p75_2, moral_fr, p90_4
#p95_7, "CD_MOD_HABI_1"
xx <- chr2_less_diffarx[,c("mean_4")]
class(xx[,1]) # tibble
mX = data.matrix(xx)

Model01 <- arx(chr2_less_diffarx$DR, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary") 
Model01
AIC(Model01)
#modèle retenu pour la prévision

### prévision horizon h=8 ###
#prépa
training_dlbase <- chr2_less_diff[1:25,]
testingbase <- chr2_less_diff[26:33, 1]
dependvar <- chr2_less_diff[,c("mean_4")]
indepvar <- chr2_less_diff$DR
dependvar2 <- data.frame(dependvar)
mX <- data.matrix(dependvar2[1:25,])
y <- indepvar[1:25]
model <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary")
forecast <- predict(model, n.ahead = 8, newmxreg = data.matrix(dependvar2[(26:33),]))
forecast

########################################################################


########################################################################


########################################################################

########################## CHR8 LESS DIFF ##############################

########################################################################

### variables retenues après le stepwize ###
xx <- chr8_less_diffarx[,c("CD_PROF_2","mean_7","p25_6" , "CD_TY_CLI_RCI_2","CD_ETA_CIV_1","p25_1",
                        "p5_8",  "p95_5","CD_PROF_3","change_in_stock","PIB","median_3","p5_6","tx_endet",
                        "p90_2" , "p5_1" , "GGTrend" , "p25_4")]
str(xx)
# convert tibble in matrix for the function arx
class(xx[,1:18]) # tibble
mX = data.matrix(xx)
str(mX)
### Variable dépendante
y <- chr8_less_diffarx$DR


########################################################################

### ARX SANS AR(1) ###
model <- arx(y, mc = TRUE, ar = NULL, mxreg = mX, vcov.type = "ordinary")
model
getsm <- getsm(model) 
#getsm
# la procédure gets ne fonctionne pas

#### retrait au fur-et à mesure des var non significatives ####
#p25_1, p5_8, median_3, p5_1, p5_6, p25_4, p95_5, change_in_stock
#CD_PROF_3, GGTrend, p90_2
xx <- chr8_less_diffarx[,c("CD_PROF_2","mean_7","p25_6" , "CD_TY_CLI_RCI_2","CD_ETA_CIV_1",
                           "PIB","tx_endet")]

class(xx[,1:18]) # tibble
mX = data.matrix(xx)
str(mX)
model <- arx(y, mc = TRUE, ar = NULL, mxreg = mX, vcov.type = "ordinary")
model
AIC(model)
#modèle retenu pour la prévision

### prévision horizon h=8 ###
training_dlbase <- chr8_less_diff[1:25,]
testingbase <- chr8_less_diff[26:33, 1]
dependvar <- chr8_less_diff[,c( "CD_PROF_2","mean_7","p25_6" , "CD_TY_CLI_RCI_2","CD_ETA_CIV_1",
                                "PIB","tx_endet")]
indepvar <- chr8_less_diff$DR
dependvar2 <- data.frame(dependvar)
mX <- data.matrix(dependvar2[1:25,])
y <- indepvar[1:25]
model <- arx(y, mc = T, ar = NULL, mxreg = mX, vcov.type = "ordinary")
forecast <- predict(model, n.ahead = 8, newmxreg = data.matrix(dependvar2[(26:33),]))
forecast

########################################################################

### ARX avec AR(1) ###
xx <- chr8_less_diffarx[,c("CD_PROF_2","mean_7","p25_6" , "CD_TY_CLI_RCI_2","CD_ETA_CIV_1","p25_1",
                           "p5_8",  "p95_5","CD_PROF_3","change_in_stock","PIB","median_3","p5_6","tx_endet",
                           "p90_2" , "p5_1" , "GGTrend" , "p25_4")]
class(xx[,1:18]) # tibble
mX = data.matrix(xx)
Model01 <- arx(chr8_less_diffarx$DR, mc = T, ar = 1, mxreg = mX, vcov.type = "ordinary") 
Model01
getsm1 <- getsm(Model01) 
getsm1
# la procédure gets ne fonctionne pas

### le retard n'est pas significatif pour le modèle. Pas de prévision possible sur ce modèle