########################################################################
#######################      DRIM GAME 2022      ######################

########################################################################

### Importation des libraries ###
library(readxl)
library(tsoutliers)
library(tseries)
library(stats)
library(scales)
library(dplyr)
library(ggplot2)
library(corrr)
library(openxlsx)
library(seastests)
library(seasonal)

### Importation de l'environnement ###
#setwd("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/")
#setwd("/Users/emma/Desktop/BDDs")
setwd("C:/Users/alexa/OneDrive/Bureau/M2_EKAP/drim_game/Paris/finale")
#setwd("/Users/este.pcl/Desktop/DrimGame 2022/Script")

### Importation des bases de donnees ###
#pour alex

tot <- read_excel("Tot_avec_retard.xlsx")

str(tot[,145:166])

chr2[,100:166] <- sapply(chr2[,100:166],as.numeric)
chr8[,100:166]<- sapply(chr8[,100:166],as.numeric)
tot[,145:166] <- sapply(tot[,145:166],as.numeric)

base_chr2 <- chr2[,4:92]
base_chr8<- chr8[,4:92]
base_tot <- tot[,145:166]

str(base_tot)


########################################################################


########################################################################

####################            OUTLIERS           ####################


### BASE TOT ###

### Passage des va en series temporelles ###
new_base <- ts(base_tot[,-c(3,14)], start=c(2009,01),frequency=4)
#52 marche mais peu d'obs

### Boucle pour enlever les obs atypiques ###
i=1
matrice <- matrix(0:0, nrow=nrow(new_base), ncol=ncol(new_base))
for ( i in 1:ncol(new_base)) {
  fit <- tso(new_base[,i])
  matrice[,i] <- fit$yadj
}
matrice_tot <- as.data.frame(matrice)

### Changement du nom des var ###
colnames(matrice_tot)<-c(colnames(base_tot[,-c(3,14)]))


########################################################################


########################################################################

####################           SAISONALITE          ####################



### TOT ###

matrice_tot_sai <- ts(matrice_tot, start=c(2009,04),frequency=4)

n <- ncol(matrice_tot_sai)
tot_sai = matrix(0,32,20)
count = 0
nom_col = names(matrice_tot_sai)
for (j in 1:n){
  result_test <- combined_test(matrice_tot_sai[,j])
  if (result_test$Pval<0.05){
    seasX <- seas(matrice_tot_sai[,j])
    tot_sai[,j] <- final(seasX)
    count = count + 1
    cat("Variable desaisonnalisee : ",j, "\n")
  }else{
    tot_sai[,j] <- matrice_tot_sai[,j]
  }
}
cat("Nombre de variables desaisonnalisee : ", count, "/", n) #1 var sur 80 differenciees

colnames(tot_sai) = colnames(matrice_tot_sai)

########################################################################


########################################################################

###     STATIONNARISER : BOUCLE MOINS RESTRICTIVE, ADF ET KPSS      ###

### tot ###
new_base <- ts(chr2_sai, start=c(2009,01),frequency=4)
#adf.test(new_base[,81])

### Boucle pour stationariser ###
n <- ncol(new_base)
chr2_less_diff = matrix(0,31,20)
count = 0
nom_col = names(new_base)
for (j in 1:n){
  result_test <- adf.test(new_base[,j])
  result_test2 <- kpss.test(new_base[,j])
  if (result_test$p.value>0.05 & result_test2$p.value<0.05){
    chr2_less_diff[,j] <- diff(new_base[,j], differences = 1)
    count = count + 1
    cat("Variable stationnarisee : ",j, "\n")
  }else{
    chr2_less_diff[,j] <- new_base[-1,j]
  }
}
cat("Nombre de variables stationnarisees : ", count, "/", n) #5

tot_retard_diff = chr2_less_diff
colnames(tot_retard_diff) = colnames(tot_sai)
tot_retard_diff <- as.data.frame(tot_retard_diff)
str(tot_retard_diff)
View(tot_retard_diff)


write.xlsx(tot_retard_diff, "tot_retard_diff.xlsx", sheetName = "tot_retard_diff",
           colNames = TRUE, rowNames = TRUE, append = FALSE)

