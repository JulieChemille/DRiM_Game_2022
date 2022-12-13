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
chr2 <- read_excel("BDD_X_chr2.xlsx")
chr8 <- read_excel("BDD_X_chr8.xlsx")
tot <- read_excel("BDD_X_totale.xlsx")

#pour emma
# chr2 <- read_excel("/Users/emma/Desktop/M2/S1/drim/BDDs/BDD_X_chr2.xlsx")
# chr8 <- read_excel("/Users/emma/Desktop/M2/S1/drim/BDDs/BDD_X_chr8.xlsx")
# tot <- read_excel("/Users/emma/Desktop/M2/S1/drim/BDDs/BDD_X_totale.xlsx")

#pour julie
#chr2 <- read_excel("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/BDD_X_chr2.xlsx")
#chr8 <- read_excel("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/BDD_X_chr8.xlsx")
#tot <- read_excel("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/BDD_X_totale.xlsx")

#pour este
#chr2 <- read_excel("/Users/este.pcl/Desktop/DrimGame/Donnees/BDD_X_chr2.xlsx")
#chr8 <- read_excel("/Users/este.pcl/Desktop/DrimGame/Donnees/BDD_X_chr8.xlsx")
#tot <- read_excel("/Users/este.pcl/Desktop/DrimGame/Donnees/BDD_X_totale.xlsx")

str(chr2)
str(chr8)
str(tot)

chr2[,70:89] <- sapply(chr2[,70:89],as.numeric)
chr8[,70:89]<- sapply(chr8[,70:89],as.numeric)
tot[,70:89] <- sapply(tot[,70:89],as.numeric)

str(chr2)
str(chr8)
str(tot)

base_chr2 <- chr2[,3:92]
base_chr8<- chr8[,3:92]
base_tot <- tot[,3:92]

#remplacer les valeurs negatives par la moyenne pour les variables concernees 
for ( i in 1:34) {
  if (base_chr2[i,28] < 0) base_chr2[i,28] <- mean(base_chr2$p5_4)
}

for ( j in 1:34) {
  if (base_chr2[j,29] < 0) base_chr2[j,29] <- mean(base_chr2$p10_4)
}

########################################################################


########################################################################

####################            OUTLIERS           ####################

### CHR2 ###

### Passage des va en series temporelles ###
new_base <- ts(base_chr2[,-c(31,43,44,45,46,47,48,49,60,79)], start=c(2008,04),frequency=4)
str(new_base)

### Boucle pour enlever les obs atypiques ###
i=1
matrice <- matrix(0:0, nrow=nrow(new_base), ncol=ncol(new_base))
for ( i in 1:ncol(new_base)) {
  fit <- tso(new_base[,i])
  matrice[,i] <- fit$yadj
}

matrice_chr2 <- as.data.frame(matrice)

### Changement du nom des va ###
colnames(matrice_chr2)<-c(colnames(base_chr2[,-c(31,43,44,45,46,47,48,49,60,79)]))


### CHR8 ###
### Passage des va en series temporelles ###
new_base <- ts(base_chr8[,-c(10,28,29,31,59,79)], start=c(2008,04),frequency=4)

### Boucle pour enlever les obs atypiques ###
i=1
matrice <- matrix(0:0, nrow=nrow(new_base), ncol=ncol(new_base))
for ( i in 1:ncol(new_base)) {
  fit <- tso(new_base[,i])
  matrice[,i] <- fit$yadj
}
matrice_chr8 <- as.data.frame(matrice)

### Changement du nom des va ###
colnames(matrice_chr8)<-c(colnames(base_chr8[,-c(10,28,29,31,59,79)]))
#p10-8 = 0 


### BASE TOT ###

### Passage des va en series temporelles ###
new_base <- ts(base_tot[,-c(28,29,30,31,32,42,43,44,45,46,47,49,52,53,56,70,71,72,79)], start=c(2008,04),frequency=4)
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
colnames(matrice_tot)<-c(colnames(base_tot[,-c(28,29,30,31,32,42,43,44,45,46,47,49,52,53,56,70,71,72,79)]))

### Ajout le txcho a la main dans les bdd ###
tx_cho <- base_chr2$Tx_cho
matrice_chr2 <- cbind(matrice_chr2, tx_cho)
matrice_chr8 <- cbind(matrice_chr8, tx_cho)
matrice_tot <- cbind(matrice_tot,tx_cho)
str(matrice_chr2) #81 variables
str(matrice_chr8) #85 variables
str(matrice_tot) #72 variables

########################################################################


########################################################################

####################           SAISONALITE          ####################

### CHR2 ###
matrice_chr2_sai <- ts(matrice_chr2[,-c(8)], start=c(2008,04),frequency=4)

n <- ncol(matrice_chr2_sai)
chr2_sai = matrix(0,34,80)
count = 0
nom_col = names(matrice_chr2_sai)
for (j in 1:n){
  result_test <- combined_test(matrice_chr2_sai[,j])
  if (result_test$Pval<0.05){
    seasX <- seas(matrice_chr2_sai[,j])
    chr2_sai[,j] <- final(seasX)
    count = count + 1
    cat("Variable desaisonnalisee : ",j, "\n")
  }else{
    chr2_sai[,j] <- matrice_chr2_sai[,j]
  }
}
cat("Nombre de variables desaisonnalisee : ", count, "/", n) # 8 var sur 80 differenciees

colnames(chr2_sai) = colnames(matrice_chr2_sai)


### CHR 8 ###

matrice_chr8_sai <- ts(matrice_chr8[,-c(56)], start=c(2008,04),frequency=4)

n <- ncol(matrice_chr8_sai)
chr8_sai = matrix(0,34,84)
count = 0
nom_col = names(matrice_chr8_sai)
for (j in 1:n){
  result_test <- combined_test(matrice_chr8_sai[,j])
  if (result_test$Pval<0.05){
    seasX <- seas(matrice_chr8_sai[,j])
    chr8_sai[,j] <- final(seasX)
    count = count + 1
    cat("Variable desaisonnalisee : ",j, "\n")
  }else{
    chr8_sai[,j] <- matrice_chr8_sai[,j]
  }
}
cat("Nombre de variables desaisonnalisee : ", count, "/", n) #6 var sur 80 differenciees

colnames(chr8_sai) = colnames(matrice_chr8_sai)


### TOT ###

matrice_tot_sai <- ts(matrice_tot, start=c(2008,04),frequency=4)

n <- ncol(matrice_tot_sai)
tot_sai = matrix(0,34,72)
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
cat("Nombre de variables desaisonnalisee : ", count, "/", n) #17 var sur 80 differenciees

colnames(tot_sai) = colnames(matrice_tot_sai)

########################################################################


########################################################################

###     STATIONNARISER : BOUCLE MOINS RESTRICTIVE, ADF ET KPSS      ###

### CHR2 ###
new_base <- ts(chr2_sai, start=c(2008,04),frequency=4)
#adf.test(new_base[,81])
j =1
i=1

### Boucle pour stationariser ###
n <- ncol(new_base)
chr2_less_diff = matrix(0,33,80)
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
cat("Nombre de variables stationnarisees : ", count, "/", n) #20

colnames(chr2_less_diff) = colnames(chr2_sai)
chr2_less_diff <- as.data.frame(chr2_less_diff)
str(chr2_less_diff)
View(chr2_less_diff)


### TOT ###

new_base <- ts(tot_sai, start=c(2008,04),frequency=4)
str(new_base)

### Boucle pour stationariser ###
n <- ncol(new_base)
tot_less_diff = matrix(0,33,72)
count = 0
nom_col = names(new_base)
for (j in 1:n){
  result_test <- adf.test(new_base[,j])
  result_test2 <- kpss.test(new_base[,j])
  if (result_test$p.value>0.05 & result_test2$p.value<0.05 & j !=1){
    tot_less_diff[,j] <- diff(new_base[,j], differences = 1)
    count = count + 1
    cat("Variable stationnarisee : ",j, "\n")
  }else{
    tot_less_diff[,j] <- new_base[-1,j]
  }
}

cat("Nombre de variables stationnarisees : ", count, "/", n) #26
#26 variables stationnarisees

colnames(tot_less_diff) = colnames(tot_sai)
tot_less_diff <- as.data.frame(tot_less_diff)
str(tot_less_diff)
View(tot_less_diff)


### CHR8 ###
new_base <- ts(chr8_sai, start=c(2008,04),frequency=4)
str(new_base)

### Boucle pour stationariser ###
n <- ncol(new_base)
chr8_less_diff = matrix(0,33,84)
count = 0
nom_col = names(new_base)
for (j in 1:n){
  result_test <- adf.test(new_base[,j])
  result_test2 <- kpss.test(new_base[,j])
  if (result_test$p.value>0.05 & result_test2$p.value<0.05 & j != 1){
    chr8_less_diff[,j] <- diff(new_base[,j], differences = 1)
    count = count + 1
    cat("Variable stationnarisee : ",j, "\n")
  }else{
    chr8_less_diff[,j] <- new_base[-1,j]
  }
}

cat("Nombre de variables stationnarisees : ", count, "/", n) #42

colnames(chr8_less_diff) = colnames(chr8_sai)
chr8_less_diff <- as.data.frame(chr8_less_diff)
str(chr8_less_diff)
View(chr8_less_diff)

########################################################################


########################################################################

####################         CORRELATIONS          ####################

### Correlations avec le Y ###

### CHR2 ###
res.cor2 <- correlate(chr2_statio)
quartz()
windows()
res.cor2 %>%
  focus(DR) %>%
  mutate(term = reorder(term, DR)) %>%
  ggplot(aes(x= term, y=DR, fill=DR)) +
  ggtitle("Bar chart des correlations avec la variable a expliquer DR pour la matrice chr2") +
  xlab("variable") +
  ylab("correlation") +  
  scale_fill_gradient2(low = "dark blue", mid = "dark grey",
                       high = "red", space = "Lab" ) +
  geom_col() + 
  coord_flip() +
  theme(axis.text.y = element_text(size=5, color='black'),
        axis.text.x = element_text(color="black",size=10), 
        plot.title = element_text(hjust = 0.5))


### CHR8 ###
res.cor8 <- correlate(chr8_statio)
quartz()
windows()
res.cor8 %>%
  focus(DR) %>%
  mutate(term = reorder(term, DR)) %>%
  ggplot(aes(x= term, y=DR, fill=DR)) +
  ggtitle("Bar chart des correlations avec la variable a expliquer DR pour la matrice chr8") +
  xlab("variable") +
  ylab("correlation") +  
  scale_fill_gradient2(low = "dark blue", mid = "dark grey",
                       high = "red", space = "Lab" ) +
  geom_col() + 
  coord_flip() +
  theme(axis.text.y = element_text(size=5, color='black'),
        axis.text.x = element_text(color="black",size=10), 
        plot.title = element_text(hjust = 0.5))


### TOT ###
res.cortot <- correlate(tot_statio)
quartz()
windows()
res.cortot %>%
  focus(DR) %>%
  mutate(term = reorder(term, DR)) %>%
  ggplot(aes(x= term, y=DR, fill=DR)) +
  ggtitle("Bar chart des correlations avec la variable a expliquer DR pour la matrice tot") +
  xlab("variable") +
  ylab("correlation") +  
  scale_fill_gradient2(low = "dark blue", mid = "dark grey",
                       high = "red", space = "Lab" ) +
  geom_col() + 
  coord_flip() +
  theme(axis.text.y = element_text(size=5, color='black'),
        axis.text.x = element_text(color="black",size=10), 
        plot.title = element_text(hjust = 0.5))


#Stationnarisation du Y dans la base tot_less_diff
time =c(1:nrow(tot_less_diff))
test <- cbind(tot_less_diff, time)
reg <-lm(DR ~time, data=tot_less_diff)
plot(tot_less_diff$DR, type="l")
reg_fitted <- ts(reg$fitted, start=c(2009,01), frequency = 4)
lines(reg_fitted, col='red', lwd=2)

plot(test[,1]-reg_fitted, type='l',
     xlab="", col='orangered2')
plot(ts(reg$fitted.values))

tot_less_diff$DR = as.numeric(test[,1]-reg_fitted)




#exporter les bases de données 
write.xlsx(tot_less_diff, "tot_less_diff.xlsx", sheetName = "tot_less_diff",
           colNames = TRUE, rowNames = TRUE, append = FALSE)

write.xlsx(chr2_less_diff, "chr2_less_diff.xlsx", sheetName = "chr2_less_diff",
           colNames = TRUE, rowNames = TRUE, append = FALSE)

write.xlsx(chr8_less_diff, "chr8_less_diff.xlsx", sheetName = "chr8_less_diff",
           colNames = TRUE, rowNames = TRUE, append = FALSE)

#exporter la valeur de la tendance 
write.xlsx(reg_fitted, "tendance.xlsx") 
