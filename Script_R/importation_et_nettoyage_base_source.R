########################################################################
#######################      DRIM GAME 2022      ######################

########################################################################

### Importation des librairies ###
#install.packages("xlsx")
#install.packages("stringr")
library(xlsx)
library(data.table)
library(stringr)
library(readxl)
library(openxlsx)

########################################################################


########################################################################

### Importation de l'environnement ###

#setwd("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/")
#setwd("/Users/emma/Desktop/BDDs")
#setwd("/Users/este.pcl/Desktop/DrimGame 2022/Script")
setwd("C:/Users/alexa/OneDrive/Bureau/M2_EKAP/drim_game/Paris/finale")
### Importation de la base de donnees ###
base <- read.csv2("total_red.csv",sep=',')
base <- as.data.frame(base)
dim(base)
str(base)

########################################################################


########################################################################

##################      Mise en forme de la base      #################

### Suppression des % de la base de donnees ###
base$DR<-str_replace_all(base$DR,pattern="%",replacement="")
base$CD_TY_CLI_RCI_1<-str_replace_all(base$CD_TY_CLI_RCI_1,pattern="%",replacement="")
base$CD_TY_CLI_RCI_2<-str_replace_all(base$CD_TY_CLI_RCI_2,pattern="%",replacement="")
base$CD_ETA_CIV_1<-str_replace_all(base$CD_ETA_CIV_1,pattern="%",replacement="")
base$CD_ETA_CIV_2<-str_replace_all(base$CD_ETA_CIV_2,pattern="%",replacement="")
base$CD_MOD_HABI_1<-str_replace_all(base$CD_MOD_HABI_1,pattern="%",replacement="")
base$CD_MOD_HABI_2<-str_replace_all(base$CD_MOD_HABI_2,pattern="%",replacement="")
base$CD_PROF_1<-str_replace_all(base$CD_PROF_1,pattern="%",replacement="")
base$CD_PROF_2<-str_replace_all(base$CD_PROF_2,pattern="%",replacement="")
base$CD_PROF_3<-str_replace_all(base$CD_PROF_3,pattern="%",replacement="")
base$CD_QUAL_VEH_1<-str_replace_all(base$CD_QUAL_VEH_1,pattern="%",replacement="")
base$CD_QUAL_VEH_2<-str_replace_all(base$CD_QUAL_VEH_2,pattern="%",replacement="")

str(base)
### Passage des variables en num ###
base[,3:78] <- sapply(base[,3:79],as.numeric)
base[,80:82] <- sapply(base[,80:82],as.numeric)
base <- base[,-79]
str(base)

### Separation des bases de donnees ###
totale <- base[which(base$CHRONIQUE != "CHR2" & base$CHRONIQUE != "CHR8"),]
chr8 <- base[which(base$CHRONIQUE != "CHR2" & base$CHRONIQUE != "Totale"),]
chr2 <- base[which(base$CHRONIQUE != "Totale" & base$CHRONIQUE != "CHR8"),]
str(totale)
str(chr2)
str(chr8)


write.xlsx(chr2, "BDD_X_chr2.xlsx", sheetName = "chr2", 
           colNames = TRUE, rowNames = TRUE, append = FALSE)
write.xlsx(chr8, "BDD_X_chr8.xlsx", sheetName = "chr8", 
           colNames = TRUE, rowNames = TRUE, append = FALSE)
write.xlsx(totale, "BDD_X_totale.xlsx", sheetName = "totale", 
           colNames = TRUE, rowNames = TRUE, append = FALSE)


