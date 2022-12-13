########################################################################
#######################      DRIM GAME 2022      ######################

                        ### PREVISIONS MCO ###

########################################################################

### Importation des librairies ###
library(readxl)
library(lmtest)
library(car)
library(corrplot)
library(stringr)
library(stringr)
library(EnvStats)
library(outliers)
library(corrplot)
library(PerformanceAnalytics)
library(stats)
library(lmtest)
library(car)
library(gvlma)
library(nortest)
library(stats)

########################################################################


########################################################################

### Importation de l'environnement ###

#setwd("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/")
setwd("/Users/este.pcl/Desktop/DrimGame/PARIS/Scripts/prev_naive_ARIMA_mco/previsions MCO")
#setwd("C:/Users/alexa/OneDrive/Bureau/M2_EKAP/drim_game/BDDs")
#setwd("/Users/este.pcl/Desktop/DrimGame 2022/Script")

### Importation des bdd ###
chr2_less_diff <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/chr2_less_diff.xlsx")
chr8_less_diff <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/chr8_less_diff.xlsx")
tot_less_diff <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/tot_less_diff.xlsx")

chr2_less_diff <- chr2_less_diff[-c(26:33),]
chr8_less_diff <- chr8_less_diff[-c(26:33),]
tot_less_diff <- tot_less_diff[-c(26:33),]

########################################################################


########################################################################

############################       MCO        #########################

#Les variables explicatives de chacun des modeles ont ete recuperees au 
#prealable par une methode de selection de variable stepwise
#On retire la variable avec la p-value la plus eleve pour chaque modele realise
#jusqu'a obtenir un modele avec des variablex explicatives toutes significatives
#Un ensemble de tests sont realises ensuite sur ces modeles obtenus pour chaque profil

### CHR2 ###

mco<-lm(DR ~ p95_4 + mean_5 + p95_7 + p25_3 + mean_1 + p90_4 + p10_2 + p25_7 + mean_4  + moral_fr + p5_7
        + p90_5 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7 + PIB + p10_3, data = chr2_less_diff)
summary(mco)
#p10_3

mco<-lm(DR ~ p95_4 + mean_5 + p95_7 + p25_3 + mean_1 + p90_4 + p10_2 + p25_7 + mean_4  + moral_fr + p5_7
        + p90_5 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7 + PIB, data = chr2_less_diff)
summary(mco)
#p25_3

mco<-lm(DR ~ p95_4 + mean_5 + p95_7 + mean_1 + p90_4 + p10_2 + p25_7 + mean_4  + moral_fr + p5_7
        + p90_5 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7 + PIB, data = chr2_less_diff)
summary(mco)
#PIB

mco<-lm(DR ~ p95_4 + mean_5 + p95_7 + mean_1 + p90_4 + p10_2 + p25_7 + mean_4  + moral_fr + p5_7
        + p90_5 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7, data = chr2_less_diff)
summary(mco)
#p25_7

mco<-lm(DR ~ p95_4 + mean_5 + p95_7 + mean_1 + p90_4 + p10_2 + mean_4  + moral_fr + p5_7
        + p90_5 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7, data = chr2_less_diff)
summary(mco)
#p95_4

mco<-lm(DR ~ mean_5 + p95_7 + mean_1 + p90_4 + p10_2 + mean_4  + moral_fr + p5_7
        + p90_5 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7, data = chr2_less_diff)
summary(mco)
#p5_7

mco<-lm(DR ~ mean_5 + p95_7 + mean_1 + p90_4 + p10_2 + mean_4  + moral_fr
        + p90_5 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7, data = chr2_less_diff)
summary(mco)
#p10_2

mco<-lm(DR ~ mean_5 + p95_7 + mean_1 + p90_4 + mean_4  + moral_fr + p90_5 + 
          p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7, data = chr2_less_diff)
summary(mco)
#median_7

mco<-lm(DR ~ mean_5 + p95_7 + mean_1 + p90_4 + mean_4  + moral_fr + p90_5 + 
          p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1, data = chr2_less_diff)
summary(mco)
#p5_1

mcochr2<-lm(DR ~ mean_5 + p95_7 + mean_1 + p90_4 + mean_4  + moral_fr + p90_5 + 
          p25_5 + p75_2 + CD_MOD_HABI_1, data = chr2_less_diff)
summary(mcochr2)
#Modele retenu

### Test de Shapiro de normalite  des residus ###
mcochr2$residus<-residuals(mcochr2)

### Tests ###
shapiro.test(mcochr2$residus) #H0 accepte (p-value > 0.05) : Les residus suivent une loi normale
skewness(mcochr2$residus) #Skew = 0 donc symetrie
kurtosis(mcochr2$residus) #Kurtosis < 3 donc densite avec un pic moins important
ks.test(mcochr2$residus, "pnorm", mean(mcochr2$residus), sd(mcochr2$residus)) 
#Test de Kolmogorov-Smirnov accepte (p-value > 0.05) : normalite des individus

### Test de Breush-Pagan d'homoscedasticite  des residus ###
bptest(mcochr2) #H0 accepte (p-value > 0.05) : Homoscedasticite des residus

### Test de Ramsey de la linearite de la forme fonctionnelle ###
reset(mcochr2) #H0 accepte (p-value > 0.05) : Forme fonctionnelle  lineaire

### Test du VIF de multi colinearite des variables explicatives ###
vif(mcochr2) #aucune multi-colinearite

### Distance de cook ###
plot(cooks.distance(mcochr2),type="h")

### Previsions ###
chr2 <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/chr2_less_diff.xlsx")

y1 = 5.664e+02 + 2.198e-06*chr2$mean_5[26] - 2.966e-01*chr2$p95_7[26] - 5.691e+02*chr2$mean_1[26] 
+ 7.464e-03*chr2$p90_4[26] +  9.965e-03*chr2$mean_4[26] + 4.414e-03*chr2$moral_fr[26] - 9.423e-07*chr2$p90_5[26]
+  1.150e-06*chr2$p25_5[26] + 1.689e-04*chr2$p75_2[26] -1.251e-02*chr2$CD_MOD_HABI_1[26]

y2 = 5.664e+02 + 2.198e-06*chr2$mean_5[27] - 2.966e-01*chr2$p95_7[27] - 5.691e+02*chr2$mean_1[27] 
+ 7.464e-03*chr2$p90_4[27] +  9.965e-03*chr2$mean_4[27] + 4.414e-03*chr2$moral_fr[27] - 9.423e-07*chr2$p90_5[27]
+  1.150e-06*chr2$p25_5[27] + 1.689e-04*chr2$p75_2[27] -1.251e-02*chr2$CD_MOD_HABI_1[27]

y3 = 5.664e+02 + 2.198e-06*chr2$mean_5[28] - 2.966e-01*chr2$p95_7[28] - 5.691e+02*chr2$mean_1[28] 
+ 7.464e-03*chr2$p90_4[28] +  9.965e-03*chr2$mean_4[28] + 4.414e-03*chr2$moral_fr[28] - 9.423e-07*chr2$p90_5[28]
+  1.150e-06*chr2$p25_5[28] + 1.689e-04*chr2$p75_2[28] -1.251e-02*chr2$CD_MOD_HABI_1[28]

y4 = 5.664e+02 + 2.198e-06*chr2$mean_5[29] - 2.966e-01*chr2$p95_7[29] - 5.691e+02*chr2$mean_1[29] 
+ 7.464e-03*chr2$p90_4[29] +  9.965e-03*chr2$mean_4[29] + 4.414e-03*chr2$moral_fr[29] - 9.423e-07*chr2$p90_5[29]
+  1.150e-06*chr2$p25_5[29] + 1.689e-04*chr2$p75_2[29] -1.251e-02*chr2$CD_MOD_HABI_1[29]

y5 = 5.664e+02 + 2.198e-06*chr2$mean_5[30] - 2.966e-01*chr2$p95_7[30] - 5.691e+02*chr2$mean_1[30] 
+ 7.464e-03*chr2$p90_4[30] +  9.965e-03*chr2$mean_4[30] + 4.414e-03*chr2$moral_fr[30] - 9.423e-07*chr2$p90_5[30]
+  1.150e-06*chr2$p25_5[30] + 1.689e-04*chr2$p75_2[30] -1.251e-02*chr2$CD_MOD_HABI_1[30]

y6 = 5.664e+02 + 2.198e-06*chr2$mean_5[31] - 2.966e-01*chr2$p95_7[31] - 5.691e+02*chr2$mean_1[31] 
+ 7.464e-03*chr2$p90_4[31] +  9.965e-03*chr2$mean_4[31] + 4.414e-03*chr2$moral_fr[31] - 9.423e-07*chr2$p90_5[31]
+  1.150e-06*chr2$p25_5[31] + 1.689e-04*chr2$p75_2[31] -1.251e-02*chr2$CD_MOD_HABI_1[31]

y7 = 5.664e+02 + 2.198e-06*chr2$mean_5[32] - 2.966e-01*chr2$p95_7[32] - 5.691e+02*chr2$mean_1[32] 
+ 7.464e-03*chr2$p90_4[32] +  9.965e-03*chr2$mean_4[32] + 4.414e-03*chr2$moral_fr[32] - 9.423e-07*chr2$p90_5[32]
+  1.150e-06*chr2$p25_5[32] + 1.689e-04*chr2$p75_2[32] -1.251e-02*chr2$CD_MOD_HABI_1[32]

y8 = 5.664e+02 + 2.198e-06*chr2$mean_5[33] - 2.966e-01*chr2$p95_7[33] - 5.691e+02*chr2$mean_1[33] 
+ 7.464e-03*chr2$p90_4[33] +  9.965e-03*chr2$mean_4[33] + 4.414e-03*chr2$moral_fr[33] - 9.423e-07*chr2$p90_5[33]
+  1.150e-06*chr2$p25_5[33] + 1.689e-04*chr2$p75_2[33] -1.251e-02*chr2$CD_MOD_HABI_1[33]

########################################################################


########################################################################

### CHR8 ###

mco<-lm(DR ~ CD_PROF_2 + mean_7 + p25_6 + CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p25_1 
        + p5_8 + p95_5 + CD_PROF_3 + change_in_stock + PIB + median_3  + p5_6  + tx_endet  
        + p90_2 + p5_1 + GGTrend + p25_4, data = chr8_less_diff)
summary(mco)
#median_3

mco<-lm(DR ~ CD_PROF_2 + mean_7 + p25_6 + CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p25_1 
        + p5_8 + p95_5 + CD_PROF_3 + change_in_stock + PIB + p5_6  + tx_endet  
        + p90_2 + p5_1 + GGTrend + p25_4, data = chr8_less_diff)
summary(mco)
#p25_1

mco<-lm(DR ~ CD_PROF_2 + mean_7 + p25_6 + CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p5_8 
        + p95_5 + CD_PROF_3 + change_in_stock + PIB + p5_6  + tx_endet  
        + p90_2 + p5_1 + GGTrend + p25_4, data = chr8_less_diff)
summary(mco)
#p5_1

mco<-lm(DR ~ CD_PROF_2 + mean_7 + p25_6 + CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p5_8 
        + p95_5 + CD_PROF_3 + change_in_stock + PIB + p5_6  + tx_endet  
        + p90_2 + GGTrend + p25_4, data = chr8_less_diff)
summary(mco)
#p5_8

mco<-lm(DR ~ CD_PROF_2 + mean_7 + p25_6 + CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p95_5 
        + CD_PROF_3 + change_in_stock + PIB + p5_6  + tx_endet  
        + p90_2 + GGTrend + p25_4, data = chr8_less_diff)
summary(mco)
#p25_4

mco<-lm(DR ~ CD_PROF_2 + mean_7 + p25_6 + CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p95_5 
        + CD_PROF_3 + change_in_stock + PIB + p5_6  + tx_endet  
        + p90_2 + GGTrend, data = chr8_less_diff)
summary(mco)
#p5_6

mco<-lm(DR ~ CD_PROF_2 + mean_7 + p25_6 + CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p95_5 
        + CD_PROF_3 + change_in_stock + PIB + tx_endet + p90_2 + GGTrend, data = chr8_less_diff)
summary(mco)
#p95_5

mco<-lm(DR ~ CD_PROF_2 + mean_7 + p25_6 + CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + CD_PROF_3 
        + change_in_stock + PIB + tx_endet + p90_2 + GGTrend, data = chr8_less_diff)
summary(mco)
#change_in_stock

mco<-lm(DR ~ CD_PROF_2 + mean_7 + p25_6 + CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + CD_PROF_3 
        + PIB + tx_endet + p90_2 + GGTrend, data = chr8_less_diff)
summary(mco)
#CD_PROF_3

mco<-lm(DR ~ CD_PROF_2 + mean_7 + p25_6 + CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + PIB 
        + tx_endet + p90_2 + GGTrend, data = chr8_less_diff)
summary(mco)
#GGTrend

mcochr8<-lm(DR ~ CD_PROF_2 + mean_7 + p25_6 + CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + PIB 
        + tx_endet + p90_2, data = chr8_less_diff)
summary(mcochr8)
#Modele retenu

### Test de Shapiro de normalite  des residus ###
mcochr8$residus<-residuals(mcochr8)

### Tests ###
shapiro.test(mcochr8$residus) #H0 accepte (p-value > 0.05) : Les residus suivent une loi normale
skewness(mcochr8$residus) #Skew = 0 donc symetrie
kurtosis(mcochr8$residus) #Kurtosis < 3 donc densite avec un pic moins important
ks.test(mcochr8$residus, "pnorm", mean(mcochr8$residus), sd(mcochr8$residus)) 
#Test de Kolmogorov-Smirnov accepte (p-value > 0.05) : normalite des individus

### Test de Breush-Pagan d'homoscedasticite  des residus ###
bptest(mcochr8) #H0 accepte (p-value > 0.05) : Homoscedasticite des residus

### Test de Ramsey de la linearite de la forme fonctionnelle ###
reset(mcochr8) #H0 accepte (p-value > 0.05) : Forme fonctionnelle  lineaire

### Test du VIF de multi colinearite des variables explicatives ###
vif(mcochr8) #aucune multi-colinearite

### Distance de cook ###
plot(cooks.distance(mcochr8),type="h")

### Previsions ###
chr8 <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/chr8_less_diff.xlsx")

y1 = -5.001e+00 + 5.956e-01*chr8$CD_PROF_2[26] + 9.372e+01*chr8$mean_7[26] + 4.368e+03*chr8$p25_6[26] 
-6.229e-01*chr8$CD_TY_CLI_RCI_2[26] - 1.976e-01*chr8$CD_ETA_CIV_1[26] - 5.807e-05*chr8$PIB[26]
+  3.476e-01*chr8$tx_endet[26] + 1.268e-02*chr8$p90_2[26] 

y2 = -5.001e+00 + 5.956e-01*chr8$CD_PROF_2[27] + 9.372e+01*chr8$mean_7[27] + 4.368e+03*chr8$p25_6[27] 
-6.229e-01*chr8$CD_TY_CLI_RCI_2[27] - 1.976e-01*chr8$CD_ETA_CIV_1[27] - 5.807e-05*chr8$PIB[27]
+  3.476e-01*chr8$tx_endet[27] + 1.268e-02*chr8$p90_2[27] 

y3 = -5.001e+00 + 5.956e-01*chr8$CD_PROF_2[28] + 9.372e+01*chr8$mean_7[28] + 4.368e+03*chr8$p25_6[28] 
-6.229e-01*chr8$CD_TY_CLI_RCI_2[28] - 1.976e-01*chr8$CD_ETA_CIV_1[28] - 5.807e-05*chr8$PIB[28]
+  3.476e-01*chr8$tx_endet[28] + 1.268e-02*chr8$p90_2[28] 

y4 = -5.001e+00 + 5.956e-01*chr8$CD_PROF_2[29] + 9.372e+01*chr8$mean_7[29] + 4.368e+03*chr8$p25_6[29] 
-6.229e-01*chr8$CD_TY_CLI_RCI_2[29] - 1.976e-01*chr8$CD_ETA_CIV_1[29] - 5.807e-05*chr8$PIB[29]
+  3.476e-01*chr8$tx_endet[29] + 1.268e-02*chr8$p90_2[29] 

y5 = -5.001e+00 + 5.956e-01*chr8$CD_PROF_2[30] + 9.372e+01*chr8$mean_7[30] + 4.368e+03*chr8$p25_6[30] 
-6.229e-01*chr8$CD_TY_CLI_RCI_2[30] - 1.976e-01*chr8$CD_ETA_CIV_1[30] - 5.807e-05*chr8$PIB[30]
+  3.476e-01*chr8$tx_endet[30] + 1.268e-02*chr8$p90_2[30] 

y6 = -5.001e+00 + 5.956e-01*chr8$CD_PROF_2[31] + 9.372e+01*chr8$mean_7[31] + 4.368e+03*chr8$p25_6[31] 
-6.229e-01*chr8$CD_TY_CLI_RCI_2[31] - 1.976e-01*chr8$CD_ETA_CIV_1[31] - 5.807e-05*chr8$PIB[31]
+  3.476e-01*chr8$tx_endet[31] + 1.268e-02*chr8$p90_2[31] 

y7 = -5.001e+00 + 5.956e-01*chr8$CD_PROF_2[32] + 9.372e+01*chr8$mean_7[32] + 4.368e+03*chr8$p25_6[32] 
-6.229e-01*chr8$CD_TY_CLI_RCI_2[32] - 1.976e-01*chr8$CD_ETA_CIV_1[32] - 5.807e-05*chr8$PIB[32]
+  3.476e-01*chr8$tx_endet[32] + 1.268e-02*chr8$p90_2[32] 

y8 = -5.001e+00 + 5.956e-01*chr8$CD_PROF_2[33] + 9.372e+01*chr8$mean_7[33] + 4.368e+03*chr8$p25_6[33] 
-6.229e-01*chr8$CD_TY_CLI_RCI_2[33] - 1.976e-01*chr8$CD_ETA_CIV_1[33] - 5.807e-05*chr8$PIB[33]
+  3.476e-01*chr8$tx_endet[33] + 1.268e-02*chr8$p90_2[33] 

########################################################################


########################################################################

### TOT ###

mco<-lm(DR ~ p25_7 + median_3 + p25_8 + p10_2 + p10_3 + tx_retournement + p90_8 + p90_5  
        + p75_7 + CD_PROF_3 + p25_2 + moral_fr + tx_cho + change_in_stock + p25_1 + p90_3 
        + p95_2, data = tot_less_diff)
summary(mco)
#CD_PROF_3

mco<-lm(DR ~ p25_7 + median_3 + p25_8 + p10_2 + p10_3 + tx_retournement + p90_8 + p90_5  
        + p75_7 + p25_2 + moral_fr + tx_cho + change_in_stock + p25_1 + p90_3 
        + p95_2, data = tot_less_diff)
summary(mco)
#moral_fr

mco<-lm(DR ~ p25_7 + median_3 + p25_8 + p10_2 + p10_3 + tx_retournement + p90_8 + p90_5  
        + p75_7 + p25_2 + tx_cho + change_in_stock + p25_1 + p90_3 
        + p95_2, data = tot_less_diff)
summary(mco)
#tx_cho

mco<-lm(DR ~ p25_7 + median_3 + p25_8 + p10_2 + p10_3 + tx_retournement + p90_8 + p90_5  
        + p75_7 + p25_2 + change_in_stock + p25_1 + p90_3 + p95_2, data = tot_less_diff)
summary(mco)
#p25_1

mco<-lm(DR ~ p25_7 + median_3 + p25_8 + p10_2 + p10_3 + tx_retournement + p90_8 + p90_5  
        + p75_7 + p25_2 + change_in_stock + p90_3 + p95_2, data = tot_less_diff)
summary(mco)
#p25_2

mco<-lm(DR ~ p25_7 + median_3 + p25_8 + p10_2 + p10_3 + tx_retournement + p90_8 + p90_5  
        + p75_7 + change_in_stock + p90_3 + p95_2, data = tot_less_diff)
summary(mco)
#p95_2

mco<-lm(DR ~ p25_7 + median_3 + p25_8 + p10_2 + p10_3 + tx_retournement + p90_8 + p90_5  
        + p75_7 + change_in_stock + p90_3, data = tot_less_diff)
summary(mco)
#p75_7

mcotot<-lm(DR ~ p25_7 + median_3 + p25_8 + p10_2 + p10_3 + tx_retournement + p90_8 + p90_5  
        + change_in_stock + p90_3, data = tot_less_diff)
summary(mcotot)
#Modele retenu

### Test de Shapiro de normalite  des residus ###
mcotot$residus<-residuals(mcotot)

### Tests ###
shapiro.test(mcotot$residus) #H0 accepte (p-value > 0.05) : Les residus suivent une loi normale
skewness(mcotot$residus) #Skew = 0 donc symetrie
kurtosis(mcotot$residus) #Kurtosis < 3 donc densite avec un pic moins important
ks.test(mcotot$residus, "pnorm", mean(mcotot$residus), sd(mcotot$residus)) 
#Test de Kolmogorov-Smirnov accepte (p-value > 0.05) : normalite des individus

### Test de Breush-Pagan d'homoscedasticite  des residus ###
bptest(mcotot) #H0 accepte (p-value > 0.05) : Homoscedasticite des residus

### Test de Ramsey de la linearite de la forme fonctionnelle ###
reset(mcotot) #H0 accepte (p-value > 0.05) : Forme fonctionnelle  lineaire

### Test du VIF de multi colinearite des variables explicatives ###
vif(mcotot) #aucune multi-colinearite

### Distance de cook ###
plot(cooks.distance(mcotot),type="h")

### Previsions ###
tot <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/tot_less_diff.xlsx")

y1 = 1.372e+01 - 4.385e+02*tot$p25_7[26] - 9.730e-04*tot$median_3[26] - 2.657e+00*tot$p25_8[26] 
- 3.143e-03*tot$p10_2[26] + 3.624e-03*tot$p10_3[26] + 8.598e-03*tot$tx_retournement[26]
- 1.045e+00*tot$p90_8[26] - 7.096e-07*tot$p90_5[26] + 1.957e-12*tot$change_in_stock[26] - 2.144e-04*tot$p90_3[26]

y2 = 1.372e+01 - 4.385e+02*tot$p25_7[27] - 9.730e-04*tot$median_3[27] - 2.657e+00*tot$p25_8[27] 
- 3.143e-03*tot$p10_2[27] + 3.624e-03*tot$p10_3[27] + 8.598e-03*tot$tx_retournement[27]
- 1.045e+00*tot$p90_8[27] - 7.096e-07*tot$p90_5[27] + 1.957e-12*tot$change_in_stock[27] - 2.144e-04*tot$p90_3[27]

y3 = 1.372e+01 - 4.385e+02*tot$p25_7[28] - 9.730e-04*tot$median_3[28] - 2.657e+00*tot$p25_8[28] 
- 3.143e-03*tot$p10_2[28] + 3.624e-03*tot$p10_3[28] + 8.598e-03*tot$tx_retournement[28]
- 1.045e+00*tot$p90_8[28] - 7.096e-07*tot$p90_5[28] + 1.957e-12*tot$change_in_stock[28] - 2.144e-04*tot$p90_3[28]

y4 = 1.372e+01 - 4.385e+02*tot$p25_7[29] - 9.730e-04*tot$median_3[29] - 2.657e+00*tot$p25_8[29] 
- 3.143e-03*tot$p10_2[29] + 3.624e-03*tot$p10_3[29] + 8.598e-03*tot$tx_retournement[29]
- 1.045e+00*tot$p90_8[29] - 7.096e-07*tot$p90_5[29] + 1.957e-12*tot$change_in_stock[29] - 2.144e-04*tot$p90_3[29]

y5 = 1.372e+01 - 4.385e+02*tot$p25_7[30] - 9.730e-04*tot$median_3[30] - 2.657e+00*tot$p25_8[30] 
- 3.143e-03*tot$p10_2[30] + 3.624e-03*tot$p10_3[30] + 8.598e-03*tot$tx_retournement[30]
- 1.045e+00*tot$p90_8[30] - 7.096e-07*tot$p90_5[30] + 1.957e-12*tot$change_in_stock[30] - 2.144e-04*tot$p90_3[30]

y6 = 1.372e+01 - 4.385e+02*tot$p25_7[31] - 9.730e-04*tot$median_3[31] - 2.657e+00*tot$p25_8[31] 
- 3.143e-03*tot$p10_2[31] + 3.624e-03*tot$p10_3[31] + 8.598e-03*tot$tx_retournement[31]
- 1.045e+00*tot$p90_8[31] - 7.096e-07*tot$p90_5[31] + 1.957e-12*tot$change_in_stock[31] - 2.144e-04*tot$p90_3[31]

y7 = 1.372e+01 - 4.385e+02*tot$p25_7[32] - 9.730e-04*tot$median_3[32] - 2.657e+00*tot$p25_8[32] 
- 3.143e-03*tot$p10_2[32] + 3.624e-03*tot$p10_3[32] + 8.598e-03*tot$tx_retournement[32]
- 1.045e+00*tot$p90_8[32] - 7.096e-07*tot$p90_5[32] + 1.957e-12*tot$change_in_stock[32] - 2.144e-04*tot$p90_3[32]

y8 = 1.372e+01 - 4.385e+02*tot$p25_7[33] - 9.730e-04*tot$median_3[33] - 2.657e+00*tot$p25_8[33] 
- 3.143e-03*tot$p10_2[33] + 3.624e-03*tot$p10_3[33] + 8.598e-03*tot$tx_retournement[33]
- 1.045e+00*tot$p90_8[33] - 7.096e-07*tot$p90_5[33] + 1.957e-12*tot$change_in_stock[33] - 2.144e-04*tot$p90_3[33]

