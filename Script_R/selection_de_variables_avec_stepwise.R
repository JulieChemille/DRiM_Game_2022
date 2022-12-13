########################################################################
#######################      DRIM GAME 2022      #######################

########################################################################

### Importation des libraries ###
library(readxl)
library(lmtest)
library(car)
library(corrplot)
library(stringr)
#Packages
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
#setwd("/Users/emma/Desktop/DRiM/BDDs")
#setwd("/Users/este.pcl/Desktop/DrimGame 2022/Script")
setwd("C:/Users/alexa/OneDrive/Bureau/M2_EKAP/drim_game/Paris/finale")

### Importation de la base de donnees ###
tot_less_diff <- read_excel("tot_less_diff.xlsx")
chr2_less_diff <- read_excel("chr2_less_diff.xlsx")
chr8_less_diff <- read_excel("chr8_less_diff.xlsx")

tot_less_diff <- tot_less_diff[-c(26:33),]
chr2_less_diff <- chr2_less_diff[-c(26:33),]
chr8_less_diff <- chr8_less_diff[-c(26:33),]

tot2 <- tot_less_diff
chr2_bis <- chr2_less_diff
chr8_bis <- chr8_less_diff

########################################################################


########################################################################

###########################         TOT        ########################

### Selection de variables ###
modele<-lm(DR~.,data=tot_less_diff)
summary(modele)

### Methode Forward ###
modele0<-lm(DR~1,data=tot_less_diff)
step(modele0, scope=list(lower=modele0, upper=modele),data=tot,direction="forward") 


### Methode Stepwise ###
step(modele0,scope=list(upper=modele),data=base2,direction="both")

x <- lm(formula = DR ~ p25_7 + median_3 + p25_8 + p95_8 + p10_2 + 
          p10_3 + tx_retournement + p90_8 + p90_5 + p95_5 + p75_7 + 
          CD_PROF_3 + p25_2 + moral_fr + p75_2 + tx_cho + mean_1 + 
          change_in_stock + p25_1 + prix_moy_carb + p90_3 + p95_2 + 
          p95_3 + median_1, data = tot_less_diff)
summary(x)
vif(x)

#regarder les correlations entre les va 
test <- tot_less_diff[,c(colnames(x$model))]
str(test)
library(corrplot)
mcor <- cor(test)
mcor
corrplot(mcor, method= "color", outline = T, addgrid.col = "darkgray", order="hclust", addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, col = colorRampPalette(c("darkred","white","midnightblue"))(100))
#p95_3, p75_2


#on enleve p95_3 et garde p75_2
x <- lm(formula = DR ~ p25_7 + median_3 + p25_8 + p95_8 + p10_2 + 
          p10_3 + tx_retournement + p90_8 + p90_5 + p95_5 + p75_7 + 
          CD_PROF_3 + p25_2 + moral_fr + p75_2 + tx_cho + mean_1 + 
          change_in_stock + p25_1 + prix_moy_carb + p90_3 + p95_2 + 
          median_1, data = tot_less_diff)
summary(x)
vif(x)

#on enleve p75_2 et garde p95_3
y <- lm(formula = DR ~ p25_7 + median_3 + p25_8 + p95_8 + p10_2 + 
          p10_3 + tx_retournement + p90_8 + p90_5 + p95_5 + p75_7 + 
          CD_PROF_3 + p25_2 + moral_fr  + tx_cho + mean_1 + 
          change_in_stock + p25_1 + prix_moy_carb + p90_3 + p95_2 + 
          p95_3 + median_1, data = tot_less_diff)
summary(y)

lrtest(x,y)
#on garde x car LogLik + grand

vif(x)

#on enleve mean_1 car vif le + eleve 
x <- lm(formula = DR ~ p25_7 + median_3 + p25_8 + p95_8 + p10_2 + 
          p10_3 + tx_retournement + p90_8 + p90_5 + p95_5 + p75_7 + 
          CD_PROF_3 + p25_2 + moral_fr + p75_2 + tx_cho  + 
          change_in_stock + p25_1 + prix_moy_carb + p90_3 + p95_2 + 
          median_1, data = tot_less_diff)
summary(x)
vif(x)

#on enleve p95_8
x <- lm(formula = DR ~ p25_7 + median_3 + p25_8  + p10_2 + 
          p10_3 + tx_retournement + p90_8 + p90_5 + p95_5 + p75_7 + 
          CD_PROF_3 + p25_2 + moral_fr + p75_2 + tx_cho  + 
          change_in_stock + p25_1 + prix_moy_carb + p90_3 + p95_2 + 
          median_1, data = tot_less_diff)
summary(x)
vif(x)

#on enleve prix_moy_carb
x <- lm(formula = DR ~ p25_7 + median_3 + p25_8  + p10_2 + 
          p10_3 + tx_retournement + p90_8 + p90_5 + p95_5 + p75_7 + 
          CD_PROF_3 + p25_2 + moral_fr + p75_2 + tx_cho  + 
          change_in_stock + p25_1 + p90_3 + p95_2 + 
          median_1, data = tot_less_diff)
summary(x)
vif(x)



#on enleve median_1
x <- lm(formula = DR ~ p25_7 + median_3 + p25_8  + p10_2 + 
          p10_3 + tx_retournement + p90_8 + p90_5 + p95_5 + p75_7 + 
          CD_PROF_3 + p25_2 + moral_fr + p75_2 + tx_cho  + 
          change_in_stock + p25_1 + p90_3 + p95_2 
        , data = tot_less_diff)
summary(x)
vif(x)

#on enleve p95_5
x <- lm(formula = DR ~ p25_7 + median_3 + p25_8  + p10_2 + 
          p10_3 + tx_retournement + p90_8 + p90_5 + p75_7 + 
          CD_PROF_3 + p25_2 + moral_fr + p75_2 + tx_cho  + 
          change_in_stock + p25_1 + p90_3 + p95_2 
        , data = tot_less_diff)
summary(x)
vif(x)

#on enleve p75_2
x <- lm(formula = DR ~ p25_7 + median_3 + p25_8  + p10_2 + 
          p10_3 + tx_retournement + p90_8 + p90_5 + p75_7 + 
          CD_PROF_3 + p25_2 + moral_fr  + tx_cho  + 
          change_in_stock + p25_1 + p90_3 + p95_2 
        , data = tot_less_diff)
summary(x)
vif(x)



#Variables retenues : 
# p25_7  median_3  p25_8 p10_2 p10_3  tx_retournement  p90_8  p90_5  p75_7 CD_PROF_3 
#p25_2  moral_fr  tx_cho   change_in_stock  p25_1  p90_3  p95_2




mat_corr <- tot[,c(colnames(y$model))]
mcor <- cor(mat_corr)
corrplot(mcor, method= "color", outline = T, addgrid.col = "darkgray", order="hclust", addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, col = colorRampPalette(c("darkred","white","midnightblue"))(100))

### Test de Shapiro de normalite  des residus ###
tot2$residus2<-residuals(x)

### Tests ###
shapiro.test(tot2$residus2) #H0 accepte : Les residus suivent une loi normale
skewness(tot2$residus2) #Skew = 0 donc symetrie
kurtosis(tot2$residus2) #Kurtosis < 3 donc densite avec un pic moins important
ks.test(tot2$residus2, "pnorm", mean(tot2$residus2), sd(tot2$residus2)) #Test de Kolmogorov-Smirnov accepte : normalite des individus

### Test de Breush-Pagan d'homoscedasticite  des residus ###
bptest(x) #H0 accepte : Homoscedasticite des residus

### Test de Ramsey de la linearite de la forme fonctionnelle ###
reset(x) #H0 accepte : Forme fonctionnelle  lineaire

### Test du VIF de multi colinearite des variables explicatives ###
vif(x) #aucune multi-colinearite

### Distance de cook ###
plot(cooks.distance(x),type="h")

########################################################################


########################################################################

##########################        CHR2        ######################### 

### Selection des variables ###
modele<-lm(DR~.,data=chr2_less_diff)
summary(modele)

### Methode Forward ###
modele0<-lm(DR~1,data=chr2_less_diff)
step(modele0, scope=list(lower=modele0, upper=modele),data=chr2_less_diff,direction="forward") 

x2 <-lm(formula = DR ~ p10_8 + p75_5 + p95_4 + mean_5 + p95_7 + p25_3 + 
          mean_1 + p90_4 + p10_2 + p25_7 + mean_4 + moral_fr + p5_7 + 
          p90_5 + p25_1 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7 + 
          PIB + climat_affaires + p10_3 + median_1, data = chr2_less_diff)
summary(x2)

### Methode Stepwise ###
step(modele0,scope=list(upper=modele),data=chr2,direction="both")

lm(formula = DR ~ p10_8 + p75_5 + p95_4 + mean_5 + p95_7 + p25_3 + 
          mean_1 + p90_4 + p10_2 + p25_7 + mean_4 + moral_fr + p5_7 + 
          p90_5 + p25_1 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7 + 
          PIB + climat_affaires + p10_3 + median_1, data = chr2_less_diff)
summary(x2)

mat_corr <- chr2_less_diff[,c(colnames(x2$model))]
mcor <- cor(mat_corr)
corrplot(mcor, method= "color", outline = T, addgrid.col = "darkgray", order="hclust", addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, col = colorRampPalette(c("darkred","white","midnightblue"))(100))
#p25_1, median1

#modele sans p25_1 et avec median_1
x2 < lm(formula = DR ~ p10_8 + p75_5 + p95_4 + mean_5 + p95_7 + p25_3 + 
          mean_1 + p90_4 + p10_2 + p25_7 + mean_4 + moral_fr + p5_7 + 
          p90_5  + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7 + 
          PIB + climat_affaires + p10_3 + median_1, data = chr2_less_diff)
summary(x2)
vif(x2)

#modele sans median1 et avec p25_1
y2 <- lm(formula = DR ~ p10_8 + p75_5 + p95_4 + mean_5 + p95_7 + p25_3 + 
           mean_1 + p90_4 + p10_2 + p25_7 + mean_4 + moral_fr + p5_7 + 
           p90_5 + p25_1 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7 + 
           PIB + climat_affaires + p10_3 , data = chr2_less_diff)
summary(y2)
vif(y2)

lrtest(x2,y2) #on prefere y2

#sans p10_8
y2 <- lm(formula = DR ~  p75_5 + p95_4 + mean_5 + p95_7 + p25_3 + 
           mean_1 + p90_4 + p10_2 + p25_7 + mean_4 + moral_fr + p5_7 + 
           p90_5 + p25_1 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7 + 
           PIB + climat_affaires + p10_3 , data = chr2_less_diff)
summary(y2)
vif(y2)

#sans p75_5
y2 <- lm(formula = DR ~  p95_4 + mean_5 + p95_7 + p25_3 + 
           mean_1 + p90_4 + p10_2 + p25_7 + mean_4 + moral_fr + p5_7 + 
           p90_5 + p25_1 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7 + 
           PIB + climat_affaires + p10_3 , data = chr2_less_diff)
summary(y2)
vif(y2)

#on enleve climat_affaires
y2 <- lm(formula = DR ~  p95_4 + mean_5 + p95_7 + p25_3 + 
           mean_1 + p90_4 + p10_2 + p25_7 + mean_4 + moral_fr + p5_7 + 
           p90_5 + p25_1 + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7 + 
           PIB  + p10_3 , data = chr2_less_diff)
summary(y2)
vif(y2)

#on enleve p25_1
y2 <- lm(formula = DR ~  p95_4 + mean_5 + p95_7 + p25_3 + 
           mean_1 + p90_4 + p10_2 + p25_7 + mean_4 + moral_fr + p5_7 + 
           p90_5  + p25_5 + p5_1 + p75_2 + CD_MOD_HABI_1 + median_7 + 
           PIB  + p10_3 , data = chr2_less_diff)
summary(y2)
vif(y2)

#p95_4  mean_5  p95_7  p25_3 mean_1  p90_4  p10_2  p25_7  mean_4  moral_fr  p5_7 
#p90_5   p25_5  p5_1  p75_2 CD_MOD_HABI_1  median_7 PIB p10_3
  


### Test de Shapiro de normalite  des residus ###
chr2_bis$residus2<-residuals(y2)

### Tests ###
shapiro.test(chr2_bis$residus2) #H0 accepte : Les residus suivent une loi normale
skewness(chr2_bis$residus2) #Skew = 0 donc symetrie
kurtosis(chr2_bis$residus2) #Kurtosis < 3 donc densite avec un pic moins important
ks.test(chr2_bis$residus2, "pnorm", mean(chr2_bis$residus2), sd(chr2_bis$residus2)) #Test de Kolmogorov-Smirnov accepte : normalite des individus

### Test de Breush-Pagan d'homoscedasticite  des residus ###
bptest(y2) #H0 accepte : Homoscedasticite des residus

### Test de Ramsey de la linearite de la forme fonctionnelle ###
reset(y2) #H0 accepte : Forme fonctionnelle pas lineaire

### Test du VIF de multi colinearite des variables explicatives ###
vif(y2) #aucune multi-colinearite

### Distance de cook ###
plot(cooks.distance(y2),type="h")

########################################################################


########################################################################

##########################         CHR8        ########################

### Selection des variables ###
modele<-lm(DR~.,data=chr8_less_diff)
summary(modele)

### Methode Forward ###
modele0<-lm(DR~1,data=chr8_less_diff)
step(modele0, scope=list(lower=modele0, upper=modele),data=chr8_less_diff,direction="forward") 

x3 <- lm(formula = DR ~ mean_6 + p95_7 + CD_PROF_2 + mean_7 + p25_6 + 
           CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p25_1 + p5_8 + p95_5 + CD_PROF_3 + 
           change_in_stock + p90_6 + PIB + median_3 + p5_6 + tx_endet + 
           mean_8 + p90_2 + p5_1 + GGTrend + p25_4 + p10_1 + mean_1, 
         data = chr8_less_diff)
summary(x3)

### Methode Stepwise ###
step(modele0,scope=list(upper=modele),data=chr8_less_diff,direction="both")

lm(formula = DR ~ mean_6 + p95_7 + CD_PROF_2 + mean_7 + p25_6 + 
     CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p25_1 + p5_8 + p95_5 + CD_PROF_3 + 
     change_in_stock + p90_6 + PIB + median_3 + p5_6 + tx_endet + 
     mean_8 + p90_2 + p5_1 + GGTrend + p25_4 + p10_1 + mean_1, 
   data = chr8_less_diff)

mat_corr <- chr8_less_diff[,c(colnames(x3$model))]
mcor <- cor(mat_corr)
cor(mat_corr)
corrplot(mcor, method= "color", outline = T, addgrid.col = "darkgray", order="hclust", addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, col = colorRampPalette(c("darkred","white","midnightblue"))(100))
#mean_1, p25_1

#sans p25_1 avec mean_1
x3 <- lm(formula = DR ~ mean_6 + p95_7 + CD_PROF_2 + mean_7 + p25_6 + 
           CD_TY_CLI_RCI_2 + CD_ETA_CIV_1  + p5_8 + p95_5 + CD_PROF_3 + 
           change_in_stock + p90_6 + PIB + median_3 + p5_6 + tx_endet + 
           mean_8 + p90_2 + p5_1 + GGTrend + p25_4 + p10_1 + mean_1, 
         data = chr8_less_diff)
summary(x3)
vif(x3)

#sans mean_1 avec p25_1
y3 <- lm(formula = DR ~ mean_6 + p95_7 + CD_PROF_2 + mean_7 + p25_6 + 
           CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p25_1 + p5_8 + p95_5 + CD_PROF_3 + 
           change_in_stock + p90_6 + PIB + median_3 + p5_6 + tx_endet + 
           mean_8 + p90_2 + p5_1 + GGTrend + p25_4 + p10_1, 
         data = chr8_less_diff)
summary(y3)
vif(y3)

lrtest(x3,y3) #on prefere y3

#sans mean_6
y3 <- lm(formula = DR ~  p95_7 + CD_PROF_2 + mean_7 + p25_6 + 
           CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p25_1 + p5_8 + p95_5 + CD_PROF_3 + 
           change_in_stock + p90_6 + PIB + median_3 + p5_6 + tx_endet + 
           mean_8 + p90_2 + p5_1 + GGTrend + p25_4 + p10_1, 
         data = chr8_less_diff)
summary(y3)
vif(y3)

#on enleve p10_1
y3 <- lm(formula = DR ~  p95_7 + CD_PROF_2 + mean_7 + p25_6 + 
           CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p25_1 + p5_8 + p95_5 + CD_PROF_3 + 
           change_in_stock + p90_6 + PIB + median_3 + p5_6 + tx_endet + 
           mean_8 + p90_2 + p5_1 + GGTrend + p25_4, 
         data = chr8_less_diff)
summary(y3)
vif(y3)

#on enleve mean_8
y3 <- lm(formula = DR ~  p95_7 + CD_PROF_2 + mean_7 + p25_6 + 
           CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p25_1 + p5_8 + p95_5 + CD_PROF_3 + 
           change_in_stock + p90_6 + PIB + median_3 + p5_6 + tx_endet + 
            p90_2 + p5_1 + GGTrend + p25_4, 
         data = chr8_less_diff)
summary(y3)
vif(y3)

#on enleve p90_6
y3 <- lm(formula = DR ~  p95_7 + CD_PROF_2 + mean_7 + p25_6 + 
           CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p25_1 + p5_8 + p95_5 + CD_PROF_3 + 
           change_in_stock + PIB + median_3 + p5_6 + tx_endet + 
           p90_2 + p5_1 + GGTrend + p25_4, 
         data = chr8_less_diff)
summary(y3)
vif(y3)

#on enleve p95_7
y3 <- lm(formula = DR ~  CD_PROF_2 + mean_7 + p25_6 + 
           CD_TY_CLI_RCI_2 + CD_ETA_CIV_1 + p25_1 + p5_8 + p95_5 + CD_PROF_3 + 
           change_in_stock + PIB + median_3 + p5_6 + tx_endet + 
           p90_2 + p5_1 + GGTrend + p25_4, 
         data = chr8_less_diff)
summary(y3)
vif(y3)

# variables selectionnees : CD_PROF_2  mean_7  p25_6 CD_TY_CLI_RCI_2  CD_ETA_CIV_1  p25_1 
#p5_8  p95_5  CD_PROF_3  change_in_stock  PIB  median_3  p5_6  tx_endet  
#p90_2 + p5_1 + GGTrend + p25_4

mat_corr <- chr8[,c(colnames(y3$model))]
mcor <- cor(mat_corr)
cor(mat_corr)
corrplot(mcor, method= "color", outline = T, addgrid.col = "darkgray", order="hclust", addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, col = colorRampPalette(c("darkred","white","midnightblue"))(100))

### Test de Shapiro de normalite  des residus ###
chr8_bis$residus2<-residuals(y3)

### Tests ###
shapiro.test(chr8_bis$residus2) #H0 accepte : Les residus suivent une loi normale
skewness(chr8_bis$residus2) #Skew = 0 donc symetrie
kurtosis(chr8_bis$residus2) #Kurtosis < 3 donc densite avec un pic moins important
ks.test(chr8_bis$residus2, "pnorm", mean(chr8_bis$residus2), sd(chr8_bis$residus2)) #Test de Kolmogorov-Smirnov accepte : normalite des individus

### Test de Breush-Pagan d'homoscedasticite  des residus ###
bptest(y3) #H0 accepte : Homoscedasticite des residus

### Test de Ramsey de la linearite de la forme fonctionnelle ###
reset(y3) #H0 accepte : Forme fonctionnelle pas lineaire

### Distance de cook ###
plot(cooks.distance(y3),type="h")

