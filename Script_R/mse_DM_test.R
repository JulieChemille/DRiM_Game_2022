########################################################################

#######################      DRIM GAME 2022      #######################

########################################################################
### Importation des libraries
library('forecast')
########################################################################

### Importation de l'environnement ###

#setwd("C:/Users/Ju/OneDrive/Documents/Master EKAP/DRiM Game/BDDs/")
#setwd("/Users/emma/Desktop/DRiM/BDDs")
setwd("/Users/este.pcl/Desktop/DrimGame/PARIS/Scripts/mse_tot")
#setwd("C:/Users/alexa/OneDrive/Bureau/M2_EKAP/drim_game/BDDs")

### Importation des bdd ###

chr2_less_diff <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/chr2_less_diff.xlsx")
chr8_less_diff <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/chr8_less_diff.xlsx")
tot_less_diff <- read_excel("/Users/este.pcl/Desktop/DrimGame/PARIS/BDD/bases PARIS/tot_less_diff.xlsx")

### Importation des valeurs prédites ###
Previsionstot <- read_excel("/Users/este.pcl/Downloads/mse_tot.xlsx", sheet = 1)
Previsionschr8 <- read_excel("/Users/este.pcl/Downloads/mse_chr8.xlsx", sheet = 1)
Previsionschr2 <- read_excel("/Users/este.pcl/Downloads/mse_chr2.xlsx", sheet = 1)

### Valeurs réalisées sur 8 points ###
# Previsionstot$valeurstot <- tot_less_diff[26:33,]
# Previsionstot$valeurstot <- Previsionstot$valeurstot$DR

valeurs2 <- chr2_less_diff[26:33,]
valeurs2 <- valeurs2$DR

#valeurs8 <- chr8_less_diff[26:33,]
#valeurs8 <- valeurs8$DR

########################################################################


########################################################################

##### MSE ####

### Creation de la fonction MSE ###
mse <- function(error){mean(error^2)}

########################################################################

### MSE pour les prévisions réalisées sur tot_less_diff ###

#prednaive	
erreurnaivetot <- Previsionstot$valeurstot - Previsionstot$prednaive
mse(erreurnaivetot)

#predarima	
erreurarimatot <- Previsionstot$valeurstot - Previsionstot$predarima
mse(erreurarimatot)

#predarx
erreurarxtot <- Previsionstot$valeurstot - Previsionstot$predarx
mse(erreurarxtot)

#predarx_1
erreurarx1tot <- Previsionstot$valeurstot - Previsionstot$predarx_1
mse(erreurarx1tot)

#	predridge
erreurridgetot <-Previsionstot$valeurstot - Previsionstot$predridge
mse(erreurridgetot)

#	predridge_ret
erreurridge_rettot <-Previsionstot$valeurstot - Previsionstot$predridge_ret
mse(erreurridge_rettot)

#	predlasso
erreurlassotot <-Previsionstot$valeurstot - Previsionstot$predlasso
mse(erreurlassotot)

#	predlasso_ret
erreurlasso_rettot <-Previsionstot$valeurstot - Previsionstot$predlasso_ret
mse(erreurlasso_rettot)

#preden
erreurentot <-Previsionstot$valeurstot - Previsionstot$preden
mse(erreurentot)

# preden_ret
erreuren_rettot <-Previsionstot$valeurstot - Previsionstot$preden_ret
mse(erreuren_rettot)

# predscad	
erreurscadtot <-Previsionstot$valeurstot - Previsionstot$predscad
mse(erreurscadtot)

# predscad_ret
erreurscad_rettot <-Previsionstot$valeurstot - Previsionstot$predscad_ret
mse(erreurscad_rettot)

# predRF	
erreurRFtot <-Previsionstot$valeurstot - Previsionstot$predRF
mse(erreurRFtot)

# predMCO
erreurMCOtot <-Previsionstot$valeurstot - Previsionstot$predMCO
mse(erreurMCOtot)

########################################################################

### MSE pour les prévisions réalisées sur chr2_less_diff ###

#prednaive	
erreurnaive2 <- valeurs2 - Previsions2$prednaive
mse(erreurnaive2)

#predarima	
erreurarima2 <- Previsionstot$valeurstot - Previsions2$predarima
mse(erreurarima2)

# predarx
erreurarx2 <- valeurs2 - Previsions2$predarx
mse(erreurarx2)

#	predarx_1
erreurarx12 <- valeurs2 - Previsions2$predarx_1
mse(erreurarx12)

#	predridge
erreurridge2 <-valeurs2 - Previsions2$predridge
mse(erreurridge2)

#	predridge_ret
erreurridge_ret2 <-valeurs2 - Previsions2$predridge_ret
mse(erreurridge_ret2)

#	predlasso
erreurlasso2 <-valeurs2 - Previsions2$predlasso
mse(erreurlasso2)

#	predlasso_ret
erreurlasso_ret2 <-valeurs2 - Previsions2$predlasso_ret
mse(erreurlasso_ret2)

# preden
erreuren2 <-valeurs2 - Previsions2$preden
mse(erreuren2)

# preden_ret
erreuren_ret2 <-valeurs2 - Previsions2$preden_ret
mse(erreuren_ret2)

# predscad	
erreurscad2 <-valeurs2 - Previsions2$predscad
mse(erreurscad2)

# predscad_ret
erreurscad_ret2 <-valeurs2 - Previsions2$predscad_ret
mse(erreurscad_ret2)

# predRF	
erreurRF2 <-valeurs2 - Previsions2$predRF
mse(erreurRF2)

# predRF_ret	
erreurRF_ret2 <-valeurs2 - Previsions2$predRF_ret
mse(erreurRF_ret2)

# predardl
erreurardl2 <-valeurs2 - Previsions2$predardl
mse(erreurardl2)

# predMCO
erreurMCO2 <-Previsionstot$valeurstot - Previsions2$predMCO
mse(erreurMCO2)


########################################################################

### MSE pour les prévisions réalisées sur chr8_less_diff ###

#prednaive	
erreurnaive8 <- valeurs8 - Previsions8$prednaive
mse(erreurnaive8)

#predarima	
erreurarima8 <- Previsionstot$valeurstot - Previsions8$predarima
mse(erreurarima8)

# predarx
erreurarx8 <- valeurs8 - Previsions8$predarx
mse(erreurarx8)

#	predarx_1
erreurarx18 <- valeurs8 - Previsions8$predarx_1
mse(erreurarx18)

#	predridge
erreurridge8 <-valeurs8 - Previsions8$predridge
mse(erreurridge8)

#	predridge_ret
erreurridge_ret8 <-valeurs8 - Previsions8$predridge_ret
mse(erreurridge_ret8)

#	predlasso
erreurlasso8 <-valeurs8 - Previsions8$predlasso
mse(erreurlasso8)

#	predlasso_ret
erreurlasso_ret8 <-valeurs8 - Previsions8$predlasso_ret
mse(erreurlasso_ret8)

# preden
erreuren8 <-valeurs8 - Previsions8$preden
mse(erreuren8)

# preden_ret
erreuren_ret8 <-valeurs8 - Previsions8$preden_ret
mse(erreuren_ret8)

# predscad	
erreurscad8 <-valeurs8 - Previsions8$predscad
mse(erreurscad8)

# predscad_ret
erreurscad_ret8 <-valeurs8 - Previsions8$predscad_ret
mse(erreurscad_ret8)

# predRF	
erreurRF8 <-valeurs8 - Previsions8$predRF
mse(erreurRF8)

# predRF_ret	
erreurRF_ret8 <-valeurs8 - Previsions8$predRF_ret
mse(erreurRF_ret8)

# predardl
erreurardl8 <-valeurs8 - Previsions8$predardl
mse(erreurardl8)

# predMCO
Previsionschr8$predMCO <- as.numeric(Previsionschr8$predMCO)
str(Previsionschr8$predMCO)
erreurMCO8 <-Previsionschr8$Val - Previsionschr8$predMCO
mse(erreurMCO8)

########################################################################


########################################################################



########################################################################

####################      TEST DIEBOLD MARIANO     #####################

########################################################################

# alternative="less", H1 : Méthode 2 moins bonne qualité que la première
library(forecast)
### création de la fonction DM avec une alternative de référence ###
i=1
fct_DM <- function(x){
  for ( i in 1:16) {
    resul = dm.test(x[1:8,1], x[1:8,1+i],alternative = c("less") , h = 8)
    resul = resul$p.value
    resul =as.numeric(resul)
    matrice_resultat[i+1,] <- resul
    
  }
  matrice_resultat <- as.data.frame(matrice_resultat)
  print( matrice_resultat)
}
########################################################################

########################################################################
####BASE tot_less_diff

### Différentes matrices avec premier modèle en référence
matrice_erreurs1 <-as.matrix(cbind(erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot,erreurridge_rettot, 
                                   erreurlassotot,erreurlasso_rettot,erreurentot,erreuren_rettot,erreurscadtot,erreurscad_rettot, 
                                   erreursRFtot,erreursRF_rettot,erreursardltot,erreurMCOtot ))
matrice_erreurs2 <-as.matrix(cbind(erreurarimatot, erreurnaivetot, erreurarxtot, erreurarx1tot, erreurridgetot, erreurridge_rettot, 
                                   erreurlassotot, erreurlasso_rettot, erreurentot, erreuren_rettot, erreurscadtot, erreurscad_rettot, 
                                   erreurRFtot, erreurRF_rettot, erreurardltot,erreurMCOtot))
matrice_erreurs3 <-as.matrix(cbind(erreurarxtot,erreurnaivetot, erreurarimatot, erreurarx1tot, erreurridgetot, erreurridge_rettot, 
                                   erreurlassotot, erreurlasso_rettot, erreurentot, erreuren_rettot, erreurscadtot, erreurscad_rettot, 
                                   erreurRFtot, erreurRF_rettot, erreurardltot,erreurMCOtot))
matrice_erreurs4 <-as.matrix(cbind(erreurarx1tot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurridgetot,erreurridge_rettot, 
                                   erreurlassotot,erreurlasso_rettot,erreurentot,erreuren_rettot,erreurscadtot,erreurscad_rettot, 
                                   erreurRFtot,erreurRF_rettot,erreurardltot,erreurMCOtot))
matrice_erreurs5 <-as.matrix(cbind(erreurridgetot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridge_rettot, 
                                   erreurlassotot,erreurlasso_rettot,erreurentot,erreuren_rettot,erreurscadtot,erreurscad_rettot, 
                                   erreurRFtot,erreurRF_rettot,erreurardltot,erreurMCOtot))
matrice_erreurs6 <-as.matrix(cbind(erreurridge_rettot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot, 
                                   erreurlassotot,erreurlasso_rettot,erreurentot,erreuren_rettot,erreurscadtot,erreurscad_rettot, 
                                   erreurRFtot,erreurRF_rettot,erreurardltot,erreurMCOtot))
matrice_erreurs7 <-as.matrix(cbind(erreurlassotot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot,
                                   erreurridge_rettot,erreurlasso_rettot,erreurentot,erreuren_rettot,erreurscadtot,erreurscad_rettot, 
                                   erreurRFtot,erreurRF_rettot,erreurardltot,erreurMCOtot))
matrice_erreurs8 <-as.matrix(cbind(erreurlasso_rettot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot,erreurridge_rettot, 
                                   erreurlassotot,erreurentot,erreuren_rettot,erreurscadtot,erreurscad_rettot, 
                                   erreurRFtot,erreurRF_rettot,erreurardltot,erreurMCOtot))
matrice_erreurs9 <-as.matrix(cbind(erreurentot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot,erreurridge_rettot, 
                                   erreurlassotot,erreurlasso_rettot,erreuren_rettot,erreurscadtot,erreurscad_rettot, 
                                   erreurRFtot,erreurRF_rettot,erreurardltot,erreurMCOtot))
matrice_erreurs10 <-as.matrix(cbind(erreuren_rettot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot,erreurridge_rettot, 
                                    erreurlassotot,erreurlasso_rettot,erreurentot,erreurscadtot,erreurscad_rettot, 
                                    erreurRFtot,erreurRF_rettot,erreurardltot,erreurMCOtot))
matrice_erreurs11 <-as.matrix(cbind(erreurscadtot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot,erreurridge_rettot, 
                                    erreurlassotot,erreurlasso_rettot,erreurentot,erreuren_rettot,erreurscad_rettot, 
                                    erreurRFtot,erreurRF_rettot,erreurardltot,erreurMCOtot))
matrice_erreurs12 <-as.matrix(cbind(erreurscad_rettot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot,erreurridge_rettot, 
                                    erreurlassotot,erreurlasso_rettot,erreurentot,erreuren_rettot,erreurscadtot, 
                                    erreurRFtot,erreurRF_rettot,erreurardltot,erreurMCOtot))
matrice_erreurs13 <-as.matrix(cbind(erreurRFtot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot,erreurridge_rettot, 
                                    erreurlassotot,erreurlasso_rettot,erreurentot,erreuren_rettot,erreurscadtot,erreurscad_rettot, 
                                    erreurRF_rettot,erreurardltot,erreurMCOtot))
matrice_erreurs14 <-as.matrix(cbind(erreurRF_rettot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot,erreurridge_rettot, 
                                    erreurlassotot,erreurlasso_rettot,erreurentot,erreuren_rettot,erreurscadtot,erreurscad_rettot, 
                                    erreurRFtot,erreurardltot,erreurMCOtot))
matrice_erreurs15 <-as.matrix(cbind(erreurardltot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot,erreurridge_rettot, 
                                    erreurlassotot,erreurlasso_rettot,erreurentot,erreuren_rettot,erreurscadtot,erreurscad_rettot, 
                                    erreurRFtot,erreurRF_rettot, erreurMCOtot))

matrice_erreurs16 <-as.matrix(cbind(erreurMCOtot,erreurnaivetot,erreurarimatot,erreurarxtot,erreurarx1tot,erreurridgetot,erreurridge_rettot, 
                                   erreurlassotot,erreurlasso_rettot,erreurentot,erreuren_rettot,erreurscadtot,erreurscad_rettot, 
                                   erreursRFtot,erreursRF_rettot,erreursardltot))

########################################################################
########################################################################

#### DM test avec erreurnaivetot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs1), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs1)
fct_DM(matrice_erreurs1)

#### DM test avec erreurarimatot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs2), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs2)
fct_DM(matrice_erreurs1)

#### DM test avec erreurarxtot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs3), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs3)
fct_DM(matrice_erreurs3)

#### DM test avec erreurarx1tot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs4), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs4)
fct_DM(matrice_erreurs4)

#### DM test avec erreurridgetot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs5), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs5)
fct_DM(matrice_erreurs5)

#### DM test avec erreurridge_rettot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs6), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs6)
fct_DM(matrice_erreurs6)

#### DM test avec erreurlassotot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs7), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs7)
fct_DM(matrice_erreurs7)

#### DM test avec erreurlasso_rettot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs8), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs8)
fct_DM(matrice_erreurs8)

#### DM test avec erreurentot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs9), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs9)
fct_DM(matrice_erreurs9)

#### DM test avec erreuren_rettot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs10), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs10)
fct_DM(matrice_erreurs10)

#### DM test avec erreurscadtot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs11), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs11)
fct_DM(matrice_erreurs11)

#### DM test avec erreurscad_rettot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs12), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs12)
fct_DM(matrice_erreurs12)

#### DM test avec erreursRFtot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs13), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs13)
fct_DM(matrice_erreurs13)

#### DM test avec erreursRF_rettot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs14), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs14)
fct_DM(matrice_erreurs14)

#### DM test avec erreursardltot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs15), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs15)
fct_DM(matrice_erreurs15)

#### DM test avec erreurMCOtot en référence
matrice_resultat <- matrix(0:0, nrow=ncol(erreurMCOtot), ncol=1)
rownames(matrice_resultat) <- colnames(erreurMCOtot)
fct_DM(erreurMCOtot)

########################################################################


########################################################################


########################################################################

########################################################################


########################################################################


########################################################################

####BASE chr2_less_diff
### Différentes matrices avec premier modèle en référence
matrice_erreurs1 <-as.matrix(cbind(erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2,erreurridge_ret2, 
                                   erreurlasso2,erreurlasso_ret2,erreuren2,erreuren_ret2,erreurscad2,erreurscad_ret2, 
                                   erreursRF2,erreursRF_ret2,erreursardl2,erreurMCO2 ))
matrice_erreurs2 <-as.matrix(cbind(erreurarima2, erreurnaive2, erreurarx2, erreurarx12, erreurridge2, erreurridge_ret2, 
                                   erreurlasso2, erreurlasso_ret2, erreuren2, erreuren_ret2, erreurscad2, erreurscad_ret2, 
                                   erreurRF2, erreurRF_ret2, erreurardl2,erreurMCO2))
matrice_erreurs3 <-as.matrix(cbind(erreurarx2,erreurnaive2, erreurarima2, erreurarx12, erreurridge2, erreurridge_ret2, 
                                   erreurlasso2, erreurlasso_ret2, erreuren2, erreuren_ret2, erreurscad2, erreurscad_ret2, 
                                   erreurRF2, erreurRF_ret2, erreurardl2,erreurMCO2))
matrice_erreurs4 <-as.matrix(cbind(erreurarx12,erreurnaive2,erreurarima2,erreurarx2,erreurridge2,erreurridge_ret2, 
                                   erreurlasso2,erreurlasso_ret2,erreuren2,erreuren_ret2,erreurscad2,erreurscad_ret2, 
                                   erreurRF2,erreurRF_ret2,erreurardl2,erreurMCO2))
matrice_erreurs5 <-as.matrix(cbind(erreurridge2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge_ret2, 
                                   erreurlasso2,erreurlasso_ret2,erreuren2,erreuren_ret2,erreurscad2,erreurscad_ret2, 
                                   erreurRF2,erreurRF_ret2,erreurardl2,erreurMCO2))
matrice_erreurs6 <-as.matrix(cbind(erreurridge_ret2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2, 
                                   erreurlasso2,erreurlasso_ret2,erreuren2,erreuren_ret2,erreurscad2,erreurscad_ret2, 
                                   erreurRF2,erreurRF_ret2,erreurardl2,erreurMCO2))
matrice_erreurs7 <-as.matrix(cbind(erreurlasso2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2,
                                   erreurridge_ret2,erreurlasso_ret2,erreuren2,erreuren_ret2,erreurscad2,erreurscad_ret2, 
                                   erreurRF2,erreurRF_ret2,erreurardl2,erreurMCO2))
matrice_erreurs8 <-as.matrix(cbind(erreurlasso_ret2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2,erreurridge_ret2, 
                                   erreurlasso2,erreuren2,erreuren_ret2,erreurscad2,erreurscad_ret2, 
                                   erreurRF2,erreurRF_ret2,erreurardl2,erreurMCO2))
matrice_erreurs9 <-as.matrix(cbind(erreuren2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2,erreurridge_ret2, 
                                   erreurlasso2,erreurlasso_ret2,erreuren_ret2,erreurscad2,erreurscad_ret2, 
                                   erreurRF2,erreurRF_ret2,erreurardl2,erreurMCO2))
matrice_erreurs10 <-as.matrix(cbind(erreuren_ret2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2,erreurridge_ret2, 
                                    erreurlasso2,erreurlasso_ret2,erreuren2,erreurscad2,erreurscad_ret2, 
                                    erreurRF2,erreurRF_ret2,erreurardl2,erreurMCO2))
matrice_erreurs11 <-as.matrix(cbind(erreurscad2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2,erreurridge_ret2, 
                                    erreurlasso2,erreurlasso_ret2,erreuren2,erreuren_ret2,erreurscad_ret2, 
                                    erreurRF2,erreurRF_ret2,erreurardl2,erreurMCO2))
matrice_erreurs12 <-as.matrix(cbind(erreurscad_ret2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2,erreurridge_ret2, 
                                    erreurlasso2,erreurlasso_ret2,erreuren2,erreuren_ret2,erreurscad2, 
                                    erreurRF2,erreurRF_ret2,erreurardl2,erreurMCO2))
matrice_erreurs13 <-as.matrix(cbind(erreurRF2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2,erreurridge_ret2, 
                                    erreurlasso2,erreurlasso_ret2,erreuren2,erreuren_ret2,erreurscad2,erreurscad_ret2, 
                                    erreurRF_ret2,erreurardl2,erreurMCO2))
matrice_erreurs14 <-as.matrix(cbind(erreurRF_ret2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2,erreurridge_ret2, 
                                    erreurlasso2,erreurlasso_ret2,erreuren2,erreuren_ret2,erreurscad2,erreurscad_ret2, 
                                    erreurRF2,erreurardl2,erreurMCO2))
matrice_erreurs15 <-as.matrix(cbind(erreurardl2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2,erreurridge_ret2, 
                                    erreurlasso2,erreurlasso_ret2,erreuren2,erreuren_ret2,erreurscad2,erreurscad_ret2, 
                                    erreurRF2,erreurRF_ret2, erreurMCO2))

matrice_erreurs16 <-as.matrix(cbind(erreurMCO2,erreurnaive2,erreurarima2,erreurarx2,erreurarx12,erreurridge2,erreurridge_ret2, 
                                    erreurlasso2,erreurlasso_ret2,erreuren2,erreuren_ret2,erreurscad2,erreurscad_ret2, 
                                    erreursRF2,erreursRF_ret2,erreursardl2))

########################################################################
########################################################################

#### DM test avec erreurnaive2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs1), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs1)
fct_DM(matrice_erreurs1)

#### DM test avec erreurarima2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs2), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs2)
fct_DM(matrice_erreurs1)

#### DM test avec erreurarx2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs3), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs3)
fct_DM(matrice_erreurs3)

#### DM test avec erreurarx12 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs4), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs4)
fct_DM(matrice_erreurs4)

#### DM test avec erreurridge2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs5), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs5)
fct_DM(matrice_erreurs5)

#### DM test avec erreurridge_ret2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs6), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs6)
fct_DM(matrice_erreurs6)

#### DM test avec erreurlasso2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs7), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs7)
fct_DM(matrice_erreurs7)

#### DM test avec erreurlasso_ret2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs8), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs8)
fct_DM(matrice_erreurs8)

#### DM test avec erreuren2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs9), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs9)
fct_DM(matrice_erreurs9)

#### DM test avec erreuren_ret2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs10), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs10)
fct_DM(matrice_erreurs10)

#### DM test avec erreurscad2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs11), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs11)
fct_DM(matrice_erreurs11)

#### DM test avec erreurscad_ret2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs12), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs12)
fct_DM(matrice_erreurs12)

#### DM test avec erreursRF2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs13), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs13)
fct_DM(matrice_erreurs13)

#### DM test avec erreursRF_ret2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs14), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs14)
fct_DM(matrice_erreurs14)

#### DM test avec erreursardl2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs15), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs15)
fct_DM(matrice_erreurs15)

#### DM test avec erreurMCO2 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(erreurMCO2), ncol=1)
rownames(matrice_resultat) <- colnames(erreurMCO2)
fct_DM(erreurMCO2)

########################################################################


########################################################################


########################################################################
###Base chr8_less_diff en référence

### Différentes matrices avec premier modèle en référence
matrice_erreurs1 <-as.matrix(cbind(erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8,erreurridge_ret8, 
                                   erreurlasso8,erreurlasso_ret8,erreuren8,erreuren_ret8,erreurscad8,erreurscad_ret8, 
                                   erreursRF8,erreursRF_ret8,erreursardl8,erreurMCO8 ))
matrice_erreurs2 <-as.matrix(cbind(erreurarima8, erreurnaive8, erreurarx8, erreurarx18, erreurridge8, erreurridge_ret8, 
                                   erreurlasso8, erreurlasso_ret8, erreuren8, erreuren_ret8, erreurscad8, erreurscad_ret8, 
                                   erreurRF8, erreurRF_ret8, erreurardl8,erreurMCO8))
matrice_erreurs3 <-as.matrix(cbind(erreurarx8,erreurnaive8, erreurarima8, erreurarx18, erreurridge8, erreurridge_ret8, 
                                   erreurlasso8, erreurlasso_ret8, erreuren8, erreuren_ret8, erreurscad8, erreurscad_ret8, 
                                   erreurRF8, erreurRF_ret8, erreurardl8,erreurMCO8))
matrice_erreurs4 <-as.matrix(cbind(erreurarx18,erreurnaive8,erreurarima8,erreurarx8,erreurridge8,erreurridge_ret8, 
                                   erreurlasso8,erreurlasso_ret8,erreuren8,erreuren_ret8,erreurscad8,erreurscad_ret8, 
                                   erreurRF8,erreurRF_ret8,erreurardl8,erreurMCO8))
matrice_erreurs5 <-as.matrix(cbind(erreurridge8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge_ret8, 
                                   erreurlasso8,erreurlasso_ret8,erreuren8,erreuren_ret8,erreurscad8,erreurscad_ret8, 
                                   erreurRF8,erreurRF_ret8,erreurardl8,erreurMCO8))
matrice_erreurs6 <-as.matrix(cbind(erreurridge_ret8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8, 
                                   erreurlasso8,erreurlasso_ret8,erreuren8,erreuren_ret8,erreurscad8,erreurscad_ret8, 
                                   erreurRF8,erreurRF_ret8,erreurardl8,erreurMCO8))
matrice_erreurs7 <-as.matrix(cbind(erreurlasso8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8,
                                   erreurridge_ret8,erreurlasso_ret8,erreuren8,erreuren_ret8,erreurscad8,erreurscad_ret8, 
                                   erreurRF8,erreurRF_ret8,erreurardl8,erreurMCO8))
matrice_erreurs8 <-as.matrix(cbind(erreurlasso_ret8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8,erreurridge_ret8, 
                                   erreurlasso8,erreuren8,erreuren_ret8,erreurscad8,erreurscad_ret8, 
                                   erreurRF8,erreurRF_ret8,erreurardl8,erreurMCO8))
matrice_erreurs9 <-as.matrix(cbind(erreuren8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8,erreurridge_ret8, 
                                   erreurlasso8,erreurlasso_ret8,erreuren_ret8,erreurscad8,erreurscad_ret8, 
                                   erreurRF8,erreurRF_ret8,erreurardl8,erreurMCO8))
matrice_erreurs10 <-as.matrix(cbind(erreuren_ret8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8,erreurridge_ret8, 
                                    erreurlasso8,erreurlasso_ret8,erreuren8,erreurscad8,erreurscad_ret8, 
                                    erreurRF8,erreurRF_ret8,erreurardl8,erreurMCO8))
matrice_erreurs11 <-as.matrix(cbind(erreurscad8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8,erreurridge_ret8, 
                                    erreurlasso8,erreurlasso_ret8,erreuren8,erreuren_ret8,erreurscad_ret8, 
                                    erreurRF8,erreurRF_ret8,erreurardl8,erreurMCO8))
matrice_erreurs12 <-as.matrix(cbind(erreurscad_ret8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8,erreurridge_ret8, 
                                    erreurlasso8,erreurlasso_ret8,erreuren8,erreuren_ret8,erreurscad8, 
                                    erreurRF8,erreurRF_ret8,erreurardl8,erreurMCO8))
matrice_erreurs13 <-as.matrix(cbind(erreurRF8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8,erreurridge_ret8, 
                                    erreurlasso8,erreurlasso_ret8,erreuren8,erreuren_ret8,erreurscad8,erreurscad_ret8, 
                                    erreurRF_ret8,erreurardl8,erreurMCO8))
matrice_erreurs14 <-as.matrix(cbind(erreurRF_ret8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8,erreurridge_ret8, 
                                    erreurlasso8,erreurlasso_ret8,erreuren8,erreuren_ret8,erreurscad8,erreurscad_ret8, 
                                    erreurRF8,erreurardl8,erreurMCO8))
matrice_erreurs15 <-as.matrix(cbind(erreurardl8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8,erreurridge_ret8, 
                                    erreurlasso8,erreurlasso_ret8,erreuren8,erreuren_ret8,erreurscad8,erreurscad_ret8, 
                                    erreurRF8,erreurRF_ret8, erreurMCO8))

matrice_erreurs16 <-as.matrix(cbind(erreurMCO8,erreurnaive8,erreurarima8,erreurarx8,erreurarx18,erreurridge8,erreurridge_ret8, 
                                    erreurlasso8,erreurlasso_ret8,erreuren8,erreuren_ret8,erreurscad8,erreurscad_ret8, 
                                    erreursRF8,erreursRF_ret8,erreursardl8))

########################################################################
########################################################################

#### DM test avec erreurnaive8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs1), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs1)
fct_DM(matrice_erreurs1)

#### DM test avec erreurarima8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs2), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs2)
fct_DM(matrice_erreurs1)

#### DM test avec erreurarx8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs3), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs3)
fct_DM(matrice_erreurs3)

#### DM test avec erreurarx18 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs4), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs4)
fct_DM(matrice_erreurs4)

#### DM test avec erreurridge8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs5), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs5)
fct_DM(matrice_erreurs5)

#### DM test avec erreurridge_ret8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs6), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs6)
fct_DM(matrice_erreurs6)

#### DM test avec erreurlasso8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs7), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs7)
fct_DM(matrice_erreurs7)

#### DM test avec erreurlasso_ret8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs8), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs8)
fct_DM(matrice_erreurs8)

#### DM test avec erreuren8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs9), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs9)
fct_DM(matrice_erreurs9)

#### DM test avec erreuren_ret8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs10), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs10)
fct_DM(matrice_erreurs10)

#### DM test avec erreurscad8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs11), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs11)
fct_DM(matrice_erreurs11)

#### DM test avec erreurscad_ret8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs12), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs12)
fct_DM(matrice_erreurs12)

#### DM test avec erreursRF8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs13), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs13)
fct_DM(matrice_erreurs13)

#### DM test avec erreursRF_ret8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs14), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs14)
fct_DM(matrice_erreurs14)

#### DM test avec erreursardl8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(matrice_erreurs15), ncol=1)
rownames(matrice_resultat) <- colnames(matrice_erreurs15)
fct_DM(matrice_erreurs15)

#### DM test avec erreurMCO8 en référence
matrice_resultat <- matrix(0:0, nrow=ncol(erreurMCO8), ncol=1)
rownames(matrice_resultat) <- colnames(erreurMCO8)
fct_DM(erreurMCO8)

