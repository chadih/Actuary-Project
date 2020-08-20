#### Import fonction Planchet #######################################

dossier=getwd() 
dossierOutils="C:/data/FPL/1255 - Tables d'expérience/0- outils"
#dossierTM=paste(dossierOutils,"/TM",sep="")
dossierOutils="http://www.ressources-actuarielles.net/C1256F13006585B2/0/041c5a177d963c7fc12573b5002387b4/$FILE"
dossierTM="http://www.ressources-actuarielles.net/C1256F13006585B2/0/91C780A7E1385379C1257F8C001FA87E/$FILE"
source(paste(dossierOutils,"\\OutilsSurvie.r",sep=""))





#### Import de la base ###############################################
library(readxl)
data_dc_autonome <- read_excel("C:/Users/hhj/Desktop/Resultat PREDICA/BDD predica 2017/data/data_dc_autonome.xlsx")
t <- data_dc_autonome








#### Mise en forme de la base #######################################

t$DateNaissance=as.Date(t$DateNaissance,"%d/%m/%Y")
t$DateEntree=as.Date(t$DateEntree,"%d/%m/%Y")
t$DateSortie=as.Date(t$DateSortie,"%d/%m/%Y")


# Suppression des individus Fausse declaration 
test1 <- length(c(t$ID_PER_ARPEGE))
t <- subset(t,t$CS_POL!=52)
t <- subset(t,t$CS_POL!=51)
t <- subset(t,t$CS_POL!=19)
t <- subset(t,t$CS_POL!=44)
test2 <- length(c(t$ID_PER_ARPEGE))

cat("Le nombre d'assuré supprimé pour fausse declaration/delai de carence est de ",test1-test2)









#### Sélection des enregistrements dans la période d'observation ########################

cat("Sélection des enregistrements","\n")
sDateDebutObservation="01/01/2000"
sDateFinObservation="31/12/2016"
DateDebutObservation=as.Date(sDateDebutObservation,"%d/%m/%Y")
DateFinObservation=as.Date(sDateFinObservation,"%d/%m/%Y")
t$DateEntree=pmax(t$DateEntree,DateDebutObservation)
t=subset(t,(t$DateEntree<=DateFinObservation))

#Suppression des enregistrements avec une date d'entrée antérieure à la naissance
t=subset(t,(t$DateNaissance<=t$DateEntree))





#### Calcul des indicatrice de censure et de non censure ###################
t$non_censure[t$Statut=="sinistré"]=1
t$non_censure[!(t$Statut=="sinistré")]=0

# Les individus sinistrés après la date de fin d'observation sont censurés à la date de fin d'observation
t$non_censure[(t$Statut=="sinistré")&(t$DateSortie>DateFinObservation)]=0
t$Statut[(t$Statut=="sinistré")&(t$DateSortie>DateFinObservation)]="autre"
t$DateSortie=pmin(t$DateSortie,DateFinObservation)





#### Calcul des âges #############################
t$AgeEntreeJours=as.double(as.numeric(difftime(t$DateEntree,t$DateNaissance,"","days")))
t$AgeSortieJours=as.double(as.numeric(difftime(t$DateSortie,t$DateNaissance,"","days")))
t$AgeSortieJours[is.na(t$AgeSortieJours)==1]=DateFinObservation-t$DateNaissance[is.na(t$AgeSortieJours)==1]
t$AgeEntree=t$AgeEntreeJours/365.25
t$AgeSortie=t$AgeSortieJours/365.25

#Suppression des âges aberrants
t_old=nrow(t)
t=subset(t,(t$AgeEntree>0))
cat(paste("Nombre de lignes après suppression des lignes pour lesquelles âge d'entrée<=0 : ",nrow(t)," - ",t_old-nrow(t)," lignes éliminées"),"\n")
t_old=nrow(t)
t=subset(t,(t$AgeEntree<100))
cat(paste("Nombre de lignes après suppression des lignes pour lesquelles âge d'entrée>=100 : ",nrow(t)," - ",t_old-nrow(t)," lignes éliminées"),"\n")
t_old=nrow(t)
t=subset(t,(t$AgeSortie>0))
cat(paste("Nombre de lignes après suppression des lignes pour lesquelles âge de sortie<=0 : ",nrow(t)," - ",t_old-nrow(t)," lignes éliminées"),"\n")
t_old=nrow(t)
t=subset(t,((t$AgeSortie-t$AgeEntree)>0))
cat(paste("Nombre de lignes après suppression des lignes pour lesquelles âge de sortie<=âge d'entrée : ",nrow(t)," - ",t_old-nrow(t)," lignes éliminées"),"\n")









#### Construction des tableaux de données H et F ###################
t_h=subset(t,(t$Sexe=="M"))
t_f=subset(t,(t$Sexe=="F"))

cat(paste("Proportion d'hommes (sex-ratio) : ",nrow(t_h)/nrow(t)),"\n")










#### Decompte des deces et exposition par année  #######################


#Décompte des décès par âge
decesVentilesH=ventileDeces(t_h,sDateDebutObservation,sDateFinObservation)
decesVentilesF=ventileDeces(t_f,sDateDebutObservation,sDateFinObservation)
decesVentiles=ventileDeces(t,sDateDebutObservation,sDateFinObservation)

#Décompte des expositions par âge
expoVentilesH=ventileExposition(t_h,sDateDebutObservation,sDateFinObservation)
expoVentilesF=ventileExposition(t_f,sDateDebutObservation,sDateFinObservation)
expoVentiles=ventileExposition(t,sDateDebutObservation,sDateFinObservation)

# Estimateur de Hoem pour les années 2000-2016
Q_H_Hoem=rowSums(decesVentilesH)/rowSums(expoVentilesH)
Q_H_Hoem[is.na(Q_H_Hoem)]=0
Q_F_Hoem=rowSums(decesVentilesF)/rowSums(expoVentilesF)
Q_F_Hoem[is.na(Q_F_Hoem)]=0
QX_Hoem=rowSums(decesVentiles)/rowSums(expoVentiles)
QX_Hoem[is.na(QX_Hoem)]=0

write.csv(Q_H_Hoem,file ="C:/Users/hhj/Desktop/Qx bruts Homme Hoem 2000-2016.csv" )
write.csv(Q_F_Hoem,file ="C:/Users/hhj/Desktop/Qx bruts Femme Hoem 2000-2016.csv")
write.csv(QX_Hoem,file ="C:/Users/hhj/Desktop/Qx bruts Mixte Hoem 2000-2016.csv")

write.csv(rowSums(expoVentilesH),file ="C:/Users/hhj/Desktop/Expo bruts Homme Hoem 2000-2016.csv" )
write.csv(rowSums(expoVentilesF),file ="C:/Users/hhj/Desktop/Expo bruts Femme Hoem 2000-2016.csv")
write.csv(rowSums(expoVentiles),file ="C:/Users/hhj/Desktop/Expo bruts Mixte Hoem 2000-2016.csv")

write.csv(rowSums(decesVentilesH),file ="C:/Users/hhj/Desktop/Deces bruts Homme Hoem 2000-2016.csv" )
write.csv(rowSums(decesVentilesF),file ="C:/Users/hhj/Desktop/Deces bruts Femme Hoem 2000-2016.csv")
write.csv(rowSums(decesVentiles),file ="C:/Users/hhj/Desktop/Deces bruts Mixte Hoem 2000-2016.csv")

####                                             ##############################
####                                             #############################
####                                             ##############################


#### Définition des Dxt et Ext ###############
Dxt_homme_prosp <- decesVentilesH
names(Dxt_homme_prosp) <- c(2000:2016)
rownames(Dxt_homme_prosp) <- c(0:120)

Dxt_femme_prosp <- decesVentilesF
names(Dxt_femme_prosp) <- c(2000:2016)
rownames(Dxt_femme_prosp) <- c(0:120)

Dxt_mixte_prosp <- decesVentiles
names(Dxt_mixte_prosp) <- c(2000:2016)
rownames(Dxt_mixte_prosp) <- c(0:120)

Ext_homme_prosp <- expoVentilesH
names(Ext_homme_prosp) <- c(2000:2016)
rownames(Ext_homme_prosp) <- c(0:120)

Ext_femme_prosp <- expoVentilesF
names(Ext_femme_prosp) <- c(2000:2016)
rownames(Ext_femme_prosp) <- c(0:120)


Ext_mixte_prosp <- expoVentiles
names(Ext_mixte_prosp) <- c(2000:2016)
rownames(Ext_mixte_prosp) <- c(0:120)




#### Taux de mortalité par an et moyenne d'age par an ####################

plot(2000:2016,colSums(Dxt_homme_prosp[40:87,])/colSums(Ext_homme_prosp[40:87,]),xlab = "Année",ylab="Taux de mortalité")



t$DateNaissance[which(t$DateEntree<as.Date("31/12/2002","%d/%m/%Y"))]

agefin2000=mean(as.double(as.numeric(difftime(as.Date("31/12/2000","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2000","%d/%m/%Y"))],"","days"))))/365.25

agefin2001=mean(as.double(as.numeric(difftime(as.Date("31/12/2001","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2001","%d/%m/%Y"))],"","days"))))/365.25

agefin2002=mean(as.double(as.numeric(difftime(as.Date("31/12/2002","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2002","%d/%m/%Y"))],"","days"))))/365.25

agefin2003=mean(as.double(as.numeric(difftime(as.Date("31/12/2003","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2003","%d/%m/%Y"))],"","days"))))/365.25

agefin2004=mean(as.double(as.numeric(difftime(as.Date("31/12/2004","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2004","%d/%m/%Y"))],"","days"))))/365.25

agefin2005=mean(as.double(as.numeric(difftime(as.Date("31/12/2005","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2005","%d/%m/%Y"))],"","days"))))/365.25

agefin2006=mean(as.double(as.numeric(difftime(as.Date("31/12/2006","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2006","%d/%m/%Y"))],"","days"))))/365.25

agefin2007=mean(as.double(as.numeric(difftime(as.Date("31/12/2007","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2007","%d/%m/%Y"))],"","days"))))/365.25

agefin2008=mean(as.double(as.numeric(difftime(as.Date("31/12/2008","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2008","%d/%m/%Y"))],"","days"))))/365.25

agefin2009=mean(as.double(as.numeric(difftime(as.Date("31/12/2009","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2009","%d/%m/%Y"))],"","days"))))/365.25

agefin2010=mean(as.double(as.numeric(difftime(as.Date("31/12/2010","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2010","%d/%m/%Y"))],"","days"))))/365.25

agefin2011=mean(as.double(as.numeric(difftime(as.Date("31/12/2011","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2011","%d/%m/%Y"))],"","days"))))/365.25

agefin2012=mean(as.double(as.numeric(difftime(as.Date("31/12/2012","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2012","%d/%m/%Y"))],"","days"))))/365.25

agefin2013=mean(as.double(as.numeric(difftime(as.Date("31/12/2013","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2013","%d/%m/%Y"))],"","days"))))/365.25

agefin2014=mean(as.double(as.numeric(difftime(as.Date("31/12/2014","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2014","%d/%m/%Y"))],"","days"))))/365.25

agefin2015=mean(as.double(as.numeric(difftime(as.Date("31/12/2015","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2015","%d/%m/%Y"))],"","days"))))/365.25

agefin2016=mean(as.double(as.numeric(difftime(as.Date("31/12/2016","%d/%m/%Y"),t$DateNaissance[which(t$DateEntree<as.Date("31/12/2016","%d/%m/%Y"))],"","days"))))/365.25

moyenneage <- c(agefin2000,agefin2001,agefin2002,agefin2003,agefin2004,agefin2005,agefin2006,agefin2007,agefin2008,agefin2009,agefin2010,agefin2011,agefin2012,agefin2013,agefin2014,agefin2015,agefin2016)



plot(2000:2016,colSums(Dxt_homme_prosp[40:87,])/colSums(Ext_homme_prosp[40:87,]),xlab = "Année",ylab="Taux de mortalité")

par(new=T)
plot(2000:2016,moyenneage, pch=15,  xlab="", ylab="", ylim=c(50,90), axes=F, type="o", col="red")

mtext("Axe de la courbe rouge",side=4,col="red",line=2.5)
axis(4, ylim=c(50,90), col="red",col.axis="red")

legend("topleft", legend = c("qx par an","moyenne d'âge"), col = c("black","red"), pch = c(1,15), bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))



####                                             ##############################
####                                             #############################
####                                             ##############################

#### Les âges retenus sont de 50 à 85 ans et les années sont de 2003 à 2016 ##############################

#### LC Hommes ############################################################


library(StMoMo)

# définition de la fonction de contrainte pour le modèle de Lee Carter
constLC <- function(ax, bx, kt, b0x, gc, wxt, ages) {
  c1 <- mean(kt[1, ], na.rm = TRUE) 
  c2 <- sum(bx[, 1], na.rm = TRUE) 
  list(ax = ax + c1 * bx, bx = bx / c2, kt = c2 * (kt - c1)) 
} 

# Spécification du modèle ( deux facons à la main ou modèle deja existant dans le package)
LC <- StMoMo(link = "log", staticAgeFun = TRUE, periodAgeFun = "NP", constFun = constLC)
LC <- lc()

# Fitting du modèle ( regression non linèaire voir Charpentier pour plus d'info)
LCfit <- fit(LC, Dxt= Dxt_homme_prosp[51:86,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[51:86,4:length(Dxt_homme_prosp)],ages=50:85 ,years=2003:2016)
plot(LCfit)
AIC(LCfit)
BIC(LCfit)
logLik(LCfit)




#### APC Hommes ############################################################

APC <- apc()

APCfit <- fit(APC, Dxt= Dxt_homme_prosp[51:86,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[51:86,4:length(Dxt_homme_prosp)],ages=50:85 ,years=2003:2016)
plot(APCfit)
AIC(APCfit)
BIC(APCfit)
logLik(APCfit)



#### RH Hommes ############################################################

RH <- rh()

RHfit <- fit(RH, Dxt= Dxt_homme_prosp[51:86,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[51:86,4:length(Dxt_homme_prosp)],ages=50:85 ,years=2003:2016)
plot(RHfit)
AIC(RHfit)
BIC(RHfit)
logLik(RHfit)


#### CBD Hommes Poisson ############################################################


CBD <- cbd(link = "log")

CBDfit <- fit(CBD, Dxt= Dxt_homme_prosp[51:86,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[51:86,4:length(Dxt_homme_prosp)],ages=50:85 ,years=2003:2016)
plot(CBDfit)
AIC(CBDfit)
BIC(CBDfit)
logLik(CBDfit)


#### CBD Hommes Binomial ############################################################

CBD_bin <- cbd()

CBD_binfit <- fit(CBD_bin, Dxt= Dxt_homme_prosp[51:86,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[51:86,4:length(Dxt_homme_prosp)],ages=50:85 ,years=2003:2016)
plot(CBD_binfit)
AIC(CBD_binfit)
BIC(CBD_binfit)
logLik(CBD_binfit)

#### M7 Hommes Poisson ############################################################

M7 <- m7(link = "log")

M7fit <- fit(M7, Dxt= Dxt_homme_prosp[51:86,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[51:86,4:length(Dxt_homme_prosp)],ages=50:85 ,years=2003:2016)
plot(M7fit)
AIC(M7fit)
BIC(M7fit)
logLik(M7fit)


#### M7 Hommes Binomial ############################################################

M7_bin <- m7()

M7_binfit <- fit(M7_bin, Dxt= Dxt_homme_prosp[51:86,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[51:86,4:length(Dxt_homme_prosp)],ages=50:85 ,years=2003:2016)
plot(M7_binfit)
AIC(M7_binfit)
BIC(M7_binfit)
logLik(M7_binfit)


#### PLAT Hommes ##################################################################

f2 <- function(x, ages) mean(ages) - x
constPlat <- function(ax, bx, kt, b0x, gc, wxt, ages) {
  nYears <- dim(wxt)[2]
  x <- ages
  t <- 1:nYears
  c <- (1 - tail(ages, 1)):(nYears - ages[1])
  xbar <- mean(x)
  #nsum g(c)=0, nsum cg(c)=0, nsum c^2g(c)=0
  phiReg <- lm(gc ~ 1 + c + I(c^2), na.action = na.omit)
  phi <- coef(phiReg)
  gc <- gc - phi[1] - phi[2] * c - phi[3] * c^2
  kt[2, ] <- kt[2, ] + 2 * phi[3] * t
  kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t^2 - 2 * xbar * t)
  ax <- ax + phi[1] - phi[2] * x + phi[3] * x^2
  #nsum kt[i, ] = 0
  ci <- rowMeans(kt, na.rm = TRUE)
  ax <- ax + ci[1] + ci[2] * (xbar - x)
  kt[1, ] <- kt[1, ] - ci[1]
  kt[2, ] <- kt[2, ] - ci[2]

  list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
}
PLAT<- StMoMo(link = "log", staticAgeFun = TRUE,
              periodAgeFun = c("1", f2), cohortAgeFun = "1",
              constFun = constPlat)


PLATfit <-  fit(PLAT, Dxt= Dxt_homme_prosp[51:86,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[51:86,4:length(Dxt_homme_prosp)],ages=50:85 ,years=2003:2016)

plot(PLATfit)
AIC(PLATfit)
BIC(PLATfit)
logLik(PLATfit)





# Résultat et comparaison

Comparaison <- matrix(nrow = 8,ncol = 3)


Comparaison[,1] <- c(logLik(LCfit),logLik(APCfit),logLik(RHfit),logLik(CBDfit),logLik(CBD_binfit),logLik(M7fit),logLik(M7_binfit),logLik(PLATfit))

Comparaison[,2] <- c(AIC(LCfit),AIC(APCfit),AIC(RHfit),AIC(CBDfit),AIC(CBD_binfit),AIC(M7fit),AIC(M7_binfit),AIC(PLATfit))

Comparaison[,3] <- c(BIC(LCfit),BIC(APCfit),BIC(RHfit),BIC(CBDfit),BIC(CBD_binfit),BIC(M7fit),BIC(M7_binfit),BIC(PLATfit))

colnames(Comparaison) <- c("LogLik","AIC","BIC")
rownames(Comparaison) <- c("LCfit","APCfit","RHfit","CBDfit","CBD_binfit","M7fit","M7_binfit","PLATfit")

# Cf. Villegas petit echantillon effet cohorte deconseillé

Comparaison
