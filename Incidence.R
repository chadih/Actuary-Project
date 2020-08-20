#### Incidence gir 12 ####

#### Récupère le répertoire courant ##### 

library(readxl)
dossier=getwd()
dossierOutils="C:/data/FPL/1255 - Tables d'expérience/0- outils"
#dossierTM=paste(dossierOutils,"/TM",sep="")
dossierOutils="http://www.ressources-actuarielles.net/C1256F13006585B2/0/041c5a177d963c7fc12573b5002387b4/$FILE"
dossierTM="http://www.ressources-actuarielles.net/C1256F13006585B2/0/91C780A7E1385379C1257F8C001FA87E/$FILE"
source(paste(dossierOutils,"\\OutilsSurvie.r",sep=""))
library(survival)



#### On récupère les données et on les traite ##### 

#Lecture de la table
cat("Lecture de la table brute","\n")
data_autonome_gir12 <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Data/Donnée pour les lois/Data loi d'incidence/data_autonome_gir12.xlsx")
t=data_autonome_gir12
t$DateNaissance=as.Date(t$DateNaissance,"%d/%m/%Y")
t$DateEntree=as.Date(t$DateEntree,"%d/%m/%Y")
t$DateSortie=as.Date(t$DateSortie,"%d/%m/%Y")








#### Selection de la periode d'observation #####


#Sélection des enregistrements dans la période d'observation
cat("Sélection des enregistrements","\n")
sDateDebutObservation="01/01/2012"
sDateFinObservation="31/12/2016"
DateDebutObservation=as.Date(sDateDebutObservation,"%d/%m/%Y")
DateFinObservation=as.Date(sDateFinObservation,"%d/%m/%Y")
t$DateEntree=pmax(t$DateEntree,DateDebutObservation)
t=subset(t,(t$DateEntree<=DateFinObservation))
#Suppression des enregistrements avec une date d'entrée antérieure à la naissance
t=subset(t,(t$DateNaissance<=t$DateEntree))




####  Calcul des indicatrice de censure et de non censure #####

t$non_censure[t$Statut=="sinistré"]=1
t$non_censure[!(t$Statut=="sinistré")]=0
#Les individus sinistrés après la date de fin d'observation sont censurés à la date de fin d'observation
t$non_censure[(t$Statut=="sinistré")&(t$DateSortie>DateFinObservation)]=0
t$Statut[(t$Statut=="sinistré")&(t$DateSortie>DateFinObservation)]="autre"
t$DateSortie=pmin(t$DateSortie,DateFinObservation)




#### Calcul des âges #####

t$AgeEntreeJours=as.double(as.numeric(difftime(t$DateEntree,t$DateNaissance,"","days")))
t$AgeSortieJours=as.double(as.numeric(difftime(t$DateSortie,t$DateNaissance,"","days")))
t$AgeSortieJours[is.na(t$AgeSortieJours)==1]=DateFinObservation-t$DateNaissance[is.na(t$AgeSortieJours)==1]
t$AgeEntree=t$AgeEntreeJours/365.25
t$AgeSortie=t$AgeSortieJours/365.25



#### Suppression des âges aberrants #####

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



#### Construction des tableaux de données H et F
t_h=subset(t,(t$Sexe=="M"))
t_f=subset(t,(t$Sexe=="F"))

cat(paste("Proportion d'hommes (sex-ratio) : ",nrow(t_h)/nrow(t)),"\n")




#### Décompte des décès par âge #####

decesVentilesH=ventileDeces(t_h,sDateDebutObservation,sDateFinObservation)
decesVentilesF=ventileDeces(t_f,sDateDebutObservation,sDateFinObservation)
decesVentiles=ventileDeces(t,sDateDebutObservation,sDateFinObservation)



#### Décompte des expositions par âge #####

expoVentilesH=ventileExposition(t_h,sDateDebutObservation,sDateFinObservation)
expoVentilesF=ventileExposition(t_f,sDateDebutObservation,sDateFinObservation)
expoVentiles=ventileExposition(t,sDateDebutObservation,sDateFinObservation)





#### Plage graphique #####

xMin=50
xMax=100
xPlage=c(xMin:xMax)



#### Estimation des taux bruts #####


#### Estimation non paramétrique des probabilités conditionnelles de sortie #####

t_ajust=subset(t,t$AgeEntree<t$AgeSortie)
t_ajust <- data.frame(t_ajust)
#Kaplan-Meier pour la population globale
wH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="kaplan-meier",data=t_ajust,subset=Sexe=="M")
wF=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="kaplan-meier",data=t_ajust,subset=Sexe=="F")
w=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="kaplan-meier",data=t_ajust)

Q_H=getQx(wH,0,121)
Q_F=getQx(wF,0,121)
QX=getQx(w,0,121)

#Flemming-Harrington pour la population globale (Estimateur tirées de l'estimateur Nelson-Aalen)
wH_FH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="fh",data=t_ajust,subset=Sexe=="M")
wF_FH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="fh",data=t_ajust,subset=Sexe=="F")
w_FH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="fh",data=t_ajust)

Q_H_FH=getQx(wH_FH,0,121)
Q_F_FH=getQx(wF_FH,0,121)
QX_FH=getQx(w_FH,0,121)


#Comparaison avec l'estimateur de Hoem pour contrôle
Q_H_Hoem=rowSums(decesVentilesH)/rowSums(expoVentilesH)
Q_H_Hoem[is.na(Q_H_Hoem)]=0
Q_F_Hoem=rowSums(decesVentilesF)/rowSums(expoVentilesF)
Q_F_Hoem[is.na(Q_F_Hoem)]=0
QX_Hoem=rowSums(decesVentiles)/rowSums(expoVentiles)
QX_Hoem[is.na(QX_Hoem)]=0



##### Tracé graphique et comparaison des estimateurs  #####

par(mfrow=c(1,1))

plot(xPlage,Q_F[xPlage+1],xlab="Age",ylab="Qx F",type="l")
lines(xPlage,Q_F_Hoem[xPlage+1],col="red")
lines(xPlage,Q_F_FH[xPlage+1],col="blue")
title("Femmes")
legend("topleft", legend = c("Taux KM","Taux Hoem","Taux FH"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))

plot(xPlage,Q_H[xPlage+1],xlab="Age",ylab="Qx H",type="l")
lines(xPlage,Q_H_Hoem[xPlage+1],col="red")
lines(xPlage,Q_H_FH[xPlage+1],col="blue")
title("Hommes")
legend("topleft", legend = c("Taux KM","Taux Hoem","Taux FH"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))

plot(xPlage,QX[xPlage+1],xlab="Age",ylab="Qx",type="l")
lines(xPlage,QX_Hoem[xPlage+1],col="red")
lines(xPlage,QX_FH[xPlage+1],col="blue")
title("Mixte")
legend("topleft", legend = c("Taux KM","Taux Hoem","Taux FH"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))


#### Comparaison des sexes ####

plot(xPlage,Q_H_FH[xPlage+1],xlab="Age",ylab="Qx H",type="l",col="blue")
lines(xPlage,Q_F_FH[xPlage+1],xlab="Age",ylab="Qx F",type="l",col="red")
lines(xPlage,QX_FH[xPlage+1],xlab="Age",ylab="Qx",type="l")

legend("topleft", legend = c("Taux Mixtes","Taux Femmes","Taux Hommes"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))

plot(0:120,Q_H_FH,xlab="Age",ylab="Qx H",type="l",col="blue")






inc <- createWorkbook()
addWorksheet(inc ,"Hommes",tabColour = 'blue')
addWorksheet(inc ,"Femmes",tabColour = 'red')




#### W_H ####

# Whittaker-Henderson Hommes

library(MortalityTables)


# Construction de la table de mortalité
new_Q_H <- c(rep(0,44),Q_H[45:length(Q_H)])
obsTable <- mortalityTable.period(name = "QX autonomes Hommes" , ages = 0:120, deathProbs = new_Q_H,exposures = rowSums(expoVentilesH))


# Lissage Whittaker Henderson avec différents paramètres
obsTable.smooth = whittaker.mortalityTable(obsTable, lambda = 1/10, d = 2, name.postfix = " smoothed (d=2, lambda=1/10)") 
obsTable.smooth1 = whittaker.mortalityTable(obsTable, lambda = 1, d = 2, name.postfix = " smoothed (d=2, lambda=1)")
obsTable.smooth2 = whittaker.mortalityTable(obsTable, lambda = 1/10, d = 3, name.postfix = " smoothed (d=3, lambda=1/10)") 
plot(obsTable, obsTable.smooth, obsTable.smooth1, title = "Observed death probabilities")


# Tracé graphique des résultats  
plot(deathProbabilities(obsTable),type='l')

lines(deathProbabilities(obsTable.smooth),type='l',col='red')
lines(deathProbabilities(obsTable.smooth1),type='l',col='blue')
lines(deathProbabilities(obsTable.smooth2),type='l',col='green')


res <- data.frame(0:120,rowSums(expoVentilesH),rowSums(decesVentilesH),Q_H,deathProbabilities(obsTable.smooth1))
names(res) <- c('Âge','Expo','deces','Taux bruts Hommes','W_H Hommes')

writeData(inc,sheet='Hommes',x=res,sep=',')


# Whittaker-Henderson Femmes



# Construction de la table de mortalité
obsTable_Femme <- mortalityTable.period(name = "QX autonomes Femmes" , ages = 0:120, deathProbs = Q_F,exposures = rowSums(expoVentilesF))


# Lissage Whittaker Henderson avec différents paramètres
obsTable.smooth_Femme = whittaker.mortalityTable(obsTable_Femme, lambda = 1/10, d = 2, name.postfix = " smoothed (d=2, lambda=1/10)") 
obsTable.smooth1_Femme = whittaker.mortalityTable(obsTable_Femme, lambda = 5, d = 2, name.postfix = " smoothed (d=2, lambda=1)")
obsTable.smooth2_Femme = whittaker.mortalityTable(obsTable_Femme, lambda = 1/10, d = 3, name.postfix = " smoothed (d=3, lambda=1/10)") 
plot(obsTable, obsTable.smooth, obsTable.smooth1, title = "Observed death probabilities")


# Tracé graphique des résultats  
plot(deathProbabilities(obsTable_Femme),type='l')

lines(deathProbabilities(obsTable.smooth_Femme),type='l',col='red')
lines(deathProbabilities(obsTable.smooth1_Femme),type='l',col='blue')
lines(deathProbabilities(obsTable.smooth2_Femme),type='l',col='green')


# On retiens lambda = 1 et d = 2



res <- data.frame(0:120,rowSums(expoVentilesF),rowSums(decesVentilesF),Q_F,deathProbabilities(obsTable.smooth1_Femme))
names(res) <- c('Âge','Expo','deces','Taux bruts Hommes','W_H Hommes')

writeData(inc,sheet='Femmes',x=res,sep=',')



saveWorkbook(inc, file = "I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Autonome vers GIR12.xlsx", overwrite = TRUE)



























#### Incidence GIR34 #####



#### Récupère le répertoire courant ##### 

library(readxl)
dossier=getwd()
dossierOutils="C:/data/FPL/1255 - Tables d'expérience/0- outils"
#dossierTM=paste(dossierOutils,"/TM",sep="")
dossierOutils="http://www.ressources-actuarielles.net/C1256F13006585B2/0/041c5a177d963c7fc12573b5002387b4/$FILE"
dossierTM="http://www.ressources-actuarielles.net/C1256F13006585B2/0/91C780A7E1385379C1257F8C001FA87E/$FILE"
source(paste(dossierOutils,"\\OutilsSurvie.r",sep=""))
library(survival)



#### On récupère les données et on les traite ##### 

#Lecture de la table
cat("Lecture de la table brute","\n")
data_autonome_gir34 <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Data/Donnée pour les lois/Data loi d'incidence/data_autonome_gir34.xlsx")
t=data_autonome_gir34
t$DateNaissance=as.Date(t$DateNaissance,"%d/%m/%Y")
t$DateEntree=as.Date(t$DateEntree,"%d/%m/%Y")
t$DateSortie=as.Date(t$DateSortie,"%d/%m/%Y")




cat("Le nombre d'assuré supprimé pour fausse declaration/ delai de carence est de ",test1-test2)




#### Selection de la periode d'observation #####


#Sélection des enregistrements dans la période d'observation
cat("Sélection des enregistrements","\n")
sDateDebutObservation="01/01/2012"
sDateFinObservation="31/12/2016"
DateDebutObservation=as.Date(sDateDebutObservation,"%d/%m/%Y")
DateFinObservation=as.Date(sDateFinObservation,"%d/%m/%Y")
t$DateEntree=pmax(t$DateEntree,DateDebutObservation)
t=subset(t,(t$DateEntree<=DateFinObservation))
#Suppression des enregistrements avec une date d'entrée antérieure à la naissance
t=subset(t,(t$DateNaissance<=t$DateEntree))




####  Calcul des indicatrice de censure et de non censure #####

t$non_censure[t$Statut=="sinistré"]=1
t$non_censure[!(t$Statut=="sinistré")]=0
#Les individus sinistrés après la date de fin d'observation sont censurés à la date de fin d'observation
t$non_censure[(t$Statut=="sinistré")&(t$DateSortie>DateFinObservation)]=0
t$Statut[(t$Statut=="sinistré")&(t$DateSortie>DateFinObservation)]="autre"
t$DateSortie=pmin(t$DateSortie,DateFinObservation)




#### Calcul des âges #####

t$AgeEntreeJours=as.double(as.numeric(difftime(t$DateEntree,t$DateNaissance,"","days")))
t$AgeSortieJours=as.double(as.numeric(difftime(t$DateSortie,t$DateNaissance,"","days")))
t$AgeSortieJours[is.na(t$AgeSortieJours)==1]=DateFinObservation-t$DateNaissance[is.na(t$AgeSortieJours)==1]
t$AgeEntree=t$AgeEntreeJours/365.25
t$AgeSortie=t$AgeSortieJours/365.25



#### Suppression des âges aberrants #####

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



#### Construction des tableaux de données H et F
t_h=subset(t,(t$Sexe=="M"))
t_f=subset(t,(t$Sexe=="F"))

cat(paste("Proportion d'hommes (sex-ratio) : ",nrow(t_h)/nrow(t)),"\n")




#### Décompte des décès par âge #####

decesVentilesH=ventileDeces(t_h,sDateDebutObservation,sDateFinObservation)
decesVentilesF=ventileDeces(t_f,sDateDebutObservation,sDateFinObservation)
decesVentiles=ventileDeces(t,sDateDebutObservation,sDateFinObservation)



#### Décompte des expositions par âge #####

expoVentilesH=ventileExposition(t_h,sDateDebutObservation,sDateFinObservation)
expoVentilesF=ventileExposition(t_f,sDateDebutObservation,sDateFinObservation)
expoVentiles=ventileExposition(t,sDateDebutObservation,sDateFinObservation)





#### Plage graphique #####

xMin=50
xMax=100
xPlage=c(xMin:xMax)



#### Estimation des taux bruts #####


#### Estimation non paramétrique des probabilités conditionnelles de sortie #####

t_ajust=subset(t,t$AgeEntree<t$AgeSortie)
t_ajust <- data.frame(t_ajust)
#Kaplan-Meier pour la population globale
wH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="kaplan-meier",data=t_ajust,subset=Sexe=="M")
wF=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="kaplan-meier",data=t_ajust,subset=Sexe=="F")
w=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="kaplan-meier",data=t_ajust)

Q_H=getQx(wH,0,121)
Q_F=getQx(wF,0,121)
QX=getQx(w,0,121)

#Flemming-Harrington pour la population globale (Estimateur tirées de l'estimateur Nelson-Aalen)
wH_FH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="fh",data=t_ajust,subset=Sexe=="M")
wF_FH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="fh",data=t_ajust,subset=Sexe=="F")
w_FH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="fh",data=t_ajust)

Q_H_FH=getQx(wH_FH,0,121)
Q_F_FH=getQx(wF_FH,0,121)
QX_FH=getQx(w_FH,0,121)


#Comparaison avec l'estimateur de Hoem pour contrôle
Q_H_Hoem=rowSums(decesVentilesH)/rowSums(expoVentilesH)
Q_H_Hoem[is.na(Q_H_Hoem)]=0
Q_F_Hoem=rowSums(decesVentilesF)/rowSums(expoVentilesF)
Q_F_Hoem[is.na(Q_F_Hoem)]=0
QX_Hoem=rowSums(decesVentiles)/rowSums(expoVentiles)
QX_Hoem[is.na(QX_Hoem)]=0



##### Tracé graphique et comparaison des estimateurs  #####

par(mfrow=c(1,1))

plot(xPlage,Q_F[xPlage+1],xlab="Age",ylab="Qx F",type="l")
lines(xPlage,Q_F_Hoem[xPlage+1],col="red")
lines(xPlage,Q_F_FH[xPlage+1],col="blue")
title("Femmes")
legend("topleft", legend = c("Taux KM","Taux Hoem","Taux FH"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))

plot(xPlage,Q_H[xPlage+1],xlab="Age",ylab="Qx H",type="l")
lines(xPlage,Q_H_Hoem[xPlage+1],col="red")
lines(xPlage,Q_H_FH[xPlage+1],col="blue")
title("Hommes")
legend("topleft", legend = c("Taux KM","Taux Hoem","Taux FH"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))

plot(xPlage,QX[xPlage+1],xlab="Age",ylab="Qx",type="l")
lines(xPlage,QX_Hoem[xPlage+1],col="red")
lines(xPlage,QX_FH[xPlage+1],col="blue")
title("Mixte")
legend("topleft", legend = c("Taux KM","Taux Hoem","Taux FH"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))


#### Comparaison des sexes ####

plot(xPlage,Q_H_FH[xPlage+1],xlab="Age",ylab="Qx H",type="l",col="blue")
lines(xPlage,Q_F_FH[xPlage+1],xlab="Age",ylab="Qx F",type="l",col="red")
lines(xPlage,QX_FH[xPlage+1],xlab="Age",ylab="Qx",type="l")

legend("topleft", legend = c("Taux Mixtes","Taux Femmes","Taux Hommes"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))

plot(0:120,Q_H_FH,xlab="Age",ylab="Qx H",type="l",col="blue")






inc34 <- createWorkbook()
addWorksheet(inc34 ,"Hommes",tabColour = 'blue')
addWorksheet(inc34 ,"Femmes",tabColour = 'red')


#### W_H ####

# Whittaker-Henderson Hommes

library(MortalityTables)


# Construction de la table de mortalité
new_Q_H <- c(rep(0,55),Q_H[56:length(Q_H)])
obsTable <- mortalityTable.period(name = "QX autonomes Hommes" , ages = 0:120, deathProbs = new_Q_H,exposures = rowSums(expoVentilesH))


# Lissage Whittaker Henderson avec différents paramètres
obsTable.smooth = whittaker.mortalityTable(obsTable, lambda = 1/10, d = 2, name.postfix = " smoothed (d=2, lambda=1/10)") 
obsTable.smooth1 = whittaker.mortalityTable(obsTable, lambda = 1, d = 2, name.postfix = " smoothed (d=2, lambda=1)")
obsTable.smooth2 = whittaker.mortalityTable(obsTable, lambda = 1/10, d = 3, name.postfix = " smoothed (d=3, lambda=1/10)") 
plot(obsTable, obsTable.smooth, obsTable.smooth1, title = "Observed death probabilities")


# Tracé graphique des résultats  
plot(deathProbabilities(obsTable),type='l')

lines(deathProbabilities(obsTable.smooth),type='l',col='red')
lines(deathProbabilities(obsTable.smooth1),type='l',col='blue')
lines(deathProbabilities(obsTable.smooth2),type='l',col='green')


res <- data.frame(0:120,rowSums(expoVentilesH),rowSums(decesVentilesH),Q_H,deathProbabilities(obsTable.smooth1))
names(res) <- c('Âge','Expo','deces','Taux bruts Hommes','W_H Hommes')

writeData(inc34,sheet='Hommes',x=res,sep=',')


# Whittaker-Henderson Femmes




# Construction de la table de mortalité
new_Q_F <- c(rep(0,49),Q_F[50:length(Q_F)])
obsTable_Femme <- mortalityTable.period(name = "QX autonomes Femmes" , ages = 0:120, deathProbs = new_Q_F,exposures = rowSums(expoVentilesF))


# Lissage Whittaker Henderson avec différents paramètres
obsTable.smooth_Femme = whittaker.mortalityTable(obsTable_Femme, lambda = 1/10, d = 2, name.postfix = " smoothed (d=2, lambda=1/10)") 
obsTable.smooth1_Femme = whittaker.mortalityTable(obsTable_Femme, lambda = 5, d = 2, name.postfix = " smoothed (d=2, lambda=1)")
obsTable.smooth2_Femme = whittaker.mortalityTable(obsTable_Femme, lambda = 1/10, d = 3, name.postfix = " smoothed (d=3, lambda=1/10)") 
plot(obsTable, obsTable.smooth, obsTable.smooth1, title = "Observed death probabilities")


# Tracé graphique des résultats  
plot(deathProbabilities(obsTable_Femme),type='l')

lines(deathProbabilities(obsTable.smooth_Femme),type='l',col='red')
lines(deathProbabilities(obsTable.smooth1_Femme),type='l',col='blue')
lines(deathProbabilities(obsTable.smooth2_Femme),type='l',col='green')


# On retiens lambda = 1 et d = 2


res <- data.frame(0:120,rowSums(expoVentilesF),rowSums(decesVentilesF),Q_F,deathProbabilities(obsTable.smooth1_Femme))
names(res) <- c('Âge','Expo','deces','Taux bruts Hommes','W_H Hommes')

writeData(inc34,sheet='Femmes',x=res,sep=',')






saveWorkbook(inc34, file = "I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Autonome vers GIR34.xlsx", overwrite = TRUE)


















#### Incidence GIR5 ##### 



#### Récupère le répertoire courant ##### 

library(readxl)
dossier=getwd()
dossierOutils="C:/data/FPL/1255 - Tables d'expérience/0- outils"
#dossierTM=paste(dossierOutils,"/TM",sep="")
dossierOutils="http://www.ressources-actuarielles.net/C1256F13006585B2/0/041c5a177d963c7fc12573b5002387b4/$FILE"
dossierTM="http://www.ressources-actuarielles.net/C1256F13006585B2/0/91C780A7E1385379C1257F8C001FA87E/$FILE"
source(paste(dossierOutils,"\\OutilsSurvie.r",sep=""))
library(survival)



#### On récupère les données et on les traite ##### 

#Lecture de la table
cat("Lecture de la table brute","\n")
data_autonome_gir5 <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Data/Donnée pour les lois/Data loi d'incidence/data_autonome_gir5.xlsx")
t=data_autonome_gir5
t$DateNaissance=as.Date(t$DateNaissance,"%d/%m/%Y")
t$DateEntree=as.Date(t$DateEntree,"%d/%m/%Y")
t$DateSortie=as.Date(t$DateSortie,"%d/%m/%Y")








#### Selection de la periode d'observation #####


#Sélection des enregistrements dans la période d'observation
cat("Sélection des enregistrements","\n")
sDateDebutObservation="01/01/2012"
sDateFinObservation="31/12/2016"
DateDebutObservation=as.Date(sDateDebutObservation,"%d/%m/%Y")
DateFinObservation=as.Date(sDateFinObservation,"%d/%m/%Y")
t$DateEntree=pmax(t$DateEntree,DateDebutObservation)
t=subset(t,(t$DateEntree<=DateFinObservation))
#Suppression des enregistrements avec une date d'entrée antérieure à la naissance
t=subset(t,(t$DateNaissance<=t$DateEntree))




####  Calcul des indicatrice de censure et de non censure #####

t$non_censure[t$Statut=="sinistré"]=1
t$non_censure[!(t$Statut=="sinistré")]=0
#Les individus sinistrés après la date de fin d'observation sont censurés à la date de fin d'observation
t$non_censure[(t$Statut=="sinistré")&(t$DateSortie>DateFinObservation)]=0
t$Statut[(t$Statut=="sinistré")&(t$DateSortie>DateFinObservation)]="autre"
t$DateSortie=pmin(t$DateSortie,DateFinObservation)




#### Calcul des âges #####

t$AgeEntreeJours=as.double(as.numeric(difftime(t$DateEntree,t$DateNaissance,"","days")))
t$AgeSortieJours=as.double(as.numeric(difftime(t$DateSortie,t$DateNaissance,"","days")))
t$AgeSortieJours[is.na(t$AgeSortieJours)==1]=DateFinObservation-t$DateNaissance[is.na(t$AgeSortieJours)==1]
t$AgeEntree=t$AgeEntreeJours/365.25
t$AgeSortie=t$AgeSortieJours/365.25



#### Suppression des âges aberrants #####

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



#### Construction des tableaux de données H et F
t_h=subset(t,(t$Sexe=="M"))
t_f=subset(t,(t$Sexe=="F"))

cat(paste("Proportion d'hommes (sex-ratio) : ",nrow(t_h)/nrow(t)),"\n")




#### Décompte des décès par âge #####

decesVentilesH=ventileDeces(t_h,sDateDebutObservation,sDateFinObservation)
decesVentilesF=ventileDeces(t_f,sDateDebutObservation,sDateFinObservation)
decesVentiles=ventileDeces(t,sDateDebutObservation,sDateFinObservation)



#### Décompte des expositions par âge #####

expoVentilesH=ventileExposition(t_h,sDateDebutObservation,sDateFinObservation)
expoVentilesF=ventileExposition(t_f,sDateDebutObservation,sDateFinObservation)
expoVentiles=ventileExposition(t,sDateDebutObservation,sDateFinObservation)





#### Plage graphique #####

xMin=50
xMax=100
xPlage=c(xMin:xMax)



#### Estimation des taux bruts #####


#### Estimation non paramétrique des probabilités conditionnelles de sortie #####

t_ajust=subset(t,t$AgeEntree<t$AgeSortie)
t_ajust <- data.frame(t_ajust)
#Kaplan-Meier pour la population globale
wH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="kaplan-meier",data=t_ajust,subset=Sexe=="M")
wF=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="kaplan-meier",data=t_ajust,subset=Sexe=="F")
w=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="kaplan-meier",data=t_ajust)

Q_H=getQx(wH,0,121)
Q_F=getQx(wF,0,121)
QX=getQx(w,0,121)

#Flemming-Harrington pour la population globale (Estimateur tirées de l'estimateur Nelson-Aalen)
wH_FH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="fh",data=t_ajust,subset=Sexe=="M")
wF_FH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="fh",data=t_ajust,subset=Sexe=="F")
w_FH=survfit(Surv(AgeEntree,AgeSortie,non_censure,type="counting")~1,type="fh",data=t_ajust)

Q_H_FH=getQx(wH_FH,0,121)
Q_F_FH=getQx(wF_FH,0,121)
QX_FH=getQx(w_FH,0,121)


#Comparaison avec l'estimateur de Hoem pour contrôle
Q_H_Hoem=rowSums(decesVentilesH)/rowSums(expoVentilesH)
Q_H_Hoem[is.na(Q_H_Hoem)]=0
Q_F_Hoem=rowSums(decesVentilesF)/rowSums(expoVentilesF)
Q_F_Hoem[is.na(Q_F_Hoem)]=0
QX_Hoem=rowSums(decesVentiles)/rowSums(expoVentiles)
QX_Hoem[is.na(QX_Hoem)]=0



##### Tracé graphique et comparaison des estimateurs  #####

par(mfrow=c(1,1))

plot(xPlage,Q_F[xPlage+1],xlab="Age",ylab="Qx F",type="l")
lines(xPlage,Q_F_Hoem[xPlage+1],col="red")
lines(xPlage,Q_F_FH[xPlage+1],col="blue")
title("Femmes")
legend("topleft", legend = c("Taux KM","Taux Hoem","Taux FH"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))

plot(xPlage,Q_H[xPlage+1],xlab="Age",ylab="Qx H",type="l")
lines(xPlage,Q_H_Hoem[xPlage+1],col="red")
lines(xPlage,Q_H_FH[xPlage+1],col="blue")
title("Hommes")
legend("topleft", legend = c("Taux KM","Taux Hoem","Taux FH"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))

plot(xPlage,QX[xPlage+1],xlab="Age",ylab="Qx",type="l")
lines(xPlage,QX_Hoem[xPlage+1],col="red")
lines(xPlage,QX_FH[xPlage+1],col="blue")
title("Mixte")
legend("topleft", legend = c("Taux KM","Taux Hoem","Taux FH"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))


#### Comparaison des sexes ####

plot(xPlage,Q_H_FH[xPlage+1],xlab="Age",ylab="Qx H",type="l",col="blue")
lines(xPlage,Q_F_FH[xPlage+1],xlab="Age",ylab="Qx F",type="l",col="red")
lines(xPlage,QX_FH[xPlage+1],xlab="Age",ylab="Qx",type="l")

legend("topleft", legend = c("Taux Mixtes","Taux Femmes","Taux Hommes"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))

plot(0:120,Q_H_FH,xlab="Age",ylab="Qx H",type="l",col="blue")










inc5 <- createWorkbook()
addWorksheet(inc5 ,"Hommes",tabColour = 'blue')
addWorksheet(inc5 ,"Femmes",tabColour = 'red')


#### W_H ####

# Whittaker-Henderson Hommes

library(MortalityTables)


# Construction de la table de mortalité
new_Q_H <- c(Q_H_Hoem[1:90],rep(0,31))
obsTable <- mortalityTable.period(name = "QX autonomes Hommes" , ages = 0:120, deathProbs = new_Q_H,exposures = rowSums(expoVentilesH))


# Lissage Whittaker Henderson avec différents paramètres
obsTable.smooth = whittaker.mortalityTable(obsTable, lambda = 1/10, d = 2, name.postfix = " smoothed (d=2, lambda=1/10)") 
obsTable.smooth1 = whittaker.mortalityTable(obsTable, lambda = 1, d = 2, name.postfix = " smoothed (d=2, lambda=1)")
obsTable.smooth2 = whittaker.mortalityTable(obsTable, lambda = 1/10, d = 3, name.postfix = " smoothed (d=3, lambda=1/10)") 
plot(obsTable, obsTable.smooth, obsTable.smooth1, title = "Observed death probabilities")


# Tracé graphique des résultats  
plot(deathProbabilities(obsTable),type='l')

lines(deathProbabilities(obsTable.smooth),type='l',col='red')
lines(deathProbabilities(obsTable.smooth1),type='l',col='blue')
lines(deathProbabilities(obsTable.smooth2),type='l',col='green')


res <- data.frame(0:120,rowSums(expoVentilesH),rowSums(decesVentilesH),Q_H,deathProbabilities(obsTable.smooth1))
names(res) <- c('Âge','Expo','deces','Taux bruts Hommes','W_H Hommes')

writeData(inc5,sheet='Hommes',x=res,sep=',')


# Whittaker-Henderson Femmes




# Construction de la table de mortalité
new_Q_F <- c(rep(0,52),Q_F_Hoem[53:length(Q_F_Hoem)])
obsTable_Femme <- mortalityTable.period(name = "QX autonomes Femmes" , ages = 0:120, deathProbs = new_Q_F,exposures = rowSums(expoVentilesF))


# Lissage Whittaker Henderson avec différents paramètres
obsTable.smooth_Femme = whittaker.mortalityTable(obsTable_Femme, lambda = 1/10, d = 2, name.postfix = " smoothed (d=2, lambda=1/10)") 
obsTable.smooth1_Femme = whittaker.mortalityTable(obsTable_Femme, lambda = 5, d = 2, name.postfix = " smoothed (d=2, lambda=1)")
obsTable.smooth2_Femme = whittaker.mortalityTable(obsTable_Femme, lambda = 1/10, d = 3, name.postfix = " smoothed (d=3, lambda=1/10)") 
plot(obsTable, obsTable.smooth, obsTable.smooth1, title = "Observed death probabilities")


# Tracé graphique des résultats  
plot(deathProbabilities(obsTable_Femme),type='l')

lines(deathProbabilities(obsTable.smooth_Femme),type='l',col='red')
lines(deathProbabilities(obsTable.smooth1_Femme),type='l',col='blue')
lines(deathProbabilities(obsTable.smooth2_Femme),type='l',col='green')


# On retiens lambda = 1 et d = 2


res <- data.frame(0:120,rowSums(expoVentilesF),rowSums(decesVentilesF),Q_F,deathProbabilities(obsTable.smooth1_Femme))
names(res) <- c('Âge','Expo','deces','Taux bruts Femmes','W_H Femmes')

writeData(inc5,sheet='Femmes',x=res,sep=',')






saveWorkbook(inc5, file = "I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Autonome vers GIR5.xlsx", overwrite = TRUE)
