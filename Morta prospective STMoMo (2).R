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




#### STMoMo Lee Carter Homme #############################

# Package
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






# Possibilite d'implémenter des modèle ou d'utiliser les deja existant 
# APC <- apc() modèle âge période cohorte
# Il existe d'autres modèle voir StMoMo documentation


# Fitting du modèle ( regression non linèaire voir Charpentier pour plus d'info)
LCfit <- fit(LC, Dxt= Dxt_homme_prosp[40:87,], Ext=Ext_homme_prosp[40:87,],ages=41:88 ,years=2000:2016)

par(mfrow=c(1,1))
plot(LCfit,type='o')

plot(40:86,exp(LCfit$ax[1:47]),xlab = "âge",ylab = "mortality rate") # forme mortalité exp alpha






# Goodness of fit ( Residus )
LCres <- residuals(LCfit)


plot(LCres, type = "colourmap", reslim = c(-3.5, 3.5)) # Heat Map pour visualiser l'evolution des qx








# Prevision du Kt horizon 50 ans (h=50)
LCfor <- forecast(LCfit, h=50,kt.method = "mrwd") 

plot(LCfor, parametricbx = FALSE)





# Simulation de trajectoire pour le Kt horizon 50 ans et 500 simulation
LCsim <- simulate(LCfit, nsim=500, h=50)


plot(LCfit$years, LCfit$kt[1,],
     xlim=c(2000,2066), ylim=c(-65,65),
     type="l", xlab="year", ylab="kt",
     main="Period index (LC)") # Tracé du Kt deja existant

matlines(LCsim$kt.s$years, LCsim$kt.s$sim[1,,1:20],
         type="l", lty=1) # Tracé  des nsim projections de Kt







# Projection du qx pour un âge fixé 
library(fanplot)


plot(LCfit$years, (Dxt_homme_prosp/Ext_homme_prosp)["75",], xlim=c(2000,2060),
     ylim=c(0.0025,0.05), pch =20, log="y",
     xlab="year", ylab="q(65,t) (log scale)")
fan(t(LCsim$rates["75",,]), start=2016,
    probs=c(2.5,10,25,50,75,90,97.5), n.fan=4, ln=NULL,
    fan.col=colorRampPalette(c("black","white")))

     


























#### Fitting du modèle en supprimant les années de 2000 à 2002 Homme ############################################################





# Fitting du modèle ( regression non linèaire voir Charpentier pour plus d'info)
LCfit <- fit(LC, Dxt= Dxt_homme_prosp[40:87,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[40:87,4:length(Dxt_homme_prosp)],ages=41:88 ,years=2003:2016)



par(mfrow=c(1,1))
plot(LCfit,type='o')

library(rgl)
couleur=colorRampPalette(c("blue","yellow","red")) 
Mmodele=exp(as.vector(LCfit$ax)+as.vector(LCfit$bx)%*%t( as.vector(LCfit$kt[1,] )))
qxLC <- fitted(LCfit, type = "rates")
persp3d(41:86,2003:2016,(Mmodele[1:46,]),theta=-20,phi=20,xlab='age' ,ylab='annee',zlab='ln(mu)' ,ticktype = "detailed",border = "black",col="light blue" )
persp3d(41:86,2003:2016,(qxLC[1:46,]),theta=-20,phi=20,xlab='age' ,ylab='annee',zlab='ln(mu)' ,ticktype = "detailed",border = "black",col="light blue" )






# Goodness of fit ( Residus )
LCres <- residuals(LCfit)


plot(LCres, type = "colourmap", reslim = c(-3, 3)) # Heat Map pour visualiser l'evolution des qx




# Tracé des residus du modèle
plot(41:88,LCres$residuals[,1],ylim = c(-3,3),ylab = "residus",xlab='âge')
for(i in 2:length(LCres$residuals)){
  lines(41:88,LCres$residuals[,i],type='p')
}

library(lmtest)
shapiro.test(LCres$residuals)
abc <- LCres$residuals





# Prevision du Kt horizon 50 ans (h=50)
LCfor <- forecast(LCfit, h=50) 




plot(LCfor, parametricbx = FALSE)


plot(2017:2066,LCfor$kt.f$mean)
lines(2017:2066,LCfor$kt.f$lower[,,1],col='red')
lines(2017:2066,LCfor$kt.f$lower[,,2],col='blue')
lines(2017:2066,LCfor$kt.f$upper[,,1],col='red')
lines(2017:2066,LCfor$kt.f$upper[,,2],col='blue')





# Predciton de la table de mortalité

Pred <- predict(LCfit,years=2017:2066,kt=LCfor$kt.f$mean,type = "rates")



persp3d(41:86,2017:2066,Pred[1:46,],theta=-20,phi=20,xlab='age' ,ylab='annee',zlab='ln(mu)' ,ticktype = "detailed",border = "black",col="light blue" )

qxLC <- fitted(LCfit, type = "rates")

matplot(2003 : 2066,
        t(cbind(qxLC[1:45,],Pred[1:45,])[c("69", "75", "80"), ]),
        type = "l", col = "black", xlab = "years", ylab = "rates",
        lwd = 1.5)

text(2006,0.015,"x=69",cex=0.69,col="red")
text(2006,0.026,"x=75",cex=0.69,col="red")
text(2006,0.041,"x=80",cex=0.69,col="red")

# certains qx croit avec les années car Beta negatif
LCfit$bxLCfit$bx
LCfit$bx[which(LCfit<0)]
matplot(2003 : 2066,
        t(cbind(qxLC[1:45,],Pred[1:45,])[c("63", "75", "80"), ]),
        type = "l", col = "black", xlab = "years", ylab = "rates",
        lwd = 1.5)

text(2006,0.008,"x=63",cex=0.69,col="red")
text(2006,0.026,"x=75",cex=0.69,col="red")
text(2006,0.041,"x=80",cex=0.69,col="red")




# Simulation de trajectoire pour le Kt horizon 50 ans et 500 simulation
LCsim <- simulate(LCfit, nsim=500, h=50)


plot(LCfit$years, LCfit$kt[1,],
     xlim=c(2003,2066), ylim=c(-65,65),
     type="l", xlab="year", ylab="kt",
     main="Period index (LC)") # Tracé du Kt deja existant

matlines(LCsim$kt.s$years, LCsim$kt.s$sim[1,,1:20],
         type="l", lty=1) # Tracé  des nsim projections de Kt





# Projection du qx pour un âge fixé 
library(fanplot)


plot(LCfit$years, (Dxt_homme_prosp[,4:length(Dxt_homme_prosp)]/Ext_homme_prosp[,4:length(Dxt_homme_prosp)])["75",], xlim=c(2003,2060),
     ylim=c(0.0025,0.15), pch =20, log="y",
     xlab="year", ylab="q(75,t) (log scale)",type='l')

fan(t(LCsim$rates["75",,]), start=2016,
    probs=c(2.5,10,25,50,75,90,97.5), n.fan=4, ln=NULL,
    fan.col=colorRampPalette(c("black","white")))


lines(LCfit$years, (Dxt_homme_prosp[,4:length(Dxt_homme_prosp)]/Ext_homme_prosp[,4:length(Dxt_homme_prosp)])["60",], xlim=c(2003,2060),
     ylim=c(0.0025,0.05), pch =20, log="y",
     xlab="year", ylab="q(75,t) (log scale)",type='l')

fan(t(LCsim$rates["66",,]), start=2016,
    probs=c(2.5,10,25,50,75,90,97.5), n.fan=4, ln=NULL,
    fan.col=colorRampPalette(c("red","white")))























#### Fitting du modèle en supprimant les années de 2000 à 2002 et en posant beta = 1/length(age) tous les âges Homme ############################################################



LC_bis <- StMoMo(link = "log", staticAgeFun = TRUE, periodAgeFun = "1", constFun = constLC)

# LC avec beta= 1/length(41:86) du coup pas de beta negatif
LCfit_bis <- fit(LC_bis, Dxt= Dxt_homme_prosp[,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[,4:length(Dxt_homme_prosp)],ages=0:120,ages.fit =0:120 ,years=2003:2016)
plot(LCfit_bis,type='o')




# Goodness of fit ( Residus )
LCres_bis <- residuals(LCfit_bis)


plot(LCres_bis, type = "colourmap", reslim = c(-3, 3)) # Heat Map pour visualiser l'evolution des qx




# Tracé des residus du modèle
plot(41:88,LCres_bis$residuals[,1],ylim = c(-3,3),ylab = "residus",xlab='âge')
for(i in 2:length(LCres_bis$residuals)){
  lines(41:88,LCres_bis$residuals[,i],type='p')
}

library(lmtest)
shapiro.test(LCres_bis$residuals)
abc <- LCres_bis$residuals





# Prevision du Kt horizon 50 ans (h=50)



LCfor_bis <- forecast(LCfit_bis, h=50) 
plot(LCfor_bis, parametricbx = FALSE)
plot(2017:2066,LCfor_bis$kt.f$mean)
lines(2017:2066,LCfor_bis$kt.f$lower[,,1],col='red')
lines(2017:2066,LCfor_bis$kt.f$lower[,,2],col='blue')
lines(2017:2066,LCfor_bis$kt.f$upper[,,1],col='red')
lines(2017:2066,LCfor_bis$kt.f$upper[,,2],col='blue')

# Predciton de la table de mortalité
Pred_bis <- predict(LCfit_bis,years=2017:2066,kt=LCfor_bis$kt.f$mean,type = "rates")

library(rgl)

persp3d(40:85,2017:2066,Pred_bis[41:86,],theta=-20,phi=20,xlab='age' ,ylab='annee',zlab='ln(mu)' ,ticktype = "detailed",border = "black",col="light blue" )

persp3d(0:120,2017:2066,Pred_bis,theta=-20,phi=20,xlab='age' ,ylab='annee',zlab='ln(mu)' ,ticktype = "detailed",border = "black",col="light blue" )


qxLC_bis <- fitted(LCfit_bis, type = "rates")



matplot(2003 : 2066,
        t(cbind(qxLC_bis,Pred_bis)[c("69", "75", "80"), ]),
        type = "l", col = "black", xlab = "years", ylab = "rates",
        lwd = 1.5)

text(2006,0.014,"x=69",cex=0.69,col="red")
text(2006,0.024,"x=75",cex=0.69,col="red")
text(2006,0.037,"x=80",cex=0.69,col="red")


# Evolution de tous les Qx cohérentes
matplot(2003 : 2066,
        t(cbind(qxLC_bis[1:45,],Pred_bis[1:45,])),
        type = "l", col = "black", xlab = "years", ylab = "rates",
        lwd = 1.5)



# Simulation de trajectoire pour le Kt horizon 50 ans et 500 simulation
LCsim_bis <- simulate(LCfit_bis, nsim=500, h=50)


plot(LCfit_bis$years, LCfit_bis$kt[1,],
     xlim=c(2003,2066), ylim=c(-65,65),
     type="l", xlab="year", ylab="kt",
     main="Period index (LC)") # Tracé du Kt deja existant

matlines(LCsim_bis$kt.s$years, LCsim_bis$kt.s$sim[1,,1:20],
         type="l", lty=1) # Tracé  des nsim projections de Kt





# Projection du qx pour un âge fixé 
library(fanplot)


plot(LCfit_bis$years, (Dxt_homme_prosp[,4:length(Dxt_homme_prosp)]/Ext_homme_prosp[,4:length(Dxt_homme_prosp)])["75",], xlim=c(2003,2060),
     ylim=c(0.0025,0.15), pch =20, log="y",
     xlab="year", ylab="q(75,t) (log scale)",type='l')

fan(t(LCsim_bis$rates["75",,]), start=2016,
    probs=c(2.5,10,25,50,75,90,97.5), n.fan=4, ln=NULL,
    fan.col=colorRampPalette(c("black","white")))






# Export du modele de Lee Carter 

res <- data.frame(qxLC_bis,Pred_bis)

write.csv(res,file = "C:/Users/hhj/Desktop/Table Prospective Lee Carter.csv")
























#### Fitting du modèle en supprimant les années de 2000 à 2002 et en posant beta = 1/length(age) tous les âges Femme ############################################################

library(StMoMo)

# définition de la fonction de contrainte pour le modèle de Lee Carter
constLC <- function(ax, bx, kt, b0x, gc, wxt, ages) {
  c1 <- mean(kt[1, ], na.rm = TRUE) 
  c2 <- sum(bx[, 1], na.rm = TRUE) 
  list(ax = ax + c1 * bx, bx = bx / c2, kt = c2 * (kt - c1)) 
} 

LC_bis <- StMoMo(link = "log", staticAgeFun = TRUE, periodAgeFun = "1", constFun = constLC)

# LC avec beta= 1/length(41:86) du coup pas de beta negatif
LCfit_bis_femme <- fit(LC_bis, Dxt= Dxt_femme_prosp[,4:length(Dxt_femme_prosp)], Ext=Ext_femme_prosp[,4:length(Dxt_homme_prosp)],ages=0:120,ages.fit =0:120 ,years=2003:2016)
plot(LCfit_bis_femme,type='o')




# Goodness of fit ( Residus )
LCres_bis_femme <- residuals(LCfit_bis_femme)


plot(LCres_bis_femme, type = "colourmap", reslim = c(-3, 3)) # Heat Map pour visualiser l'evolution des qx




# Tracé des residus du modèle
plot(41:88,LCres_bis_femme$residuals[,1],ylim = c(-3,3),ylab = "residus",xlab='âge')
for(i in 2:length(LCres_bis_femme$residuals)){
  lines(41:88,LCres_bis_femme$residuals[,i],type='p')
}

library(lmtest)
shapiro.test(LCres_bis_femme$residuals)






# Prevision du Kt horizon 50 ans (h=50)



LCfor_bis_femme <- forecast(LCfit_bis_femme, h=50) 
plot(LCfor_bis_femme, parametricbx = FALSE)
plot(2017:2066,LCfor_bis$kt.f$mean)
lines(2017:2066,LCfor_bis_femme$kt.f$lower[,,1],col='red')
lines(2017:2066,LCfor_bis_femme$kt.f$lower[,,2],col='blue')
lines(2017:2066,LCfor_bis_femme$kt.f$upper[,,1],col='red')
lines(2017:2066,LCfor_bis_femme$kt.f$upper[,,2],col='blue')

# Predciton de la table de mortalité
Pred_bis_femme <- predict(LCfit_bis_femme,years=2017:2066,kt=LCfor_bis_femme$kt.f$mean,type = "rates")

library(rgl)

persp3d(40:85,2017:2066,Pred_bis_femme[41:86,],theta=-20,phi=20,xlab='age' ,ylab='annee',zlab='ln(mu)' ,ticktype = "detailed",border = "black",col="light blue" )

persp3d(0:120,2017:2066,Pred_bis_femme,theta=-20,phi=20,xlab='age' ,ylab='annee',zlab='ln(mu)' ,ticktype = "detailed",border = "black",col="light blue" )


qxLC_bis_femme <- fitted(LCfit_bis_femme, type = "rates")



matplot(2003 : 2066,
        t(cbind(qxLC_bis_femme,Pred_bis_femme)[c("69", "75", "80"), ]),
        type = "l", col = "black", xlab = "years", ylab = "rates",
        lwd = 1.5)

text(2006,0.014,"x=69",cex=0.69,col="red")
text(2006,0.024,"x=75",cex=0.69,col="red")
text(2006,0.037,"x=80",cex=0.69,col="red")


# Evolution de tous les Qx cohérentes
matplot(2003 : 2066,
        t(cbind(qxLC_bis_femme[1:45,],Pred_bis_femme[1:45,])),
        type = "l", col = "black", xlab = "years", ylab = "rates",
        lwd = 1.5)



# Simulation de trajectoire pour le Kt horizon 50 ans et 500 simulation
LCsim_bis_femme <- simulate(LCfit_bis_femme, nsim=500, h=50)


plot(LCfit_bis_femme$years, LCfit_bis_femme$kt[1,],
     xlim=c(2003,2066), ylim=c(-65,65),
     type="l", xlab="year", ylab="kt",
     main="Period index (LC)") # Tracé du Kt deja existant

matlines(LCsim_bis$kt.s$years, LCsim_bis$kt.s$sim[1,,1:20],
         type="l", lty=1) # Tracé  des nsim projections de Kt





# Projection du qx pour un âge fixé 
library(fanplot)


plot(LCfit_bis_femme$years, (Dxt_femme_prosp[,4:length(Dxt_femme_prosp)]/Ext_femme_prosp[,4:length(Dxt_femme_prosp)])["75",], xlim=c(2003,2060),
     ylim=c(0.0025,0.15), pch =20, log="y",
     xlab="year", ylab="q(75,t) (log scale)",type='l')

fan(t(LCsim_bis_femme$rates["75",,]), start=2016,
    probs=c(2.5,10,25,50,75,90,97.5), n.fan=4, ln=NULL,
    fan.col=colorRampPalette(c("black","white")))






# Export du modele de Lee Carter 

res_femme <- data.frame(qxLC_bis_femme,Pred_bis_femme)

write.csv(res,file = "C:/Users/hhj/Desktop/Table Prospective Femme Lee Carter.csv")























#### STMoMo APC #######################



# Spécification du modèle ( deux facons à la main ou modèle deja existant dans le package)

APC <- apc()

# Possibilite d'implémenter des modèle ou d'utiliser les deja existant   
# APC <- apc() modèle âge période cohorte 
# Il existe d'autres modèle voir StMoMo documentation 


# Fitting du modèle ( regression non linèaire voir Charpentier pour plus d'info)
APCfit <- fit(APC, Dxt= Dxt_homme_prosp[,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[,4:length(Ext_homme_prosp)],ages=0:120 ,years=2003:2016)

par(mfrow=c(1,1))
plot(APCfit,type='o')

qxAPC <- fitted(APCfit, type = "rates")
persp3d(0:120,2003:2016,(qxAPC),theta=-20,phi=20,xlab='age' ,ylab='annee',zlab='ln(mu)' ,ticktype = "detailed",border = "black",col="light blue" )



# Goodness of fit ( Residus )
APCres <- residuals(APCfit)


plot(APCres, type = "colourmap", reslim = c(-3.5, 3.5)) # Heat Map pour visualiser l'evolution des qx


# Prevision du Kt horizon 50 ans (h=50)
APCfor <- forecast(APCfit, h=50) 

plot(APCfor, parametricbx = FALSE)






# Simulation de trajectoire pour le Kt horizon 50 ans et 500 simulation
APCsim <- simulate(APCfit, nsim=500, h=50)


plot(APCfit$years, APCfit$kt[1,],
     xlim=c(2000,2066), ylim=c(-3,1),
     type="l", xlab="year", ylab="kt",
     main="Period index (APC)") # Tracé du Kt deja existant

matlines(APCsim$kt.s$years, APCsim$kt.s$sim[1,,1:20],
         type="l", lty=1) # Tracé  des nsim projections de Kt



# Projection du qx pour un âge fixé 
library(fanplot)


plot(APCfit$years, (Dxt_homme_prosp/Ext_homme_prosp)["75",4:17], xlim=c(2003,2060),
     ylim=c(0.0025,0.05), pch =20, log="y",
     xlab="year", ylab="q(65,t) (log scale)")
fan(t(APCsim$rates["75",,]), start=2016,
    probs=c(2.5,10,25,50,75,90,97.5), n.fan=4, ln=NULL,
    fan.col=colorRampPalette(c("black","white")))






pred.gc.APC <- forecast(auto.arima(APCfit$gc, max.d = 1), h = 50)


Pred_APC <- predict(APCfit,years=2017:2066,kt=APCfor$kt.f$mean,gc = c(tail(APCfit$gc, 26), pred.gc.APC$mean),type = "rates")

persp3d(53:79,2017:2066,Pred_APC,theta=-20,phi=20,xlab='age' ,ylab='annee',zlab='ln(mu)' ,ticktype = "detailed",border = "black",col="light blue" )











#### Fitting du modèle en supprimant les années de 2000 à 2002 ####################



# Fitting du modèle ( regression non linèaire voir Charpentier pour plus d'info)
APCfit <- fit(APC, Dxt= Dxt_homme_prosp[40:87,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[40:87,4:length(Dxt_homme_prosp)],ages=41:88 ,years=2003:2016)

par(mfrow=c(1,1))
plot(APCfit,type='o')


# Goodness of fit ( Residus )
APCres <- residuals(APCfit)


plot(APCres, type = "colourmap", reslim = c(-3, 3)) # Heat Map pour visualiser l'evolution des qx


# Prevision du Kt horizon 50 ans (h=50)
APCfor <- forecast(APCfit, h=50) 

plot(APCfor, parametricbx = FALSE)



# Simulation de trajectoire pour le Kt horizon 50 ans et 500 simulation
APCsim <- simulate(APCfit, nsim=500, h=50)


plot(APCfit$years, APCfit$kt[1,],
     xlim=c(2003,2066), ylim=c(-65,65),
     type="l", xlab="year", ylab="kt",
     main="Period index (APC)") # Tracé du Kt deja existant

matlines(APCsim$kt.s$years, APCsim$kt.s$sim[1,,1:20],
         type="l", lty=1) # Tracé  des nsim projections de Kt



# Projection du qx pour un âge fixé 
library(fanplot)


plot(APCfit$years, (Dxt_homme_prosp[,4:length(Dxt_homme_prosp)]/Ext_homme_prosp[,4:length(Dxt_homme_prosp)])["75",], xlim=c(2003,2060),
     ylim=c(0.0025,0.15), pch =20, log="y",
     xlab="year", ylab="q(75,t) (log scale)",type='l')

fan(t(APCsim$rates["75",,]), start=2016,
    probs=c(2.5,10,25,50,75,90,97.5), n.fan=4, ln=NULL,
    fan.col=colorRampPalette(c("black","white")))


lines(APCfit$years, (Dxt_homme_prosp[,4:length(Dxt_homme_prosp)]/Ext_homme_prosp[,4:length(Dxt_homme_prosp)])["60",], xlim=c(2003,2060),
      ylim=c(0.0025,0.05), pch =20, log="y",
      xlab="year", ylab="q(75,t) (log scale)",type='l')

fan(t(APCsim$rates["66",,]), start=2016,
    probs=c(2.5,10,25,50,75,90,97.5), n.fan=4, ln=NULL,
    fan.col=colorRampPalette(c("red","white")))












# Spécification du modèle ( deux facons à la main ou modèle deja existant dans le package)

APC <- apc()

# Possibilite d'implémenter des modèle ou d'utiliser les deja existant 
# APC <- apc() modèle âge période cohorte
# Il existe d'autres modèle voir StMoMo documentation


# Fitting du modèle ( regression non linèaire voir Charpentier pour plus d'info)
APCfit <- fit(APC, Dxt= Dxt_homme_prosp[48:86,4:length(Dxt_homme_prosp)], Ext=Ext_homme_prosp[48:86,4:length(Ext_homme_prosp)],ages=49:87 ,years=2003:2016)

par(mfrow=c(1,1))
plot(APCfit,type='o')

qxAPC <- fitted(APCfit, type = "rates")
persp3d(49:87,2003:2016,(qxAPC),theta=-20,phi=20,xlab='age' ,ylab='annee',zlab='ln(mu)' ,ticktype = "detailed",border = "black",col="light blue" )



# Goodness of fit ( Residus )
APCres <- residuals(APCfit)


plot(APCres, type = "colourmap", reslim = c(-3.5, 3.5)) # Heat Map pour visualiser l'evolution des qx


# Prevision du Kt horizon 50 ans (h=50)
APCfor <- forecast(APCfit, h=50) 

plot(APCfor, parametricbx = FALSE)






# Simulation de trajectoire pour le Kt horizon 50 ans et 500 simulation
APCsim <- simulate(APCfit, nsim=500, h=50)


plot(APCfit$years, APCfit$kt[1,],
     xlim=c(2000,2066), ylim=c(-65,65),
     type="l", xlab="year", ylab="kt",
     main="Period index (APC)") # Tracé du Kt deja existant

matlines(APCsim$kt.s$years, APCsim$kt.s$sim[1,,1:20],
         type="l", lty=1) # Tracé  des nsim projections de Kt



# Projection du qx pour un âge fixé 
library(fanplot)


plot(APCfit$years, (Dxt_homme_prosp/Ext_homme_prosp)["75",4:17], xlim=c(2003,2060),
     ylim=c(0.0025,0.05), pch =20, log="y",
     xlab="year", ylab="q(65,t) (log scale)")
fan(t(APCsim$rates["75",,]), start=2016,
    probs=c(2.5,10,25,50,75,90,97.5), n.fan=4, ln=NULL,
    fan.col=colorRampPalette(c("black","white")))






pred.gc.APC <- forecast(auto.arima(APCfit$gc, max.d = 1), h = 50)


Pred_APC <- predict(APCfit,years=2017:2066,kt=APCfor$kt.f$mean,gc = c(tail(APCfit$gc, 38), pred.gc.APC$mean),type = "rates")

persp3d(49:87,2017:2066,Pred_APC,theta=-20,phi=20,xlab='age' ,ylab='annee',zlab='ln(mu)' ,ticktype = "detailed",border = "black",col="light blue" )


