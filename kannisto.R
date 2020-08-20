library(MortalityLaws)

kannisto <- read_excel("C:/Users/hhj/Desktop/kannisto.xlsx")

qx <- kannisto$Taux
 
M1 <-  MortalityLaw(65:120,qx=qx[66:121],law="kannisto",opt.method = "LF2" )
M1 <-  MortalityLaw(80:112,qx=qx[81:113],law="kannisto",opt.method = "LF2" )


par(mfrow=c(1,1))
plot(0:120,qx)
 

predict(M1,x=89:120)
predict(M1,x=89:112)


lines(65:120,M1$fitted.values)
lines(80:112,M1$fitted.values)

plot(0:120,c(qx[1:89],predict(M1,x=89:120)))

res <- data.frame(0:120,c(qx[1:89],predict(M1,x=89:120)))

write.csv(res,file="I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Hypothèses/taux.csv")
