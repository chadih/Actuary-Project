# Whittaker-Henderson

library(MortalityTables)

#W On met les qx 24 et 25 ans à 0 car ils faussent l'étude et empeche un lissage croissant on supposera que ce sont des fluctuations d'echantillonage
new_Q_H <- c(rep(0,27) , Q_H[28:121])

# Construction de la table de mortalité
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


