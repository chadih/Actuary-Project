library(readxl)

# Lecture des hypothèses 

AutonomeH <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Hypothèses/Hypothèses.xlsx"
                        ,sheet = 1 )

AutonomeF <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Hypothèses/Hypothèses.xlsx"
                        ,sheet = 3 )
GIR5 <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Hypothèses/Hypothèses.xlsx"
                        ,sheet = 4 )

GIR34H <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Hypothèses/Hypothèses.xlsx"
                        ,sheet = 5 )

GIR34F <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Hypothèses/Hypothèses.xlsx"
                     ,sheet = 6 )

GIR12H <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Hypothèses/Hypothèses.xlsx"
                     ,sheet = 7 )

GIR12F <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Hypothèses/Hypothèses.xlsx"
                     ,sheet = 8 )


# Création des Ci (matrice de markov par âge)

# Hommes

for(i in 1:121){

Aut <- c(AutonomeH$`Reste aut`[i],AutonomeH$Aut_GIR5[i],AutonomeH$Aut_GIR34[i],0,AutonomeH$Aut_GIR12[i],0,AutonomeH$Aut_DC[i])

GiR5 <- c(0,GIR5$`Rester GIR5`[i],GIR5$GIR5_GIR34[i],0,GIR5$GIR5_GIR12[i],0,GIR5$GIR5_DC[i])

GIR34 <- c(0,0,0,1-GIR34H$GIR34_GIR12[i]-GIR34H$GIR34_DC_Anc1[i],GIR34H$GIR34_GIR12[i],0,GIR34H$GIR34_DC_Anc1[i])

GIR34anc2 <- c(0,0,0,1-GIR34H$GIR34_GIR12[i]-GIR34H$`GIR34_DC_ANC2+`[i],GIR34H$GIR34_GIR12[i],0,GIR34H$`GIR34_DC_ANC2+`[i])

GIR12 <- c(0,0,0,0,0,1-GIR12H$GIR12_DC_Anc1[i],GIR12H$GIR12_DC_Anc1[i])

GIR12anc2 <- c(0,0,0,0,0,1-GIR12H$`GIR12_DC_Anc2+`[i],GIR12H$`GIR12_DC_Anc2+`[i])

DC <- c(0,0,0,0,0,0,1)

mat <- rbind(Aut,GiR5,GIR34,GIR34anc2,GIR12,GIR12anc2,DC)

assign(paste0("matrice",i-1),mat)



}

# Femmes
i=1
for(i in 1:121){
  
  Aut <- c(AutonomeF$`Rester aut`[i],AutonomeF$Aut_GIR5[i],AutonomeF$Aut_GIR34[i],0,AutonomeF$Aut_GIR12[i],0,AutonomeF$Aut_DC[i])
  
  GiR5 <- c(0,GIR5$`Rester GIR5`[i],GIR5$GIR5_GIR34[i],0,GIR5$GIR5_GIR12[i],0,GIR5$GIR5_DC[i])
  
  GIR34 <- c(0,0,0,1-GIR34F$GIR34_GIR12[i]-GIR34F$GIR34_DC_Anc1[i],GIR34F$GIR34_GIR12[i],0,GIR34F$GIR34_DC_Anc1[i])
  
  GIR34anc2 <- c(0,0,0,1-GIR34F$GIR34_GIR12[i]-GIR34F$`GIR34_DC_ANC2+`[i],GIR34F$GIR34_GIR12[i],0,GIR34F$`GIR34_DC_ANC2+`[i])
  
  GIR12 <- c(0,0,0,0,0,1-GIR12F$GIR12_DC_Anc1[i],GIR12F$GIR12_DC_Anc1[i])
  
  GIR12anc2 <- c(0,0,0,0,0,1-GIR12F$`GIR12_DC_Anc2+`[i],GIR12F$`GIR12_DC_Anc2+`[i])
  
  DC <- c(0,0,0,0,0,0,1)
  
  mat <- rbind(Aut,GiR5,GIR34,GIR34anc2,GIR12,GIR12anc2,DC)
  
  assign(paste0("matricef",i-1),mat)
  
  
  
}


# Vérification que la sommes des colonnes fassent 1
rowSums(matrice18)


# Population fictive

# for ( i in 18:120){
  
assign(paste0("V",i,"_0")  ,c(1000,0,0,0,0,0,0))


} 


# Population


# Model Point 1
Vecteur_pop_homme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 1.xlsx")
Vecteur_pop_homme <- data.frame(Vecteur_pop_homme)
Vecteur_pop_homme[is.na(Vecteur_pop_homme)] <- 0

Vecteur_pop_femme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 1.xlsx",sheet=2)
Vecteur_pop_femme <- data.frame(Vecteur_pop_femme)
Vecteur_pop_femme[is.na(Vecteur_pop_femme)] <- 0


# Model Point 2
Vecteur_pop_homme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 2.xlsx")
Vecteur_pop_homme <- data.frame(Vecteur_pop_homme)
Vecteur_pop_homme[is.na(Vecteur_pop_homme)] <- 0

Vecteur_pop_femme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 2.xlsx",sheet=2)
Vecteur_pop_femme <- data.frame(Vecteur_pop_femme)
Vecteur_pop_femme[is.na(Vecteur_pop_femme)] <- 0

# Model point 3
Vecteur_pop_homme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 3.xlsx")
Vecteur_pop_homme <- data.frame(Vecteur_pop_homme)
Vecteur_pop_homme[is.na(Vecteur_pop_homme)] <- 0

Vecteur_pop_femme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 3.xlsx",sheet=2)
Vecteur_pop_femme <- data.frame(Vecteur_pop_femme)
Vecteur_pop_femme[is.na(Vecteur_pop_femme)] <- 0

# Model point 4
Vecteur_pop_homme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 4.xlsx")
Vecteur_pop_homme <- data.frame(Vecteur_pop_homme)
Vecteur_pop_homme[is.na(Vecteur_pop_homme)] <- 0

Vecteur_pop_femme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 4.xlsx",sheet=2)
Vecteur_pop_femme <- data.frame(Vecteur_pop_femme)
Vecteur_pop_femme[is.na(Vecteur_pop_femme)] <- 0

# Model point 5
Vecteur_pop_homme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 5.xlsx")
Vecteur_pop_homme <- data.frame(Vecteur_pop_homme)
Vecteur_pop_homme[is.na(Vecteur_pop_homme)] <- 0

Vecteur_pop_femme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 5.xlsx",sheet=2)
Vecteur_pop_femme <- data.frame(Vecteur_pop_femme)
Vecteur_pop_femme[is.na(Vecteur_pop_femme)] <- 0

# Model point 6
Vecteur_pop_homme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 6.xlsx")
Vecteur_pop_homme <- data.frame(Vecteur_pop_homme)
Vecteur_pop_homme[is.na(Vecteur_pop_homme)] <- 0

Vecteur_pop_femme <- read_excel("I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Model Point/Model Point 6.xlsx",sheet=2)
Vecteur_pop_femme <- data.frame(Vecteur_pop_femme)
Vecteur_pop_femme[is.na(Vecteur_pop_femme)] <- 0

# Vecteurs des hommes
for ( i in 18:120){
  
  assign(paste0("V",i,"_0")  ,c(unlist(Vecteur_pop_homme[,i-16])))
  
  
}

#Vecteurs des femmes
for ( i in 18:120){
  
  assign(paste0("Vf",i,"_0")  ,c(unlist(Vecteur_pop_femme[,i-16])))
  
  
}









# Outil de projection de la population

age_debut <- 18
  
age_fin <- 120 

# Fonction de projection

Projector <- function (age_debut,age_fin){

  # Création des vecteurs VXX_XX = VXX-1_XX-1 * Matrice Markov
for( j in age_debut:age_fin){

for ( i in 0:(age_fin - j) ){
  
 a <- unlist(eval(parse(text=paste0("V", j+i , "_" , i ) ) )) 
  
 # matriceF si projection des femmes
  assign(paste0("V",j+1+i,"_",1+i),value = a%*%eval(parse(text=paste0("matrice",j+i))))
  
  
}
}
 
  # Création des matrice de population
  df_aut <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_GIR5 <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_GIR34anc1 <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_GIR34anc2etplus <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_GIR12anc1 <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_GIR12anc2etplus <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_DC <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  
  # Remplissage de la matrice
  for ( k in age_debut:age_fin){
    for(l in 0:(age_fin-age_debut)){
      
      if(exists(paste0("V",k,"_",l))){
        t <- eval(parse(text=paste0("V",k,"_",l)))
                  }
      else{
        t<-c(0,0,0,0,0,0,0)
        }
      
      df_aut[k-age_debut+1,l+1] <- t[1]
      df_GIR5[k-age_debut+1,l+1] <- t[2]
      df_GIR34anc1[k-age_debut+1,l+1] <- t[3]
      df_GIR34anc2etplus[k-age_debut+1,l+1] <- t[4]
      df_GIR12anc1[k-age_debut+1,l+1] <- t[5]
      df_GIR12anc2etplus[k-age_debut+1,l+1] <- t[6]
      df_DC[k-age_debut+1,l+1] <- t[7]
      
    }
    
  }
  
  res<- list(df_aut,df_GIR5,df_GIR34anc1,df_GIR34anc2etplus,df_GIR12anc1, df_GIR12anc2etplus,df_DC)
 return(res) 
   
} 
 
Projectorf <- function (age_debut,age_fin){
  
  # Création des vecteurs VXX_XX = VXX-1_XX-1 * Matrice Markov
  for( j in age_debut:age_fin){
    
    for ( i in 0:(age_fin - j) ){
      
      a <- unlist(eval(parse(text=paste0("Vf", j+i , "_" , i ) ) )) 
      
      # matriceF si projection des femmes
      assign(paste0("Vf",j+1+i,"_",1+i),value = a%*%eval(parse(text=paste0("matricef",j+i))))
      
      
    }
  }
  
  # Création des matrice de population
  df_aut <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_GIR5 <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_GIR34anc1 <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_GIR34anc2etplus <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_GIR12anc1 <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_GIR12anc2etplus <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  df_DC <- matrix(data=NA,age_fin-age_debut+1,age_fin-age_debut+1)
  
  # Remplissage de la matrice
  for ( k in age_debut:age_fin){
    for(l in 0:(age_fin-age_debut)){
      
      if(exists(paste0("Vf",k,"_",l))){
        t <- eval(parse(text=paste0("Vf",k,"_",l)))
      }
      else{
        t<-c(0,0,0,0,0,0,0)
      }
      
      df_aut[k-age_debut+1,l+1] <- t[1]
      df_GIR5[k-age_debut+1,l+1] <- t[2]
      df_GIR34anc1[k-age_debut+1,l+1] <- t[3]
      df_GIR34anc2etplus[k-age_debut+1,l+1] <- t[4]
      df_GIR12anc1[k-age_debut+1,l+1] <- t[5]
      df_GIR12anc2etplus[k-age_debut+1,l+1] <- t[6]
      df_DC[k-age_debut+1,l+1] <- t[7]
      
    }
    
  }
  
  res<- list(df_aut,df_GIR5,df_GIR34anc1,df_GIR34anc2etplus,df_GIR12anc1, df_GIR12anc2etplus,df_DC)
  return(res) 
  
} 

# Liste des projections 

df <- Projector(18,120)

dff <- Projectorf(18,120) # Femmes





library(openxlsx)

df1<- createWorkbook()
addWorksheet(df1,"Hommes Autonomes",tabColour = 'blue')
addWorksheet(df1,"Femmes Autonomes",tabColour = 'red')

addWorksheet(df1,"Hommes GIR5",tabColour = 'blue')
addWorksheet(df1,"Femmes GIR5",tabColour = 'red')

addWorksheet(df1,"Hommes GIR34anc1",tabColour = 'blue')
addWorksheet(df1,"Femmes GIR34anc1",tabColour = 'red')

addWorksheet(df1,"Hommes GIR34anc2et+",tabColour = 'blue')
addWorksheet(df1,"Femmes GIR34anc2et+",tabColour = 'red')

addWorksheet(df1,"Hommes GIR12anc1",tabColour = 'blue')
addWorksheet(df1,"Femmes GIR12anc1",tabColour = 'red')

addWorksheet(df1,"Hommes GIR12anc2et+",tabColour = 'blue')
addWorksheet(df1,"Femmes GIR12anc2et+",tabColour = 'red')

addWorksheet(df1,"Hommes DC",tabColour = 'blue')
addWorksheet(df1,"Femmes DC",tabColour = 'red')

writeData(df1,sheet='Hommes Autonomes',x=df[[1]],sep=',')
writeData(df1,sheet='Femmes Autonomes',x=dff[[1]],sep=',')

writeData(df1,sheet='Hommes GIR5',x=df[[2]],sep=',')
writeData(df1,sheet='Femmes GIR5',x=dff[[2]],sep=',')

writeData(df1,sheet='Hommes GIR34anc1',x=df[[3]],sep=',')
writeData(df1,sheet='Femmes GIR34anc1',x=dff[[3]],sep=',')

writeData(df1,sheet='Hommes GIR34anc2et+',x=df[[4]],sep=',')
writeData(df1,sheet='Femmes GIR34anc2et+',x=dff[[4]],sep=',')

writeData(df1,sheet='Hommes GIR12anc1',x=df[[5]],sep=',')
writeData(df1,sheet='Femmes GIR12anc1',x=dff[[5]],sep=',')

writeData(df1,sheet='Hommes GIR12anc2et+',x=df[[6]],sep=',')
writeData(df1,sheet='Femmes GIR12anc2et+',x=dff[[6]],sep=',')

writeData(df1,sheet='Hommes DC',x=df[[7]],sep=',')
writeData(df1,sheet='Femmes DC',x=dff[[7]],sep=',')

saveWorkbook(df1, file = "I:/_restricted area/RM/I. Risk Management/1.7 - Modèle interne/Cluster 8/PREDICA/Vérification du provisionnement/Verification de la PRC via calcul des lois/Projections pop/Projections MP1.xlsx")






