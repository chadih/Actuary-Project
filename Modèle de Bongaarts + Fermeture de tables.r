## Téléchargement des données 

# Femme
HMD_F=read.table("C:/Users/hhj/Desktop/HMD_Femme_1x1.txt",sep=",",row.names=NULL,col.names=c("Year","Age","mx","qx","ax","lx","dx","Lx","Tx","ex"))
library(readxl)
HMD_F <- read_excel("C:/Users/hhj/Desktop/HMD_Femme_1x1.xlsx")
HMD_F <- data.frame(HMD_F)
colnames(HMD_F) <- c("Year","Age","mx","qx","ax","lx","dx","Lx","Tx","ex")

HMD_F$Year=as.numeric(as.character(HMD_F$Year))

HMD_F$Age=as.numeric(as.character(HMD_F$Age))
HMD_F$qx=as.numeric(HMD_F$qx)
HMD_F$qx[is.na(HMD_F$qx)==TRUE]=1
HMD_F$Age[is.na(HMD_F$Age)==TRUE]=110


# Homme
HMD_H <- read_excel("C:/Users/hhj/Desktop/HMD_Homme_1x1.xlsx")
HMD_H <- data.frame(HMD_H)
colnames(HMD_H) <- c("Year","Age","mx","qx","ax","lx","dx","Lx","Tx","ex")


HMD_H$Year=as.numeric(as.character(HMD_H$Year))

HMD_H$Age=as.numeric(as.character(HMD_H$Age))
HMD_H$qx=as.numeric(HMD_H$qx)
HMD_H$qx[is.na(HMD_H$qx)==TRUE]=1
HMD_H$Age[is.na(HMD_H$Age)==TRUE]=110





x=(0:110)
y=(1900:2013)

# Téléchargement du package d'affichage 3d

library(rgl)

####### Méthode de Bongaarts #########

xMin=0
xMax=110
qxBO=matrix(HMD_F[,4],nrow=111)
qxBO[is.na(qxBO)]=1
Expo=matrix(HMD_F[,6],nrow=111)
DC=matrix(HMD_F[,7],nrow=111)

qxBO_H <- matrix(HMD_H[,4],nrow=111)
write.csv(qxBO_H,file = "C:/Users/hhj/Desktop/qx_HMD.csv")
# fonction � optimiser
qxThatcher=function(a,b,g,x){
	1-exp(-g)*((1+a*exp(b*(x+1)))/(1+a*exp(b*x)))^(-1/b)
  }





  getValeursInitiales=function(qxBrut,xMin,xMax)
  {
# On cherche les valeurs initiales de l'algo d'optimisation avec une regression lin�aire du log(qx)/ log(qx+1)
	q=pmax(qxBrut,10^-10)	#on enlève les valeurs nulles

	x=as.vector(xMin:xMax)
	y=log(pmax(abs(q[(xMin+1):(xMax+1)]-q[(xMin+2):(xMax+2)]),10^-10))
	t=data.frame(cbind(x,y))
	d=lm(y~x,data=t)
	p=as.vector(1:3)

	p[1]=2*10^-4							    #a=Taux accident de base
	p[3]=exp(d$coefficients[2])					#c=exp(pente)
	p[2]=max(10^-10,-log(p[3])*exp(d$coefficients[1])/(p[3]-1)^2)	#b
	return(p)
  }

getValeursInitiales(qxBO[,100],0,110)  
 
 getEcartA=function(qxBrut,expo,xMin,xMax,a,b,g){ # Fonction � minimiser pour Thatcher 
#Ecart quadratique entre les taux estimés et les taux bruts
	#Pondérations par l'exposition éventuelle
	w=pmin(1,as.vector(1:111))
			expo=expo[1:111]
		 	w=expo/(qxBrut*(1-qxBrut))

	w[(is.na(w))|(is.infinite(w))]=0	#Pour les cas où qxBrut= 0 ou 1, on met un poids nul
	
	s=0
	for (x in xMin:xMax){
		s=s+w[x+1]*(qxThatcher(a,b,g,x)-qxBrut[x+1])^2
	 }
	s
  }  

 qxAjuste=function(qxBrut,expo,xMin,xMax){	
	pInitial=getValeursInitiales(qxBrut,xMin,xMax)
	f=function(p){getEcartA(qxBrut,expo,xMin,xMax,p[1],p[2],p[3])}
	
	#Algorithme d'optimisation non-linéaire minimisant f
	argmin=constrOptim(theta = pInitial, f=f,ui=rbind(c(1,0,0),c(0,1,0),c(0,0,1)),ci=c(-0.00001,-0.00001,0),mu=1e-10,control=list(maxit=10^9),method="Nelder-Mead",outer.eps = 1e-150,outer.iterations=1000)$par

	q=as.vector(1:111)
	for (x in 0:110){
		q[x+1]=qxThatcher(a=argmin[1],b=argmin[2],g=argmin[3],x=x)
	}
	list(argmin,q)
  }
  
BO_F=matrix(ncol=111,nrow=111)
A_F=(1:111)
B_F=(1:111)
C_F=(1:111)

for( j in (1:111) )
{
 NewQx= qxAjuste(qxBO[,j],Expo[,j],0,110)
 
 A_F[j]=NewQx[[1]][1]
 B_F[j]=NewQx[[1]][2]
 C_F[j]=NewQx[[1]][3]
 
 BO_F[,j]=NewQx[[2]]
 
 }

BETA_F=mean(B_F)

  getValeursInitiales2=function(qxBrut,xMin,xMax)
  {

	q=pmax(qxBrut,10^-10)	#on enlève les valeurs nulles

	x=as.vector(xMin:xMax)
	y=log(pmax(abs(q[(xMin+1):(xMax+1)]-q[(xMin+2):(xMax+2)]),10^-10))
	t=data.frame(cbind(x,y))
	d=lm(y~x,data=t)
	p=as.vector(1:2)

	p[1]=2*10^-4							    #a=Taux accident de base
	p[2]=exp(d$coefficients[2])					#c=exp(pente)
	#b
	return(p)
  }

 qxAjuste2=function(qxBrut,expo,xMin,xMax){	
	pInitial=getValeursInitiales2(qxBrut,xMin,xMax)
	f=function(p){getEcartA(qxBrut,expo,xMin,xMax,p[1],BETA_F,p[2])}
	
	#Algorithme d'optimisation non-linéaire minimisant f
	argmin=constrOptim(theta= pInitial,f=f,ui=rbind(c(1,0),c(0,1)),ci=c(0,0),mu=1e-10,control=list(maxit=10^9),method="Nelder-Mead",outer.eps = 1e-150,outer.iterations=1000)$par

	q=as.vector(1:111)
	for (x in 0:110){
		q[x+1]=qxThatcher(a=argmin[1],b=BETA_F,g=argmin[2],x=x)
	}
	list(argmin,q)
  }
 
A2_F=(1:111)
C2_F=(1:111)
BO2_F=matrix(ncol=111,nrow=111)

for( j in (1:111) )
{
 NewQx2= qxAjuste2(qxBO[,j],Expo[,j],0,110)
 
 A2_F[j]=NewQx2[[1]][1]
 C2_F[j]=NewQx2[[1]][2]
 
 BO2_F[,j]=NewQx2[[2]]
 
 }

Expo=matrix(HMD_F[,6],nrow=111)

qBO2_F=1-exp(-BO2_F)
BorneInfBO2_F=qBO2_F-qnorm(0.975)*sqrt((qBO2_F*(1-qBO2_F))/Expo)
BorneSupBO2_F=qBO2_F+qnorm(0.975)*sqrt((qBO2_F*(1-qBO2_F))/Expo)

plot3d(x,y,HMD_F[,4])

surface3d(x,1900:2010,-log(1-BO2_F),emission="blue",specular="white",shininess=50,alpha=0.6,front="line",back="fill",size=2)




















# Fermeture des tables de mortalité

muxBO2inf_F=matrix(nrow=131,ncol=111)
muxBO2_F=matrix(nrow=131,ncol=111)
muxBO2sup_F=matrix(nrow=131,ncol=111)

# méthode de Coale & Kisker

k_80=(1:111)
s=(1:111)

for(j in (1:111))
{
k_80inf=log(-log(1-BorneInfBO2_F[81,j])/-log(1-BorneInfBO2_F[66,j]))/15
sinf=-(log(-log(1-BorneInfBO2_F[80,j])/0.8)+31*k_80inf)/465 

k_80[j]=log(BO2_F[81,j]/BO2_F[66,j])/15
s[j]=-(log(BO2_F[80,j]/1)+31*k_80[j])/465  # divisé par 0.8 pour les femmes 

k_80sup=log(-log(1-BorneSupBO2_F[81,j])/-log(1-BorneSupBO2_F[66,j]))/15
ssup=-(log(-log(1-BorneSupBO2_F[80,j])/0.8)+31*k_80sup)/465 

for(i in (1:131))
{
if(i<80)
{
muxBO2inf_F[i,j]=-log(1-BorneInfBO2_F[i,j])
muxBO2_F[i,j]=BO2_F[i,j]
muxBO2sup_F[i,j]=-log(1-BorneSupBO2_F[i,j])
}
else 
{
muxBO2inf_F[i,j]=muxBO2inf_F[i-1,j]*exp(k_80inf+sinf*(i-80))
muxBO2_F[i,j]=muxBO2_F[i-1,j]*exp(k_80[j]+s[j]*(i-80))
muxBO2sup_F[i,j]=muxBO2sup_F[i-1,j]*exp(k_80sup+ssup*(i-80))
}
}
}

# méthode de Coale & Go


muxBO2CGinf_F=matrix(nrow=131,ncol=114)
muxBO2CG_F=matrix(nrow=131,ncol=114)
muxBO2CGsup_F=matrix(nrow=131,ncol=114)

for(j in (1:114))
{
kpriinf=log(-log(1-BorneInfBO2_F[76,j])/-log(1-BorneInfBO2_F[75,j]))
Debcinqinf=prod(-log(1-BorneInfBO2_F[76:80,j]))^0.2
Rinf=(30*kpriinf-log((0.66+Debcinqinf)/Debcinqinf))/525

kpri=log(BO2_F[76,j]/BO2_F[75,j])
Debcinq=prod(BO2_F[76:80,j])^0.2
R=(30*kpri-log((0.66+Debcinq)/Debcinq))/525

kprisup=log(-log(1-BorneSupBO2_F[76,j])/-log(1-BorneSupBO2_F[75,j]))
Debcinqsup=prod(-log(1-BorneSupBO2_F[76:80,j]))^0.2
Rsup=(30*kprisup-log((0.66+Debcinqsup)/Debcinqsup))/525

for(i in (0:131))
{
if(i<80)
{
muxBO2CGinf_F[i,j]=-log(1-BorneInfBO2_F[i,j])
muxBO2CG_F[i,j]=BO2_F[i,j]
muxBO2CGsup_F[i,j]=-log(1-BorneSupBO2_F[i,j])
}
else
{
muxBO2CGinf_F[i,j]=-log(1-BorneInfBO2_F[76,j])*exp((i-76)*kpriinf-Rinf*(i-76)*(i-75)/2)
muxBO2CG_F[i,j]=BO2_F[76,j]*exp((i-76)*kpri-R*(i-76)*(i-75)/2)
muxBO2CGsup_F[i,j]=-log(1-BorneSupBO2_F[76,j])*exp((i-76)*kpri-R*(i-76)*(i-75)/2)
}
}
}


surface3d(x,y,1-exp(-muxBO2CG_F),emission="green",specular="white",shininess=50,aBO2ha=0.6,front="line",back="fill",size=2)

######################### DENUIT et GODERNIAUX


mu <- BO2_F


qinf<-BorneInfBO2_F
q <- 1-exp(-mu)
qsup<-BorneSupBO2_F

qF_hatDENUITBO2inf <- matrix(NA,nrow=131, ncol =114)
qF_hatDENUITBO2inf[1:100,]=qinf[1:100,]
qF_hatDENUITBO2 <- matrix(NA,nrow=131, ncol =114)
qF_hatDENUITBO2[1:100,]=q[1:100,]
qF_hatDENUITBO2sup <- matrix(NA,nrow=131, ncol =114)
qF_hatDENUITBO2sup[1:100,]=qsup[1:100,]


for (an in 1:114){

r2inf <- c(0)
r2 <- c(0)
r2sup <- c(0)

j <- 1
###Recherche du meilleur age pour dÃ©marrer
for (age in 60:90)
{
b <- (age+1):90
c <- b^2

fit1inf <- lm(log(qinf[(age:89)+1,an]) ~ b + c)
fit1 <- lm(log(q[(age:89)+1,an]) ~ b + c)
fit1sup <- lm(log(qsup[(age:89)+1,an]) ~ b + c)

r2inf[j] <- summary(fit1inf)$adj.r.squared
r2[j] <- summary(fit1)$adj.r.squared
r2sup[j] <- summary(fit1sup)$adj.r.squared

j <- j+1
}

r2inf <- r2inf[1:27]
r2 <- r2[1:27]
r2sup <- r2sup[1:27]

###Recherche du maximum
maxinf <- r2inf[1]
compoinf <- 1
for (j in 2: length(r2inf)){
if (r2inf[j] > maxinf){
maxinf <- r2inf[j]
compoinf <- j}
}

max <- r2[1]
compo<- 1
for (j in 2: length(r2)){
if (r2[j] > max){
max <- r2[j]
compo <- j}
}

maxsup <- r2sup[1]
composup <- 1
for (j in 2: length(r2sup)){
if (r2sup[j] > maxsup){
maxsup <- r2sup[j]
composup <- j}
}

ageinf <- compoinf + 59
age <- compo + 59
agesup <- composup + 59

b <- age:90
c <- b^2

fitinf <- lm(log(qinf[(age:90)+1,1]) ~ b + c)
fit <- lm(log(q[(age:90)+1,1]) ~ b + c)
fitsup <- lm(log(qsup[(age:90)+1,1]) ~ b + c)

quadratic.q.optinf <- as.vector(exp(fitted(fitinf)))
quadratic.q.opt <- as.vector(exp(fitted(fit)))
quadratic.q.optsup <- as.vector(exp(fitted(fitsup)))

s <- (age):130
s2 <- s^2

OptCtinf <- coef(lm(log(quadratic.q.optinf) ~ I((130 - b)^2) - 1) )
OptCt <- coef(lm(log(quadratic.q.opt) ~ I((130 - b)^2) - 1) )
OptCtsup <- coef(lm(log(quadratic.q.optsup) ~ I((130 - b)^2) - 1) )

model2inf <- exp(OptCtinf * (130^2 - 260*s + s^2))
model2 <- exp(OptCt * (130^2 - 260*s + s^2))
model2sup <- exp(OptCtsup * (130^2 - 260*s + s^2))

qF_hatDENUITBO2inf[(age+1):131,an] <- model2inf
qF_hatDENUITBO2[(age+1):131,an] <- model2
qF_hatDENUITBO2sup[(age+1):131,an] <- model2sup

baseinf=qF_hatDENUITBO2inf
base=qF_hatDENUITBO2
basesup=qF_hatDENUITBO2sup

for(i in ((age-5):(age+5))){qF_hatDENUITBO2inf[i,an] <- exp(1/11 * sum(log(baseinf[(i-5):(i+5),an])))}
for(i in ((age-5):(age+5))){qF_hatDENUITBO2[i,an] <- exp(1/11 * sum(log(base[(i-5):(i+5),an])))}
for(i in ((age-5):(age+5))){qF_hatDENUITBO2sup[i,an] <- exp(1/11 * sum(log(basesup[(i-5):(i+5),an])))}

}

persp(x,y,qF_hatDENUITBO2,theta=-30)

plot(qF_hatDENUITBO2[(80:130),20],type="l")
lines(qF_hatDENUITBO2inf[(80:130),20],type="l",col="red")
lines(qF_hatDENUITBO2sup[(80:130),20],type='l',col="blue")

muF_hatDENUITBO2inf <- -log(1-qF_hatDENUITBO2inf)
muF_hatDENUITBO2 <- -log(1-qF_hatDENUITBO2)
muF_hatDENUITBO2sup <- -log(1-qF_hatDENUITBO2sup)

plot3d(x,y,HMD_F[,4],type="n")
surface3d(x,y,qF_hatDENUITBO2[1:111,],emission="blue",specular="white",shininess=50,aBO2ha=0.6,front="line",back="fill",size=2)

#Modèle de Kannisto

Expo=matrix(HMD_F[,6],nrow=111)
DC=matrix(HMD_F[,7],nrow=111)
MU=BO2_F

MuKaBO2inf_F=matrix(nrow=131,ncol=114)
MuKaBO2_F=matrix(nrow=131,ncol=114)
MuKaBO2sup_F=matrix(nrow=131,ncol=114)

for(j in (1:114))
{

brutinf=-log(1-BorneInfBO2_F[(80:92),j])
brut=MU[(80:100),j]
brutsup=-log(1-BorneSupBO2_F[(80:92),j])

ageb=(80:92)
age=(80:100)

yinf=log(brutinf/(1-brutinf))
modelinf=lm(yinf~ageb)

y=log(brut/(1-brut))
model=lm(y~age)

ysup=log(brutsup/(1-brutsup))
modelsup=lm(ysup~ageb)
	
ainf=coef(modelinf)[1]
Ainf=exp(ainf)/(1+exp(ainf))
Binf=coef(modelinf)[2]

a=coef(model)[1]
A=exp(a)/(1+exp(a))
B=coef(model)[2]

asup=coef(modelsup)[1]
Asup=exp(asup)/(1+exp(asup))
Bsup=coef(modelsup)[2]
	
for(i in (1:131))
{
if(i<80)
{
MuKaBO2inf_F[i,j]=-log(1-BorneInfBO2_F[i,j])
MuKaBO2_F[i,j]=MU[i,j]
MuKaBO2sup_F[i,j]=-log(1-BorneSupBO2_F[i,j])
}
else
{
MuKaBO2inf_F[i,j]=Ainf*exp(Binf*i)/(1+Ainf*(exp(Binf*i)-1))
MuKaBO2_F[i,j]=A*exp(B*i)/(1+A*(exp(B*i)-1))
MuKaBO2sup_F[i,j]=Asup*exp(Bsup*i)/(1+Asup*(exp(Bsup*i)-1))
}
}

}


plot(q_hatDENUIT[,10],type="l",lty="longdash")
lines(1-exp(-muxBO2CG)[,10],type="l",lty="dashed")
lines(1-exp(-muxBO2)[,10],type="l",lty="dotted")
lines(1-exp(-MuKaBO2_F)[,10],type="l",lty="dotdash")

dev.new()

plot(q_hatDENUIT[,30],type="l",lty="longdash")
lines(1-exp(-muxBO2CG)[,30],type="l",lty="dashed")
lines(1-exp(-muxBO2)[,30],type="l",lty="dotted")
lines(1-exp(-MuKaBO2_F)[,30],type="l",lty="dotdash")

dev.new()

plot(q_hatDENUIT[,50],type="l",lty="longdash")
lines(1-exp(-muxBO2CG)[,50],type="l",lty="dashed")
lines(1-exp(-muxBO2)[,50],type="l",lty="dotted")
lines(1-exp(-MuKaBO2_F)[,50],type="l",lty="dotdash")

dev.new()

plot(qF_hatDENUITBO2[,70],type="l",lty="longdash")
lines(1-exp(-muxBO2CG_F)[,70],type="l",lty="dashed")
lines(1-exp(-muxBO2_F)[,70],type="l",lty="dotted")
lines(1-exp(-MuKaBO2_F)[,70],type="l",lty="dotdash")

dev.new()

plot(q_hatDENUIT[,90],type="l",lty="longdash")
lines(1-exp(-muxBO2CG)[,90],type="l",lty="dashed")
lines(1-exp(-muxBO2)[,90],type="l",lty="dotted")
lines(1-exp(-MuKaBO2_F)[,90],type="l",lty="dotdash")


####### Méthode de Bongaarts ######### pour les hommes 

xMin=0
xMax=110
qxBO=matrix(HMD_H[,4],nrow=111)
Expo=matrix(HMD_H[,6],nrow=111)
DC=matrix(HMD_H[,7],nrow=111)

qxThatcher=function(a,b,g,x){
	1-exp(-g)*((1+a*exp(b*(x+1)))/(1+a*exp(b*x)))^(-1/b)
  }

  getValeursInitiales=function(qxBrut,xMin,xMax)
  {

	q=pmax(qxBrut,10^-10)	#on enlève les valeurs nulles

	x=as.vector(xMin:xMax)
	y=log(pmax(abs(q[(xMin+1):(xMax+1)]-q[(xMin+2):(xMax+2)]),10^-10))
	t=data.frame(cbind(x,y))
	d=lm(y~x,data=t)
	p=as.vector(1:3)

	p[1]=2*10^-4							    #a=Taux accident de base
	p[3]=exp(d$coefficients[2])					#c=exp(pente)
	p[2]=max(10^-10,-log(p[3])*exp(d$coefficients[1])/(p[3]-1)^2)	#b
	return(p)
  }

getValeursInitiales(qxBO[,100],0,110)  
 
 getEcartA=function(qxBrut,expo,xMin,xMax,a,b,g){
#Ecart quadratique entre les taux estimés et les taux bruts
	#Pondérations par l'exposition éventuelle
	w=pmin(1,as.vector(1:111))
			expo=expo[1:111]
		 	w=expo/(qxBrut*(1-qxBrut))

	w[(is.na(w))|(is.infinite(w))]=0	#Pour les cas où qxBrut= 0 ou 1, on met un poids nul
	
	s=0
	for (x in xMin:xMax){
		s=s+w[x+1]*(qxThatcher(a,b,g,x)-qxBrut[x+1])^2
	 }
	s
  }  

 qxAjuste=function(qxBrut,expo,xMin,xMax){	
	pInitial=getValeursInitiales(qxBrut,xMin,xMax)
	f=function(p){getEcartA(qxBrut,expo,xMin,xMax,p[1],p[2],p[3])}
	
	#Algorithme d'optimisation non-linéaire minimisant f
	argmin=constrOptim(theta= pInitial,f=f,ui=rbind(c(1,0,0),c(0,1,0),c(0,0,1)),ci=c(-0.00001,-0.00001,0),mu=1e-10,control=list(maxit=10^9),method="Nelder-Mead",outer.eps = 1e-150,outer.iterations=1000)$par

	q=as.vector(1:111)
	for (x in 0:110){
		q[x+1]=qxThatcher(a=argmin[1],b=argmin[2],g=argmin[3],x=x)
	}
	list(argmin,q)
  }
  
BO_H=matrix(ncol=114,nrow=111)
A_H=(1:114)
B_H=(1:114)
C_H=(1:114)

for( j in (1:114) )
{
 NewQx= qxAjuste(qxBO[,j],Expo[,j],0,110)
 
 A_H[j]=NewQx[[1]][1]
 B_H[j]=NewQx[[1]][2]
 C_H[j]=NewQx[[1]][3]
 
 BO_H[,j]=NewQx[[2]]
 
 }

BETA_H=mean(B_H)

  getValeursInitiales2=function(qxBrut,xMin,xMax)
  {

	q=pmax(qxBrut,10^-10)	#on enlève les valeurs nulles

	x=as.vector(xMin:xMax)
	y=log(pmax(abs(q[(xMin+1):(xMax+1)]-q[(xMin+2):(xMax+2)]),10^-10))
	t=data.frame(cbind(x,y))
	d=lm(y~x,data=t)
	p=as.vector(1:2)

	p[1]=2*10^-4							    #a=Taux accident de base
	p[2]=exp(d$coefficients[2])					#c=exp(pente)
	#b
	return(p)
  }

 qxAjuste2=function(qxBrut,expo,xMin,xMax){	
	pInitial=getValeursInitiales2(qxBrut,xMin,xMax)
	f=function(p){getEcartA(qxBrut,expo,xMin,xMax,p[1],BETA_H,p[2])}
	
	#Algorithme d'optimisation non-linéaire minimisant f
	argmin=constrOptim(theta= pInitial,f=f,ui=rbind(c(1,0),c(0,1)),ci=c(0,0),mu=1e-10,control=list(maxit=10^9),method="Nelder-Mead",outer.eps = 1e-150,outer.iterations=1000)$par

	q=as.vector(1:111)
	for (x in 0:110){
		q[x+1]=qxThatcher(a=argmin[1],b=BETA_H,g=argmin[2],x=x)
	}
	list(argmin,q)
  }
 
A2_H=(1:114)
C2_H=(1:114)
BO2_H=matrix(ncol=114,nrow=111)

for( j in (1:114) )
{
 NewQx2= qxAjuste2(qxBO[,j],Expo[,j],0,110)
 
 A2_H[j]=NewQx2[[1]][1]
 C2_H[j]=NewQx2[[1]][2]
 
 BO2_H[,j]=NewQx2[[2]]
 
 }

Expo=matrix(HMD_H[,6],nrow=111)

qBO2_H=1-exp(-BO2_H)
BorneInfBO2_H=qBO2_H-qnorm(0.975)*sqrt((qBO2_H*(1-qBO2_H))/Expo)
BorneSupBO2_H=qBO2_H+qnorm(0.975)*sqrt((qBO2_H*(1-qBO2_H))/Expo)

plot3d(x,y,HMD_H[,4],type="n")

surface3d(x,y,-log(1-BO2),emission="blue",specular="white",shininess=50,alpha=0.6,front="line",back="fill",size=2)



# Fermeture des tables de mortalité

muxBO2inf_H=matrix(nrow=131,ncol=114)
muxBO2_H=matrix(nrow=131,ncol=114)
muxBO2sup_H=matrix(nrow=131,ncol=114)

# méthode de Coale & Kisker

k_80=(1:114)
s=(1:114)

for(j in (1:114))
{
k_80inf=log(-log(1-BorneInfBO2_H[81,j])/-log(1-BorneInfBO2_H[66,j]))/15
sinf=-(log(-log(1-BorneInfBO2_H[80,j])/0.8)+31*k_80inf)/465 

k_80[j]=log(BO2_H[81,j]/BO2_H[66,j])/15
s[j]=-(log(BO2_H[80,j]/1)+31*k_80[j])/465  # divisé par 0.8 pour les femmes 

k_80sup=log(-log(1-BorneSupBO2_H[81,j])/-log(1-BorneSupBO2_H[66,j]))/15
ssup=-(log(-log(1-BorneSupBO2_H[80,j])/0.8)+31*k_80sup)/465 

for(i in (1:131))
{
if(i<80)
{
muxBO2inf_H[i,j]=-log(1-BorneInfBO2_H[i,j])
muxBO2_H[i,j]=BO2_H[i,j]
muxBO2sup_H[i,j]=-log(1-BorneSupBO2_H[i,j])
}
else 
{
muxBO2inf_H[i,j]=muxBO2inf_H[i-1,j]*exp(k_80inf+sinf*(i-80))
muxBO2_H[i,j]=muxBO2_H[i-1,j]*exp(k_80[j]+s[j]*(i-80))
muxBO2sup_H[i,j]=muxBO2sup_H[i-1,j]*exp(k_80sup+ssup*(i-80))
}
}
}

# méthode de Coale & Go


muxBO2CGinf_H=matrix(nrow=131,ncol=114)
muxBO2CG_H=matrix(nrow=131,ncol=114)
muxBO2CGsup_H=matrix(nrow=131,ncol=114)

for(j in (1:114))
{
kpriinf=log(-log(1-BorneInfBO2_H[76,j])/-log(1-BorneInfBO2_H[75,j]))
Debcinqinf=prod(-log(1-BorneInfBO2_H[76:80,j]))^0.2
Rinf=(30*kpriinf-log((0.66+Debcinqinf)/Debcinqinf))/525

kpri=log(BO2_H[76,j]/BO2_H[75,j])
Debcinq=prod(BO2_H[76:80,j])^0.2
R=(30*kpri-log((0.66+Debcinq)/Debcinq))/525

kprisup=log(-log(1-BorneSupBO2_H[76,j])/-log(1-BorneSupBO2_H[75,j]))
Debcinqsup=prod(-log(1-BorneSupBO2_H[76:80,j]))^0.2
Rsup=(30*kprisup-log((0.66+Debcinqsup)/Debcinqsup))/525

for(i in (0:131))
{
if(i<80)
{
muxBO2CGinf_H[i,j]=-log(1-BorneInfBO2_H[i,j])
muxBO2CG_H[i,j]=BO2_H[i,j]
muxBO2CGsup_H[i,j]=-log(1-BorneSupBO2_H[i,j])
}
else
{
muxBO2CGinf_H[i,j]=-log(1-BorneInfBO2_H[76,j])*exp((i-76)*kpriinf-Rinf*(i-76)*(i-75)/2)
muxBO2CG_H[i,j]=BO2_H[76,j]*exp((i-76)*kpri-R*(i-76)*(i-75)/2)
muxBO2CGsup_H[i,j]=-log(1-BorneSupBO2_H[76,j])*exp((i-76)*kpri-R*(i-76)*(i-75)/2)
}
}
}


surface3d(x,y,1-exp(-muxBO2CG_H),emission="green",specular="white",shininess=50,aBO2ha=0.6,front="line",back="fill",size=2)

######################### DENUIT et GODERNIAUX


mu <- BO2_H


qinf<-BorneInfBO2_H
q <- 1-exp(-mu)
qsup<-BorneSupBO2_H

qH_hatDENUITBO2inf <- matrix(NA,nrow=131, ncol =114)
qH_hatDENUITBO2inf[1:100,]=qinf[1:100,]
qH_hatDENUITBO2 <- matrix(NA,nrow=131, ncol =114)
qH_hatDENUITBO2[1:100,]=q[1:100,]
qH_hatDENUITBO2sup <- matrix(NA,nrow=131, ncol =114)
qH_hatDENUITBO2sup[1:100,]=qsup[1:100,]


for (an in 1:114){

r2inf <- c(0)
r2 <- c(0)
r2sup <- c(0)

j <- 1
###Recherche du meilleur age pour dÃ©marrer
for (age in 60:90)
{
b <- (age+1):90
c <- b^2

fit1inf <- lm(log(qinf[(age:89)+1,an]) ~ b + c)
fit1 <- lm(log(q[(age:89)+1,an]) ~ b + c)
fit1sup <- lm(log(qsup[(age:89)+1,an]) ~ b + c)

r2inf[j] <- summary(fit1inf)$adj.r.squared
r2[j] <- summary(fit1)$adj.r.squared
r2sup[j] <- summary(fit1sup)$adj.r.squared

j <- j+1
}

r2inf <- r2inf[1:27]
r2 <- r2[1:27]
r2sup <- r2sup[1:27]

###Recherche du maximum
maxinf <- r2inf[1]
compoinf <- 1
for (j in 2: length(r2inf)){
if (r2inf[j] > maxinf){
maxinf <- r2inf[j]
compoinf <- j}
}

max <- r2[1]
compo<- 1
for (j in 2: length(r2)){
if (r2[j] > max){
max <- r2[j]
compo <- j}
}

maxsup <- r2sup[1]
composup <- 1
for (j in 2: length(r2sup)){
if (r2sup[j] > maxsup){
maxsup <- r2sup[j]
composup <- j}
}

ageinf <- compoinf + 59
age <- compo + 59
agesup <- composup + 59

b <- age:90
c <- b^2

fitinf <- lm(log(qinf[(age:90)+1,1]) ~ b + c)
fit <- lm(log(q[(age:90)+1,1]) ~ b + c)
fitsup <- lm(log(qsup[(age:90)+1,1]) ~ b + c)

quadratic.q.optinf <- as.vector(exp(fitted(fitinf)))
quadratic.q.opt <- as.vector(exp(fitted(fit)))
quadratic.q.optsup <- as.vector(exp(fitted(fitsup)))

s <- (age):130
s2 <- s^2

OptCtinf <- coef(lm(log(quadratic.q.optinf) ~ I((130 - b)^2) - 1) )
OptCt <- coef(lm(log(quadratic.q.opt) ~ I((130 - b)^2) - 1) )
OptCtsup <- coef(lm(log(quadratic.q.optsup) ~ I((130 - b)^2) - 1) )

model2inf <- exp(OptCtinf * (130^2 - 260*s + s^2))
model2 <- exp(OptCt * (130^2 - 260*s + s^2))
model2sup <- exp(OptCtsup * (130^2 - 260*s + s^2))

qH_hatDENUITBO2inf[(age+1):131,an] <- model2inf
qH_hatDENUITBO2[(age+1):131,an] <- model2
qH_hatDENUITBO2sup[(age+1):131,an] <- model2sup

baseinf=qH_hatDENUITBO2inf
base=qH_hatDENUITBO2
basesup=qH_hatDENUITBO2sup

for(i in ((age-5):(age+5))){qH_hatDENUITBO2inf[i,an] <- exp(1/11 * sum(log(baseinf[(i-5):(i+5),an])))}
for(i in ((age-5):(age+5))){qH_hatDENUITBO2[i,an] <- exp(1/11 * sum(log(base[(i-5):(i+5),an])))}
for(i in ((age-5):(age+5))){qH_hatDENUITBO2sup[i,an] <- exp(1/11 * sum(log(basesup[(i-5):(i+5),an])))}

}

persp(x,y,qH_hatDENUITBO2,theta=-30)

plot(qH_hatDENUITBO2[(80:130),20],type="l")
lines(qH_hatDENUITBO2inf[(80:130),20],type="l",col="red")
lines(qH_hatDENUITBO2sup[(80:130),20],type='l',col="blue")

muH_hatDENUITBO2inf <- -log(1-qH_hatDENUITBO2inf)
muH_hatDENUITBO2 <- -log(1-qH_hatDENUITBO2)
muH_hatDENUITBO2sup <- -log(1-qH_hatDENUITBO2sup)

plot3d(x,y,HMD_H[,4],type="n")
surface3d(x,y,qH_hatDENUITBO2[1:111,],emission="blue",specular="white",shininess=50,aBO2ha=0.6,front="line",back="fill",size=2)

#Modèle de Kannisto

Expo=matrix(HMD_H[,6],nrow=111)
DC=matrix(HMD_H[,7],nrow=111)
MU=BO2_H

MuKaBO2inf_H=matrix(nrow=131,ncol=114)
MuKaBO2_H=matrix(nrow=131,ncol=114)
MuKaBO2sup_H=matrix(nrow=131,ncol=114)

for(j in (1:114))
{

brutinf=-log(1-BorneInfBO2_H[(80:92),j])
brut=MU[(80:100),j]
brutsup=-log(1-BorneSupBO2_H[(80:92),j])

ageb=(80:92)
age=(80:100)

yinf=log(brutinf/(1-brutinf))
modelinf=lm(yinf~ageb)

y=log(brut/(1-brut))
model=lm(y~age)

ysup=log(brutsup/(1-brutsup))
modelsup=lm(ysup~ageb)
	
ainf=coef(modelinf)[1]
Ainf=exp(ainf)/(1+exp(ainf))
Binf=coef(modelinf)[2]

a=coef(model)[1]
A=exp(a)/(1+exp(a))
B=coef(model)[2]

asup=coef(modelsup)[1]
Asup=exp(asup)/(1+exp(asup))
Bsup=coef(modelsup)[2]
	
for(i in (1:131))
{
if(i<80)
{
MuKaBO2inf_H[i,j]=-log(1-BorneInfBO2_H[i,j])
MuKaBO2_H[i,j]=MU[i,j]
MuKaBO2sup_H[i,j]=-log(1-BorneSupBO2_H[i,j])
}
else
{
MuKaBO2inf_H[i,j]=Ainf*exp(Binf*i)/(1+Ainf*(exp(Binf*i)-1))
MuKaBO2_H[i,j]=A*exp(B*i)/(1+A*(exp(B*i)-1))
MuKaBO2sup_H[i,j]=Asup*exp(Bsup*i)/(1+Asup*(exp(Bsup*i)-1))
}
}

}
