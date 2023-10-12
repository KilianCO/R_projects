## Modèles Probas
## 16/09
# TP 1

# changer le working directory
# setwd("U:/Mes documents/M2/Modèles Probas")


data=read.csv("U:/Mes documents/M2/Modèles Probas/donnees.csv")

hist(data$X..defaillances)
summary(data)
boxplot(data)

defaillances=data$X..defaillances.
defaillances

# On remarque qu'il y a 48 valeurs égales à 2555
sum(defaillances==2555)

# On hist seulement les valeurs inférieures à 2555
hist(defaillances[defaillances<2555],breaks=10)


# Estimation du max de vraisemblance pour un ech de loi Weibull censuré

# ImplémentationS
beta = 1.5
eta = 100
n = 50
c = 40
ech = rweibull(n,beta, eta)  # génère
ech[ech>c]=c                 # fixe plafond
m=sum(ech<c)
ech

equa = function(beta){
  (1/beta)-sum(log(ech)*(ech^beta))/(sum((ech^beta)))+(1/m)*sum(log(ech[ech<c]))
  }

# uniroot résout l'équation
betaE1 = uniroot(equa,c(0,5))$root
betaE1
etaE1 = ((1/m)*sum(ech**betaE1))**(1/betaE1)
etaE1


#On refait pareil avec nos données d'aspirateur
c = 2555
n = length(defaillances)
m = sum(defaillances<c)

equa2 = function(beta){
  (1/beta)-sum(log(defaillances)*(defaillances^beta))/(sum((defaillances^beta)))+(1/m)*sum(log(defaillances[defaillances<c]))
}

uniroot(equa2,c(0,5))$root

betaE2 = uniroot(equa2,c(0,5))$root
etaE2 = ((1/m)*sum(ech**betaE2))**(1/betaE2)
list(betaE1, betaE2, etaE1, etaE2)

# On fait notre boucle
beta = 0.5    #Paramètre de forme
eta = 100     #Paramètre d'échelle

n = 25        #Nombre d'observation
c = 40        #censure
N = 50

BETA = rep(0,N)
BETA[1] = beta

ETA = rep(0,N)
ETA[1] = eta

ech = rweibull(n,beta, eta)  # génère
ech[ech>c]=c                 # fixe plafond
m=sum(ech<c)


equa = function(beta){
  (1/beta)-sum(log(ech)*(ech^beta))/(sum((ech^beta)))+(1/m)*sum(log(ech[ech<c]))
}

for(k in 2:N){
  ech = rweibull(n,BETA[k-1],ETA[k-1])
  ech[ech > c] = c
  
  m = sum(ech<c)
  
  BETA[k] = uniroot(equa,c(0,5))$root
  ETA[k] = ((1/m)*sum(ech**BETA[k]))**(1/BETA[k])
}

BETA
ETA

# Partie E
beta = 3
eta = 100
n = 25
c = 40
M = rep(0,50)

for(i in 1:50){
  Ech = rweibull(n, beta, eta)
  m = sum(Ech < c)
  M[i] = m                                #Nombre de défaillance
}
mean(M)                                #Nombre moyen de défaillance
m = sum(Ech < c)
m # le nombre de défaillances???



# Suite
beta = 3
eta = 100
n = 25
c = 40
M = rep(0,50)
BetaMV = rep(0,50)
EtaMV = rep(0,50)

equa = function(beta){
  (1/beta)-sum(log(Ech)*(Ech^beta))/(sum((Ech^beta)))+(1/m)*sum(log(Ech[Ech<c]))
}

for(i in 1:50){
  Ech = rweibull(n, beta, eta)
  m = sum(Ech < c)
  M[i] = m                                #Nombre de défaillance
  betamv = uniroot(equa,c(0.01,10))$root
  etamv = ((1/m)*sum(Ech**betamv))**(1/betamv)
  BetaMV[i] = betamv                            #Beta Max de Vraisemblance
  EtaMV[i] = etamv                              #Eta Max de Vraisemblance
}
mean(M)                                #Nombre moyen de défaillance
mean(BetaMV)                                #Moyenne des Beta Max de Vraisemblance
mean(EtaMV)                                #Moyenne des Eta Max de Vraisemblance
sd(BetaMV)                                #Ecart-type des Beta Max de Vraisemblance
sd(EtaMV)                                #Ecart-type des Eta Max de Vraisemblance


# TP du 30/09 ####
BETA = rep(0,N)
BETA[1] = 1

ETA = rep(0,N)
ETA[1] = 200

ech = rweibull(n,beta, eta)  # génère
ech[ech>c]=c                 # fixe plafond
m=sum(ech<c)


for(k in 2:N){
  echT = ech
  for(l in 1:n){
    if(echT[l]==c){
      echT[l] = ETA[k-1]*(-log(runif(1)) + (c/ETA[k-1])**BETA[k-1])**(1/BETA[k-1])
    }
  }
  
  equa2 = function(b){
    (1/b)-sum(log(echT)*(echT^b))/(sum((echT^b)))+(1/n)*sum(log(echT))
  }
  
  BETA[k] = uniroot(equa2,c(0.0001,10),extendInt="yes")$root
  ETA[k] = ((1/n)*sum(echT**BETA[k]))**(1/BETA[k])
}

BETA
ETA


# Fonctions ####
MV = function (ech, c){ #en sortie beta_chapeau et eta_chapeau
  m = sum(ech<c)
  equa = function(b){
    (1/b)-sum(log(ech)*(ech^b))/(sum((ech^b)))+(1/m)*sum(log(ech[ech<c]))
  }
  betamv=uniroot(equa,c(0.001,10))$root
  etamv=((1/m)*sum(ech**betamv))**(1/betamv)
  return(c(betamv,etamv))
}

SEM=function(ech,c){
 for(k in 2:N){
    echT = ech
    for(l in 1:n){
      if(echT[l]==c){
        echT[l] = ETA[k-1]*(-log(runif(1)) + (c/ETA[k-1])**BETA[k-1])**(1/BETA[k-1])
      }
    }
    equa2 = function(b){
      (1/b)-sum(log(echT)*(echT^b))/(sum((echT^b)))+(1/n)*sum(log(echT))
    }
    BETA[k] = uniroot(equa2,c(0.0001,10),extendInt="yes")$root
    ETA[k] = ((1/n)*sum(echT**BETA[k]))**(1/BETA[k])
 }
  par(mfrow=c(1,2))
  plot(BETA,type='l',col='red',main='Convergence de Beta',xlab='N',ylim=c(1,4))
  abline(h=beta)
  plot(ETA,type='l',col='red',main='Convergence de Eta',xlab='N',ylim=c(80,200))
  abline(h=eta)
  return(list(BETA,ETA))
}

SEM2=function(ech,c){
  for(k in 2:N){
    echT = ech
    for(l in 1:n){
      if(echT[l]==c){
        echT[l] = ETA[k-1]*(-log(runif(1)) + (c/ETA[k-1])**BETA[k-1])**(1/BETA[k-1])
      }
    }
    equa2 = function(b){
      (1/b)-sum(log(echT)*(echT^b))/(sum((echT^b)))+(1/n)*sum(log(echT))
    }
    BETA[k] = uniroot(equa2,c(0.0001,10),extendInt="yes")$root
    ETA[k] = ((1/n)*sum(echT**BETA[k]))**(1/BETA[k])
  }
  return(list(BETA,ETA))
}

# Tests ####
beta = 2
eta = 100
n = 1000        #Nombre d'observations
c = 40        #censure
N = 50

ech = rweibull(n,beta, eta)   # génère
ech[ech>=c]=c                 # fixe plafond
m=sum(ech<c)

BETA = rep(0,N)
BETA[1] = 2.5

ETA = rep(0,N)
ETA[1] = 150


MV(ech,c)
SEM(ech,c)


# Partie E ####
c=40
N=50
n=25
eta=100
beta=0.5

ech = rweibull(n,beta, eta)   # génère
ech[ech>=c]=c                 # fixe plafond
m=sum(ech<c)

BETA = rep(0,N)
BETA[1] = 2.5

ETA = rep(0,N)
ETA[1] = 150


replicate(SEM2[1,N])


