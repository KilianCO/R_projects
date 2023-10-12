#### Fonctions ####

MV = function(ech,c){  #Fonction retournant le maximum de vraisemblance d'un échantillon censuré de loi Weibull
  m = sum(ech < c)
  equa = function(b){
    (1/b)-sum(log(ech)*(ech^b))/(sum((ech^b)))+(1/m)*sum(log(ech[ech<c]))
  }
  betamv = uniroot(equa,c(0.0001,10))$root
  etamv = ((1/m)*sum(ech**betamv))**(1/betamv)
  return(list(BetaMV = betamv,EtaMV = etamv))
}  

SEM = function(ech,c,N,n,ETA,BETA){  #Fonction SEM affichant le plot des convergences de Beta et Eta
  for(k in 2:N){
    echT = ech
    for (l in 1:n){
      if(echT[l] == c){
        echT[l] = ETA[k-1]*(-log(runif(1)) + (c/ETA[k-1])**BETA[k-1])**(1/BETA[k-1]) 
      }
    }
    equat = function(b){
      (1/b)-sum(log(echT)*(echT^b))/(sum((echT^b)))+(1/n)*sum(log(echT))
    }
    BETA[k] = uniroot(equat,c(0.0001,10),extendInt="yes")$root
    ETA[k] = ((1/n)*sum(echT**BETA[k]))**(1/BETA[k])
  }
  par(mfrow=c(1,2))
  plot(BETA,type='l', col='red', main = 'Convergence de Beta', xlab = 'N', ylim =c(min(min(BETA),beta),max(max(BETA),beta)))
  abline(h=beta)
  plot(ETA,type='l', col='blue', main = 'Convergence de Eta', xlab = 'N', ylim =c(min(min(ETA),eta),max(max(ETA),eta)))
  abline(h=eta)
  return(list(BETA,ETA))
}

SEM2 = function(ech,c,N,n,ETA,BETA){     #Fonction SEM retournant tous les Beta et Eta sous forme de matrice
  for(k in 2:N){
    echT = ech
    for (l in 1:n){
      if(echT[l] == c){
        echT[l] = ETA[k-1]*(-log(runif(1)) + (c/ETA[k-1])**BETA[k-1])**(1/BETA[k-1]) 
      }
    }
    equat = function(b){
      (1/b)-sum(log(echT)*(echT^b))/(sum((echT^b)))+(1/n)*sum(log(echT))
    }
    BETA[k] = uniroot(equat,c(0.0001,10),extendInt="yes")$root
    ETA[k] = ((1/n)*sum(echT**BETA[k]))**(1/BETA[k])
  }
  return(matrix(c(BETA,ETA),nrow = 2, byrow = TRUE))
}

#### Initialisation ####

beta = 0.5    #Paramètre de forme
eta = 100     #Paramètre d'échelle
n = 50        #Nombre d'observation
c = 60       #censure
cens = seq(40,100, by = 20)   #Pour tester différentes censures
N = 50        #Nombre d'itération
N1 = 50       #Nombre d'échantillon à simuler
nobs = seq(25,500, by = 5)   #Nombre d'observation pour la convergence des méthodes

TestBetaMV = rep(NA,length(cens))
TestEtaMV = rep(NA,length(cens))

TestBetaSEM = rep(NA,length(cens))
TestEtaSEM = rep(NA,length(cens))

ConvBetaMV = rep(NA,length(nobs)) ##Vecteur qui contiendra les différents Béta obtenu avec la méthode MV pour la convergence
ConvEtaMV = rep(NA,length(nobs)) ##Vecteur qui contiendra les différents Eta obtenu avec la méthode MV pour la convergence

ConvBetaSEM = rep(NA,length(nobs)) ##Vecteur qui contiendra les différents Béta obtenu avec la méthode SEM pour la convergence
ConvEtaSEM = rep(NA,length(nobs)) ##Vecteur qui contiendra les différents Eta obtenu avec la méthode SEM pour la convergence

betaMV = rep(NA,N1)   #Vecteur qui contiendra les différents Béta obtenu avec la méthode MV
etaMV = rep(NA,N1)    #Vecteur qui contiendra les différents Eta obtenu avec la méthode MV

betaSEM = rep(NA,N1)  #Vecteur qui contiendra les différents Béta obtenu avec la méthode SEM
etaSEM = rep(NA,N1)   #Vecteur qui contiendra les différents Eta obtenu avec la méthode SEM

M = rep(NA,N1)        #Vecteur qui contiendra le nombre de défaillance dans un échantillon

BETA = rep(0,N)       #Vecteur qui contiendra les différents Beta utilisé pour la méthode SEM
BETA[1] = 1.5         #On initialise le premier Beta

ETA = rep(0,N)        #Vecteur qui contiendra les différents Eta utilisé pour la méthode SEM
ETA[1] = 150          #On initialise le premier Eta
  
ech = rweibull(n, beta, eta)  #On créer un échantillon de weibull de paramètre (Beta,Eta)
ech[ech >= c] = c             #On censure toute les données supérieur à notre taux de censure

#### Test des fonctions ####

MV(ech,c)
SEM(ech,c,N,n,ETA,BETA)
SEM2(ech,c,N,n,ETA,BETA)

ech = rweibull(n, beta, eta)

for(i in 1:length(cens)){
  echT = ech
  echT[echT >= cens[i]] = cens[i]
  TestBetaMV[i] = MV(echT,cens[i])$BetaMV
  TestEtaMV[i] = MV(echT,cens[i])$EtaMV
  temp = SEM2(echT,cens[i],N,n,ETA,BETA) 
  TestBetaSEM[i] = temp[1,N]         
  TestEtaSEM[i] = temp[2,N]
}

#### Comparaison et convergence ####

for(i in 1:length(nobs)){           #On enregistre pour chaque nombre d'observation différent, les valeurs des estimateurs obtenu avec les méthodes MV et SEM
  ech = rweibull(nobs[i], beta, eta)   #On initialise notre échantillon
  ech[ech >= c] = c              #On le censure
  ConvBetaMV[i] = MV(ech,c)$BetaMV   #On enregistre le Beta obtenu avec la méthode MV
  ConvEtaMV[i] = MV(ech,c)$EtaMV     #On enregistre le Eta obtenu avec la méthode MV
  temp = SEM2(ech,c,N,nobs[i],ETA,BETA) #On execute la fonction SEM et on enregistre le résultat dans une variable temporaire
  ConvBetaSEM[i] = temp[1,N]          #On enregistre le Beta obtenu avec la méthode SEM
  ConvEtaSEM[i] = temp[2,N]           #On enregistre le Eta obtenu avec la méthode SEM
}


for(i in 1:N1){           #On enregistre pour chaque échantillon, les valeurs des estimateurs obtenu avec les méthodes MV et SEM ainsi que le nombre de défaillance dans notre échantillon
  ech = rweibull(n, beta, eta)   #On initialise notre échantillon
  ech[ech >= c] = c              #On le censure
  M[i] = sum(ech < c)            #On enregistre le nombre de défaillance
  betaMV[i] = MV(ech,c)$BetaMV   #On enregistre le Beta obtenu avec la méthode MV
  etaMV[i] = MV(ech,c)$EtaMV     #On enregistre le Eta obtenu avec la méthode MV
  temp = SEM2(ech,c,N,n,ETA,BETA) #On execute la fonction SEM et on enregistre le résultat dans une variable temporaire
  betaSEM[i] = temp[1,N]          #On enregistre le Beta obtenu avec la méthode SEM
  etaSEM[i] = temp[2,N]           #On enregistre le Eta obtenu avec la méthode SEM
}


MoyM = mean(M)                   #Nombre moyen de défaillance

MoyBetaMV = mean(betaMV)         #Moyenne des Beta Max de Vraisemblance
MoyEtaMV = mean(etaMV)           #Moyenne des Eta Max de Vraisemblance

SdBetaMV = sd(betaMV)            #Ecart-type des Beta Max de Vraisemblance
SdEtaMV = sd(etaMV)              #Ecart-type des Eta Max de Vraisemblance

MoyBetaSEM = mean(betaSEM)         #Moyenne des Beta Max de Vraisemblance
MoyEtaSEM = mean(etaSEM)           #Moyenne des Eta Max de Vraisemblance

SdBetaSEM = sd(betaSEM)            #Ecart-type des Beta Max de Vraisemblance
SdEtaSEM = sd(etaSEM)              #Ecart-type des Eta Max de Vraisemblance

#### Affichage des résultats ####

TestBetaMV
TestEtaMV

TestBetaSEM
TestEtaSEM


par(mfrow=c(1,2))
plot(nobs,ConvBetaMV,type='l', col='red', main = 'Convergence de Beta MV', xlab = 'n', ylim =c(min(min(ConvBetaMV),beta),max(max(ConvBetaMV),beta)))
abline(h=beta)
plot(nobs,ConvBetaSEM,type='l', col='red', main = 'Convergence de Beta SEM', xlab = 'n', ylim =c(min(min(ConvBetaSEM),beta),max(max(ConvBetaSEM),beta)))
abline(h=beta)

plot(nobs,ConvEtaMV,type='l', col='blue', main = 'Convergence de Eta MV', xlab = 'n', ylim =c(min(min(ConvEtaMV),eta),max(max(ConvEtaMV),eta)))
abline(h=eta)
plot(nobs,ConvEtaSEM,type='l', col='blue', main = 'Convergence de Eta SEM', xlab = 'n', ylim =c(min(min(ConvEtaSEM),eta),max(max(ConvEtaSEM),eta)))
abline(h=eta)


hist(betaMV, freq = FALSE, main = 'Histogramme de BetaMV')
lines(density(betaMV), col = 'red')
legend(x = "topright", legend = c("Densité"), col ='red', lty = 1, cex = 1)

hist(betaSEM, freq = FALSE, main = 'Histogramme de BetaSEM')
lines(density(betaSEM), col = 'red')
legend(x = "topright", legend = c("Densité"), col ='red', lty = 1, cex = 1)

hist(etaMV, freq = FALSE, main = 'Histogramme de EtaMV')
lines(density(etaMV), col = 'red')
legend(x = "topright", legend = c("Densité"), col ='red', lty = 1, cex = 1)

hist(etaSEM, freq = FALSE, main = 'Histogramme de EtaSEM')
lines(density(etaSEM), col = 'red')
legend(x = "topright", legend = c("Densité"), col ='red', lty = 1, cex = 1)


cat("Le nombre moyen de défaillance pour Beta =", beta, "et Eta =", eta, "est :",MoyM)

cat("Le Beta moyen obtenue par la méthode MV, pour Beta =", beta, "et Eta =", eta, "est :", MoyBetaMV)
cat("Le Eta moyen obtenue par la méthode MV, pour Beta =", beta, "et Eta =", eta, "est :", MoyEtaMV)

cat("L'écart-type de Beta obtenue par la méthode MV, pour Beta =", beta, "et Eta =", eta, "est :", SdBetaMV)
cat("L'écart-type de Eta obtenue par la méthode MV, pour Beta =", beta, "et Eta =", eta, "est :", SdEtaMV)

cat("Le Beta moyen obtenue par la méthode SEM, pour Beta =", beta, "et Eta =", eta, "est :", MoyBetaSEM)
cat("Le Beta moyen obtenue par la méthode SEM, pour Beta =", beta, "et Eta =", eta, "est :", MoyEtaSEM)

cat("L'écart-type de Beta obtenue par la méthode SEM, pour Beta =", beta, "et Eta =", eta, "est :", SdBetaSEM)
cat("L'écart-type de Eta obtenue par la méthode SEM, pour Beta =", beta, "et Eta =", eta, "est :", SdEtaSEM)
