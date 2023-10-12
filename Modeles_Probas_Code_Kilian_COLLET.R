#### Fonctions ####

MV = function(ech,c){  #Fonction qui calcule et retourne le maximum de vraisemblance d'un echantillon censure de loi Weibull
  m = sum(ech < c)
  equa = function(b){
    (1/b)-sum(log(ech)*(ech^b))/(sum((ech^b)))+(1/m)*sum(log(ech[ech<c]))
  }
  betamv = uniroot(equa,c(0.0001,10))$root
  etamv = ((1/m)*sum(ech**betamv))**(1/betamv)
  return(list(BetaMV = betamv,EtaMV = etamv))
}  

SEM = function(ech,c,N,n,ETA,BETA){  #Fonction SEM qui plot les convergences de Beta et Eta
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

SEM2 = function(ech,c,N,n,ETA,BETA){     #Fonction SEM qui retourne tous les Beta et Eta sous forme matricielle
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

beta = 0.5    # C'est le parametre de forme
eta = 100     # C'est le parametre d'echelle
n = 50        # Nombre d'observation
c = 60       # La censure
cens = seq(40,100, by = 20)   # Sous forme de vecteur pour tester differentes censures
N = 50        # Le nombre d'iterations
N1 = 50       # Le nombre d'echantillons a simuler
nobs = seq(25,500, by = 5)   # Le nombre d'observations pour la convergence des methodes

TestBetaMV = rep(NA,length(cens))
TestEtaMV = rep(NA,length(cens))

TestBetaSEM = rep(NA,length(cens))
TestEtaSEM = rep(NA,length(cens))

ConvBetaMV = rep(NA,length(nobs)) ## Vecteur des differents Beta obtenus avec la methode MV
ConvEtaMV = rep(NA,length(nobs)) ## Vecteur des differents Eta obtenus avec la methode MV

ConvBetaSEM = rep(NA,length(nobs)) ## Vecteur des differents Beta obtenus avec la methode SEM
ConvEtaSEM = rep(NA,length(nobs)) ## Vecteur des differents Eta obtenus avec la methode SEM

betaMV = rep(NA,N1)   # Vecteur des differents Beta obtenus avec la methode MV
etaMV = rep(NA,N1)    # Vecteur des differents Eta obtenus avec la methode MV

betaSEM = rep(NA,N1)  # Vecteur des differents Beta obtenus avec la methode SEM
etaSEM = rep(NA,N1)   # Vecteur des differents Eta obtenus avec la methode SEM

M = rep(NA,N1)        # Vecteur contenant le nombre de defaillances dans un echantillon

BETA = rep(0,N)       # Vecteur des differents Beta utilises avec la methode SEM
BETA[1] = 1.5         # Initialisation du premier Beta

ETA = rep(0,N)        # Vecteur des differents Eta utilises pour la methode SEM
ETA[1] = 200          # Initialisation du premier Eta

ech = rweibull(n, beta, eta)  # On cree un echantillon de weibull de parametre (Beta,Eta)
ech[ech >= c] = c             # On censure toute les donnees superieures a notre taux de censure

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

for(i in 1:length(nobs)){           # On enregistre pour chaque nombre d'observation different, les valeurs des estimateurs obtenus avec chaque méthode
  ech = rweibull(nobs[i], beta, eta)   # Initialisation de l'echantillon
  ech[ech >= c] = c                   # Censure
  ConvBetaMV[i] = MV(ech,c)$BetaMV   # On enregistre le Beta obtenu avec la methode MV
  ConvEtaMV[i] = MV(ech,c)$EtaMV     # On enregistre le Eta obtenu avec la methode MV
  temp = SEM2(ech,c,N,nobs[i],ETA,BETA) # On execute la fonction SEM et on enregistre le resultat dans une variable temporaire
  ConvBetaSEM[i] = temp[1,N]          # On enregistre le Beta obtenu avec la methode SEM
  ConvEtaSEM[i] = temp[2,N]           # On enregistre le Eta obtenu avec la methode SEM
}


for(i in 1:N1){           # On fait de même pour chaque echantillon
  # On garde les valeurs des estimateurs obtenu avec chaque methode ainsi que le nombre de defaillances dans notre echantillon
  ech = rweibull(n, beta, eta) 
  ech[ech >= c] = c
  M[i] = sum(ech < c)
  betaMV[i] = MV(ech,c)$BetaMV
  etaMV[i] = MV(ech,c)$EtaMV
  temp = SEM2(ech,c,N,n,ETA,BETA)
  betaSEM[i] = temp[1,N]
  etaSEM[i] = temp[2,N]
}


MoyM = mean(M)                   # Le nombre moyen de defaillances

MoyBetaMV = mean(betaMV)         # Moyenne des Beta MV
MoyEtaMV = mean(etaMV)           # Moyenne des Eta MV

SdBetaMV = sd(betaMV)            # Ecart-type des Beta MV
SdEtaMV = sd(etaMV)              # Ecart-type des Eta MV

MoyBetaSEM = mean(betaSEM)         # Moyenne des Beta MV
MoyEtaSEM = mean(etaSEM)           # Moyenne des Eta MV

SdBetaSEM = sd(betaSEM)            # Ecart-type des Beta MV
SdEtaSEM = sd(etaSEM)              # Ecart-type des Eta MV

#### Affichage des resultats ####

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
legend(x = "topright", legend = c("Densite"), col ='red', lty = 1, cex = 1)

hist(betaSEM, freq = FALSE, main = 'Histogramme de BetaSEM')
lines(density(betaSEM), col = 'red')
legend(x = "topright", legend = c("Densite"), col ='red', lty = 1, cex = 1)

hist(etaMV, freq = FALSE, main = 'Histogramme de EtaMV')
lines(density(etaMV), col = 'red')
legend(x = "topright", legend = c("Densite"), col ='red', lty = 1, cex = 1)

hist(etaSEM, freq = FALSE, main = 'Histogramme de EtaSEM')
lines(density(etaSEM), col = 'red')
legend(x = "topright", legend = c("Densite"), col ='red', lty = 1, cex = 1)


cat("Le nombre moyen de defaillances pour Beta =", beta, "et Eta =", eta, "est :",MoyM)

cat("Le Beta moyen obtenu par la methode MV, pour Beta =", beta, "et Eta =", eta, "est :", MoyBetaMV)
cat("Le Eta moyen obtenu par la methode MV, pour Beta =", beta, "et Eta =", eta, "est :", MoyEtaMV)

cat("L'ecart-type de Beta obtenu par la methode MV, pour Beta =", beta, "et Eta =", eta, "est :", SdBetaMV)
cat("L'ecart-type de Eta obtenu par la methode MV, pour Beta =", beta, "et Eta =", eta, "est :", SdEtaMV)

cat("Le Beta moyen obtenu par la methode SEM, pour Beta =", beta, "et Eta =", eta, "est :", MoyBetaSEM)
cat("Le Beta moyen obtenu par la methode SEM, pour Beta =", beta, "et Eta =", eta, "est :", MoyEtaSEM)

cat("L'ecart-type de Beta obtenue par la methode SEM, pour Beta =", beta, "et Eta =", eta, "est :", SdBetaSEM)
cat("L'ecart-type de Eta obtenue par la methode SEM, pour Beta =", beta, "et Eta =", eta, "est :", SdEtaSEM)
