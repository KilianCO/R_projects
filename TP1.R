# TP1 : Regression Linéaire

# Exercice 1 ####
Cholesterol=c(354,190,405,263,451,302,288,385,402,365,209,290,346,254,395)
Poids=c(84,73,65,70,76,69,63,72,79,75,47,89,65,57,59)
Age=c(46,20,52,30,57,25,28,36,57,44,24,31,52,23,60)
Taille=c(180,190,160,155,165,170,175,180,150,165,160,165,165,170,175)

data=data.frame(Cholesterol,Poids,Age,Taille)
X=cbind(Poids,Age,Taille)
Y=c(Cholesterol)

nobs=length(Y)
shapiro.test(Cholesterol)
cor.test(Y, 1:nobs, method = "spearman")
# p > 0.05 donc HO (décorrelation des obs) non rejetée
# Si Y de loi normale, décorrélation ==> indépendance

# Question 1
Chol = b0 + b1 * Poids + b2 * Age + b3 * Taille + Epsilon
# Avec Epsilon_i suivent N(0,sigma²)

# Question 2
cholest.lm = lm(Cholesterol~Poids+Age+Taille)
cholest.lm
summary(cholest.lm)
# La variance est "residual standard error" au carré (ici 37.4)

# Question 2
shapiro.test(residuals(cholest.lm))
# p > 0.05 donc H0 (décorrélation des obs) non rejetée


predict(cholest.lm)
Res_std=rstudent(cholest.lm)

# Les conditions théoriques nécessaires sont satisfaites donc on peut analyser nos résultats

# Pour faire des tests d'hypothèses implant beta, il faut que béta soit gaussien
# pour que béta soit gaussient il faut que les résidus soient gaussiens
# ==> Test de significativité global


# Question 3
summary(cholest.lm)

# Question 4
confint(cholest.lm,level=0.9) #Intervalle de confiance

# Question 5
summary(cholest.lm)
# Pour tester le modèle, H0 est : aucune variable explicative
# H1 est au moins une variable explicative
# On regarde la dernière ligne
# On regarde la dernière ligne : F-statistic
# C'est un test de Fisher et sa p<0.05 donc on rejette HO 
# Le modèle a un pouvoir explicatif
# Les degrés de liberté sont 3 et 11

# Question 6-7-8
summary(cholest.lm)
# Seul l'age a une p-valeur < 0.05 donc l'hypothèse H0 : le coeff est significativement différent
# de 0 est donc vérifiée
# C'est donc la seule variable qui influence le taux de Chol
# Notons que si 0 est dans l'IC la variable n'est pas significative
# Remarque : chaque b_i testé est effectué en supposant que les autres variables SONT dans le modèle

# Question 9
cholest1=Cholesterol[abs(rstudent(cholest.lm))<2]
poids1=Poids[abs(rstudent(cholest.lm))<2]
taille1=Taille[abs(rstudent(cholest.lm))<2]
age1=Age[abs(rstudent(cholest.lm))<2]

Y1=Cholesterol[abs(rstudent(cholest.lm))<2]
X1=c(poids1,age1,taille1)
nobs1=length(Y1)

shapiro.test(Y1) #p>0.05 donc HO : normalitée des données acceptée
cor.test(Y1, 1:nobs1, method = "spearman") #p>0.05 donc HO : décorrélation acceptée
cholest2.lm = lm(Cholesterol~Poids+Age+Taille)
summary(cholest2.lm)

# Poids et taille non significatifs
cholest2.lm=lm(Y1~Poids1+Age1) # on commence par enlever celle qui a le moins d'effet
summary(cholest2_lm)

# Poids non significatif
cholest2.lm=lm(Y1~Age1)
summary(Cholest2_lm)


# Exercice 2 ####

setwd("U:/Mes documents/M2/RegLin - GD")
data=read.table("auto-mpg.data", col.names=c("mpg","cylinders","displacement","horsepower","weight",
"acceleration","model_year","origin","car_name"),na.strings="?",stringsAsFactors = TRUE)
# Name doit être en factor pour créer des groupes : transformer les variables chaines de caract
# En groupes


data

Y=data$mpg
X=subset(data,select=-c(mpg, cylinders,model_year,origin, car_name))


ml=lm(Y ~ X$displacement + X$horsepower + X$weight + X$acceleration)
shapiro.test(residuals(ml)) #p<0.05 donc rejet de l'hypothèse de normalité


# On va devoir corriger le jeu de données
hist(Y,freq=FALSE)
Ylog=log(Y)



shapiro.test(Ylog)


ml=lm(Ylog ~ X$displacement + X$horsepower + X$weight + X$acceleration)
shapiro.test(residuals(lm))