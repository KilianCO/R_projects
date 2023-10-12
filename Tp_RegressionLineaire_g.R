# Exercice 1 ####

Cholesterol = c(354,190,405,263,451,302,288,385,402,365,209,290,346,254,395)
Poids = c(84,73,65,70,76,69,63,72,79,75,47,89,65,57,59)
Age = c(46,20,52,30,57,25,28,36,57,44,24,31,52,23,60)
Taille = c(180,190,160,155,165,170,175,180,150,165,160,165,165,170,175)

data = data.frame(Cholesterol,Poids,Age,Taille)

#Question 1

modele = lm(data$Cholesterol ~ data$Poids+data$Age+data$Taille)

resume = summary(modele)

par(mfrow=c(2,2))
plot(modele)
par(mfrow=c(1,1))

#normalité ?

shapiro.test(modele$residuals)

#pvalue > 0.05 donc on ne rejette pas l'hypothèse de normalité

mean(modele$residuals) #On a bien residus centrée

#indépendance ?

nobs = c(1:length(Age))
cor.test(data$Cholesterol,nobs,method = "spearman")
#On ne rejette pas H0 donc les Yi ne sont pas correlées

#Question 2

#simga2_chap = 37.4

#Question 3

beta = modele$coefficients
beta

#Question 4

Ic_coeff = confint(modele,level = 0.90)
Ic_coeff

#Question 5

F_stat = resume$fstatistic
#pvalue = 0.0002408 < 0.05 donc on rejette H0 : le modele est significatif

#Question 6,7,8

#Seul le coeff Age est significatif au risque de 5%
#Quand on augmente l'age de un an, on augmente en moyenne le taux de cholesterol de 4.7523 unité

#Question 9 (on enleve celle qui a la pvalue la plus eleve donc celle qui influe le moins)

modele_final = lm(data$Cholesterol ~ data$Age + data$Poids)
resume_final = summary(modele_final)
resume_final

#nul donc on garde que l'age

shapiro.test(modele_final$residuals)
modele_final_a = lm(data$Cholesterol ~ data$Age)
resume_final_a = summary(modele_final_a)
resume_final_a


#pvalue > 0.05 donc on ne rejette pas l'hypothèse de normalité

mean(modele_final$residuals) #On a bien residus centrée

#indépendance ?

nobs = c(1:length(Age))
cor.test(data$Cholesterol,nobs,method = "spearman")

# Exercice 2 ####

fileUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"
auto <- read.table(fileUrl)
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model year", "origin", "car name")
auto = as.data.frame(auto[,1:(dim(auto)[2]-1)])

#valeur manquante : Que 4 donc on peut retirer la ligne

cpt = rep(0,length(auto$horsepower))
for (i in 1:length(auto$horsepower)) {
  if (auto$horsepower[i] == '?') {
    cpt[i] = i
  }
}

index = c(cpt[cpt != 0])

auto2 <- auto[-index,]

auto_final = transform(auto2, horsepower = as.double(horsepower))

str(auto_final)

mdl_car = lm(mpg~.,data=auto_final)

resume_car = summary(mdl_car)
resume_car

#normalité ?

shapiro.test(mdl_car$residuals)

#pvalue > 0.05 donc on rejette l'hypothèse de normalité

boxplot(mdl_car$residuals)

#On essaie de passer au log

mdl_car_log = lm(log(mpg)~.,data=auto_final)
resume_final_log = summary(mdl_car_log)

par(mfrow=c(2,2))
plot(mdl_car_log)
par(mfrow=c(1,1))


shapiro.test(mdl_car_log$residuals)
#Donc on rejette pas H0 au risque 1% donc on ne rejette pas l'hypothese de normalité au risque 1%


mean(resume_final_log$residuals) #On a bien residus centrée (tres proche de 0)

#indépendance ?

nobs = c(1:length(auto_final$horsepower))
cor.test(log(auto_final$mpg),nobs,method = "spearman")
#On n'a pas indep. Mais on va faire ici comme is c'était indep.
#On verra dans la suite que on fait la regression et on traite les residus comme une serie chronologique

resume_final_log

pairs(auto_final[,2:dim(auto_final)[2]])

#Modele significatif risque 5%.
#acceleration pas significatif donc on la retire

auto_sans_acceleration = auto_final[,-c(6)]
auto_sans_acceleration

mdl_car_log_sansacc = lm(log(mpg)~.,data=auto_sans_acceleration)
resume_final_log_sansacc = summary(mdl_car_log_sansacc)

par(mfrow=c(2,2))
plot(mdl_car_log_sansacc)
par(mfrow=c(1,1))

res_sansacc = summary(mdl_car_log_sansacc)
res_sansacc

# Exercice 3 ####

Y = c(100,300,150,500,400,350,200,250,450,600,800,10,50,1)
X1 = c(1200,1500,1000,2000,2500,2500,2000,2300,3000,2700,2800,800,700,800)
X2 = c(1,0,0,1,2,3,2,3,4,1,0,1,0,2)
X3 = c(300,400,250,500,600,600,500,700,800,650,550,300,250,300)
donnee = data.frame(Y,X1,X2,X3)

#Question 1

model = lm(Y ~ X1, data = donnee)

shapiro.test(model$residuals) #ok pour normalité
nobs = c(1:length(X1))
cor.test(donnee$Y,nobs,method = "spearman") #Ok pour independance

resume1 = summary(model)
resume1

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

#model globalement significatif au risque 5%
#Salaire significatif au risque 5%
#Quand on augmente le salaire d'une unité, on augmente en moyenne l'épargne du ménage de 0.24401 unité

#Question 2

model2 = lm(Y ~ X1+X2+X3, data = donnee)

shapiro.test(model2$residuals) #ok pour normalité
nobs = c(1:length(X1))
cor.test(donnee$Y,nobs,method = "spearman") #Ok pour independance

resume2 = summary(model2)
resume2

par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(1,1))

#model globalement significatif au risque 5%
#Salaire et nombre d'enfant significatif au risque 5%
#loyer mensuel pas significatif au risque 5% => on l'enlève

donne_sansloyer = donnee[,-c(4)]

model3 = lm(Y ~ X1+X2, data = donne_sansloyer)

shapiro.test(model3$residuals) #ok pour normalité
nobs = c(1:length(X1))
cor.test(donne_sansloyer$Y,nobs,method = "spearman") #Ok pour independance

resume3 = summary(model3)
resume3

par(mfrow=c(2,2))
plot(model3)
par(mfrow=c(1,1))

#model globalement significatif au risque 5%
#Salaire et nombre d'enfant significatif au risque 5%
#Quand on augmente le salaire d'une unité, on augmente en moyenne l'épargne du ménage de 0.3210 unité
#Quand on augmente le nb d'enfants d'une unité, on diminue en moyenne l'épargne du ménage de 99.3304 unité
# Exercice 4 ####

temp <- read.table("U:/Mes documents/reg grande dimension/US Temperatures Datafile.txt",header = TRUE)
temp_data = data.frame(temp)

model_temp = lm(JanTemp~Lat+Long,data = temp_data)
summary(model_temp)

shapiro.test(model_temp$residuals) #ok pour normalité
nobs = c(1:length(temp_data$JanTemp))
cor.test(temp_data$JanTemp,nobs,method = "spearman") #Ok pour independance

res = summary(model_temp)
res

par(mfrow=c(2,2))
plot(model_temp)
par(mfrow=c(1,1))

#Le modele est globalement significatif au risque 5%
#Les coeffs sont significatif au risque 5%
#Lorsqu'on augmente la latitude d'une unité, on diminue en moyenne la temp de 2.16355 unité
#Lorsqu'on augmente la longitude d'une unité, on augmente en moyenne la temp de 0.13396 unité