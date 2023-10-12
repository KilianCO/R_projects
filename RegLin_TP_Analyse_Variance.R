# Importation des données
setwd("U:/Mes documents/M2/RegLin - GD")

data = read.table('AirPullution.txt', col.names = c("NOISE","SIZE","TYPE","SIDE"))
attach(data)

# Question 1

SIZE=factor(SIZE) #Cat variable avec deux valeurs 1 et 2
TYPE=factor(TYPE) #Cat variable avec trois valeurs 1 et 2 et 3
SIDE=factor(SIDE) #Cat variable avec deux valeurs 1 et 2

m1 = lm(NOISE ~ TYPE, contrasts=list(TYPE=contr.sum))
summary(m1)
#contrasts pour imposer une contrainte
#sur la variable type, on impose que la somme soit égale à 0


m1b = lm(NOISE ~ TYPE)
summary(m1b)

# Modèle statistique (m1)
# Voir feuille


# Question 2
summary(m1)
# On regarde la F-statistic : 1.246
# ici sa p-value (0.2721) est supérieure à 0.05
# donc H0 acceptée


# Question 3
shapiro.test((residuals(m1)))
rstudent(m1)
hist(rstudent(m1))

# Question 4
#On regarde le R²-ajusté, ici : 0.006985


cat("\n ANOVA DE TYPE III \n")
print(anova(m1,type="III")) # pour afficher le tableau d'analyse
predict(m1)
resid=residuals(m1) #résidus
ri=rstudent(m1) # résidus standardisés
shapiro.test(ri)
boxplot(ri)


Y1=NOISE[abs(ri)<2]
F1=TYPE[abs(ri)<2]
m1r=lm(Y1~F1,contrasts=list(F1=contr.sum))
summary(m1r)
shapiro.test(residuals(m1r))
rstudent(m1r) #on continue

m1b=lm(NOISE~TYPE)
summary(m1b)

# Question 5
m2=lm(NOISE~TYPE+SIZE,contrasts=list(SIZE=contr.sum,TYPE=contr.sum))
print(summary(m2))
resid=residuals(m2)
shapiro.test(resid)

#On enlève les outliers
n2=NOISE[abs(rstandard(m2))<2]
t2=TYPE[abs(rstandard(m2))<2]
s2=SIZE[abs(rstandard(m2))<2]
m2b=lm(n2~t2+s2,contrasts=list(s2=contr.sum,t2=contr.sum))
print(summary(m2b))
resid2=residuals(m2b)
shapiro.test(resid2) #la p-value est plus petite que sur m2 donc on garde m2

# Question 6
# On regarde les p-value dans summary(m2)
# ici p-value = 10^-16 donc H0 rejetée donc m2 significatif

# Question 7
library(car) #pour ensuite utiliser la fonction Anova
print(Anova(m2,type="III")) #pour tester chaque variable du modèle
print(anova(m2)) #seulement dans le cas de données équilibrées
resid=residuals(m2)
shapiro.test(resid)

# Question 8
print(summary(m2))
# F1_1 = 5.4 et F1_2 = -5.4
# ==> Octel fait moins de bruit

# Question 9
# On regarde le R2 ajusté : 0.8987

# Question 10
