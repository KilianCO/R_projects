setwd("U:/Mes documents/M2/RegLin - GD")


# Exercice 2 ####

exo2=read.table('dmd.txt',col.names=c("ind","hospid","age","sdate","ck","h","pk","ld","carrier","obsno"),na.strings=".");
attach(exo2)
cat("\n SORTIES EXERCICES 2 \n")
cat("effectifs carrier \n")
table(carrier)
m2=glm(carrier ~ age+ck+h+pk+ld, family="binomial")
summary(m2)



# Exercice 1 ####


# Exercice à réaliser en TP ####
exoTP=read.table('exo_TP_regr_logistic.txt',col.names=c("remiss","cell","li","blast"))
attach(exoTP)

cat("\n QUESTION 1 \n")
table(remiss)
# Y=1 rémission // Y=0 non-rémission
# Seulement 9 rémissions et 18 non rémissions ==> n=27
# P[Y=1]=9/27=1/3

cat("\n QUESTION 2 \n")
# On modélise P[Y=1|X=x]=pi(x)
# Avec X=(X_1,X_2,X_3) le vecteur des variables explicatives (cell,li,blast)
# Ces variables sont toutes numériques
# Et x=(x1,x2,x3) une mesure

# D'après le cours :
# pi(x)=exp(b0+b1*x1+b2*x2+b3*x3)/(1+exp(b0+b1*x1+b2*x2+b3*x3))

# Modèle logistique => estimer b0,b1,b2,b3

m2 = glm(remiss ~ cell+li+blast, family="binomial")
#On indique que Y suit une loi binomiale

cat("\n QUESTION 3 \n")
summary(m2)

# Ho : la variable "cell" n'influe pas sur P[Y=1|X2,X3]
# b1=0 si X2,X3 sont pris en compte en P[Y=1]
# Modèle : pi(x2,x3)=exp(b0+b2*x2+b3*x3)/(1+exp(b0+b2*x2+b3*x3))

# H1 : X1 influe P[Y=1|X2,X3]
# b1 =/= 0 si X2,X3 sont pris en compte en P[Y=1]
# => (M2)

# z=1.058 et pvalue=0.29 (supérieur à 0.05) ==> HO acceptée

# b0=0, b1=0, b3=0 acceptées
# Mais X2 influe P[Y=1]

cat("\n QUESTION 4 \n")
# b0hat = -10.6
# Si x1=x2=x3=0 alors P[Y=1]=exp(-10.6)/(1+exp(-10.6))
# Mais puisque l'hypothèse b0=0 est acceptée, on a aussi :
# P[Y=1]=exp(0)/(1+exp(0))=1/2

# b1hat = 7.46 > 0 
# Si X1 augmente alors P[Y=1|X1,X2,X3] augmente aussi.
# Mais cette augmentation n'est pas significative car H0 : b1=0 est acceptée

# b2hat = 3.26
# Si X2 augmente alors P[Y=1|X1,X2,X3] augmente aussi
# De manière significative car b2 =/= 0
# pi(x1,x2+1,x3)= ...
# pi(x1,x2,x3)= ...
# pi[x1,x2+1,x3]/pi[x1,x2,x3] = exp(b2)/(1+exp(b0+b1*x1+b2(x2+1)+b3*x3))

cat("\n QUESTION 5 \n")
pi2=predict(m2,type="response")
prev2=vector(mode="numeric",length=length(pi2))
prev2[pi2>.5]=1  ## prevision de remiss par lemodèle m2
pred2=predict(m2,type="response")
print(pred2)
table(remiss,prev2)

# Le taux de mauvais classement pour la non-remission est 2/18
# Le taux de mauvais classement pour la rémission est de 5/9

cat("\n QUESTION 6 \n")
m3=glm(remiss~li,family="binomial")

cat("\n QUESTION 7 \n")
pi3=predict(m3,type="response")
prev3=vector(mode="numeric",length=length(pi3))
prev3[pi3>.5]=1  ## prevision de remiss par lemodèle m2

table(remiss,prev3)

