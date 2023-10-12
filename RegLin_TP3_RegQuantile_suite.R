# Exercice 2 ####
library(quantreg)
setwd('U:/Mes documents/M2/RegLin - GD')
 
plasma = read.table('Donnee.txt', col.names = c("age","sex","smokestat","quetelet","vituse","calories","fat","fiber","alcohol","cholest","betadiet","retdiet","betaplasma","retplasma"))
attach(plasma)
shapiro.test(betaplasma)

par(mfrow=c(1,2))
hist(betaplasma, main = "Histogramme de BetaPlasma");boxplot(betaplasma);

sex=factor(sex)
smokstat=factor(smokestat)
vituse=factor(vituse)
Y=log(betaplasma+0.1)
shapiro.test(Y);hist(Y);boxplot(Y)

# modèle sur log(betaplasma+0.1)
m1_LS=lm(Y~age+sex+smokstat+quetelet+vituse+calories+fat+fiber+alcohol+cholest+betadiet+retdiet+retplasma)
coef_LS=coef(m1_LS)
shapiro.test(x=residuals(m1_LS))
hist(residuals(m1_LS));boxplot(residuals(m1_LS))
summary(m1_LS)

# modèle sur betaplasma
m1_Q1=rq(Y~age+sex+smokstat+quetelet+vituse+calories+fat+fiber+alcohol+cholest+betadiet+retdiet+retplasma)
summary(m1_Q1) # affiche seulement les intervalles de confiance des estimateurs
summary(m1_Q1,se="iid") # affiche les résultats des tests d'hypothèse
coef_Q1=coef(m1_Q1) #les coeffs
shapiro.test(x=residuals(m1_Q1))
hist(residuals(m1_Q1));boxplot(residuals(m1_Q1))

predict_LS=predict(m1_LS)
predict_Q1=predict(m1_Q1)
median(abs(Y-predict_LS))
median(abs(Y-predict_Q1))







