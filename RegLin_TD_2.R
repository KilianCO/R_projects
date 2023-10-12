setwd("U:/Mes documents/M2/RegLin - GD")

par(mfrow =  c(2,2))

#### Exercice 1 : ####

plasma = read.table('Donnee.txt', col.names = c("age","sex","smokestat","quetelet","vituse","calories","fat","fiber","alcohol","cholesterol","betadiet","retdiet","betaplasma","retplasma"))
attach(plasma)
hist(betaplasma, main = "Histogramme de BetaPlasma")
library(car)
sex = factor(sex)
smokestat = factor(smokestat)
vituse = factor(vituse)

cat("Exercice 1 \n")
m1 = lm(betaplasma~vituse, contrasts = list(vituse = contr.sum))
cat("\n Résultats par summary \n ")
print(summary(m1))
shapiro.test(rstudent(m1))
hist(rstudent(m1))
boxplot(rstudent(m1), main = "rstudent(m1)")
ll = log(betaplasma + 0.2)
hist(ll)
shapiro.test(ll)
boxplot(ll, main = "ll")
m2 = lm(ll~vituse, contrasts = list(vituse = contr.sum))
cat("\n Résultats par summary \n ")
print(summary(m2))
shapiro.test(rstudent(m2))
hist(rstudent(m2))
boxplot(rstudent(m2), main = "rstudent(m2)")
ll1 = ll[abs(rstudent(m2))<2]
vituse1 = vituse[abs(rstudent(m2))<2]
m3 = lm(ll1~vituse1, contrasts = list(vituse1=contr.sum))
cat("\n Résultats par summary \n ")
print(summary(m3))
shapiro.test(rstudent(m3))
hist(rstudent(m3))
boxplot(rstudent(m3), main = "rstudent(m3)")


#### Exercice 2 : ####

cat("Exercice 2 \n")
sex1 = sex[abs(rstudent(m2))<2]
smokestat1 = smokestat[abs(rstudent(m2))<2]
m4 = lm(ll1~smokestat1 + vituse1 + smokestat1*vituse1 + sex1*smokestat1 + sex1*vituse1, contrasts = list(smokestat1 = contr.sum, sex1 = contr.sum, vituse1=contr.sum))
print(summary(m4))
cat("\n ANOVa DE TYPE III \n ")
print(Anova(m4, type = "III"))
m5 = lm(ll1~sex1 + smokestat1 + vituse1 + smokestat1*vituse1, contrasts = list(smokestat1 = contr.sum, vituse1=contr.sum))
print(summary(m5))
cat("\n ANOVa DE TYPE III \n ")
print(Anova(m5, type = "III"))
m6 = lm(ll1~smokestat1 + vituse1 + smokestat1*vituse1, contrasts = list(smokestat1 = contr.sum, vituse1=contr.sum))
print(summary(m6))
cat("\n ANOVa DE TYPE III \n ")
print(Anova(m6, type = "III"))
shapiro.test(rstudent(m6))
hist(rstudent(m6))
boxplot(rstudent(m6), main = "rstudent(m6)")
