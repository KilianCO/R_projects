sink('regr_quantile_expectile_etud.sink')
library(quantreg)
par(mfrow=c(1,2))

# Exercice 1 ####
data(Mammals)
attach(Mammals)
x = log(weight)
y = log(speed)

cat("Exerice 1 \n")
hist(y)
shapiro.test(y)

cat("Question 1 \n")
m1 = rq(y~x, tau=0.5)
summary(m1) #affiche seulement intervalle de confiance des estimations
summary(m1, se="iid")
QUANT=coef(m1) #les coefs
m2=rq(y~x, tau=0.75)
summary(m2, se="iid")
summary(m2)
anova.rq(m1,m2) #compare les deux modèles
prev1=predict(m1)
plot(y,prev1)
sigma1=sd(y-prev1)
sigma1
prev2=predict(m2)
plot(y,prev2)
sigma2=sd(y-prev2)
sigma2
MAD1=mean(abs(y-prev1))
MAD2=mean(abs(y-prev2))

# Question 2
cat("Question 2 \n")
m3=lm(y~x)
summary(m3)
#on compare les intervalles de confiance
confint(m3)
summary(m1,se="iid")
summary(m2,se="iid")
prev3=predict(m3)
sigma3=sd(y-prev3)
sigma1;sigma2;sigma3;
median(y-prev1);median(y-prev2);median(y-prev3)
par(mfrow=c(1,3))
hist(y-prev1);hist(y-prev2);hist(y-prev3)
MAD3=mean(abs(y-prev3))
MAD1;MAD2;MAD3


# Question 3
cat("\n Question 3 \n")
hoppers=factor(hoppers)
specials=factor(specials)
m1bis=rq(y~x+hoppers+specials,tau=0.5)
summary(m1bis,se="iid")
prev1bis=predict(m1bis)
sigma1bis=sd(y-prev1bis)
sigma1bis;sigma1
m2bis=rq(y~x+hoppers+specials,tau=0.75)
summary(m2bis,se="iid")
prev2bis=predict(m2bis)
sigma2bis=sd(y-prev2bis)
sigma2bis;sigma2

sink()
