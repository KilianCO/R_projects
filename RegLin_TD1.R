# TD1_REG_GD

sink('exo2_td_regrlin.sink')
postscript("figTD_exo2_regrlin.ps")
#pdf("figTD_exo2_regrlin.pdf")



par(mfrow=c(1,2));



#Air Quality####



attach(airquality)

m0=lm(Ozone~Solar.R+Wind+Temp)

residu=resid(m0)

hist(residu,main="Histogramme Residu M0")

shapiro.test(residu)

boxplot(residu)



boxplot(rstudent(m0))



par(mfrow=c(1,2));



#Question 3####



Ozone1=Ozone[abs(rstudent(m0))<2];

Solar1=Solar.R[abs(rstudent(m0))<2];

Wind1=Wind[abs(rstudent(m0))<2];

Temp1=Temp[abs(rstudent(m0))<2];

m1=lm(Ozone1~Solar1+Wind1+Temp1)

shapiro.test(resid(m1))

hist(resid(m1),main="Histogramme Residu M1")

boxplot(resid(m1))





#Question4####



Ozone2=Ozone[abs(rstudent(m1))<2];

Solar2=Solar.R[abs(rstudent(m1))<2];

Wind2=Wind[abs(rstudent(m1))<2];

Temp2=Temp[abs(rstudent(m1))<2];

m2=lm(Ozone2~Solar2+Wind2+Temp2)

shapiro.test(resid(m2))

hist(resid(m2),main="Histogramme Residu M2")

boxplot(resid(m2))





#Question 5####

shapiro.test(Ozone);

hist(Ozone,main="Histogramme Ozone")

logOzone=log(Ozone)

shapiro.test(logOzone)

m3=lm(logOzone~Solar.R+Wind+Temp)

hist(resid(m3),main="Histogramme Residu M3")



#Question 6####
# on essaie de changer de strategie
shapiro.test(Ozone)
hist(Ozone,main="Histogramme Ozone")
logOzone=log(Ozone)
shapiro.test(logOzone)
m3=lm(logOzone~Solar.R+Wind+Temp)
hist(resid(m3),main="Histogramme Residu M3")
shapiro.test(resid(m3))
logOzone4=logOzone[abs(rstudent((m3)))<2];
Solar4=Solar.R[abs(rstudent((m3)))<2];
Wind4=Wind[abs(rstudent((m3)))<2];
Temp4=Temp[abs(rstudent((m3)))<2];
m4=lm(logOzone4~Solar4+Wind4+Temp4)
shapiro.test(resid(m4))
hist(resid(m4),main="Histogramme Residu M4")
boxplot(resid(m4))
cat("Les résidus pour le modèle M4: \n")
resid(m4)
summary(m4)
sink()
graphics.off


