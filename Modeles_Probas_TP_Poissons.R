# 09/12/2022
# kilian Collet

# Tp Processus de Poisson

# Question 1
lambda = 1
n=30
inter=rexp(n,lambda)
instants=cumsum(inter)
plot(c(0,instants),0:n,'s',xlab='temps',ylab='Ti')

# Question 2
t=20
i=2
inter=c(0,rexp(1,lambda))

while(inter[i]<t){
  i=i+1
  inter[i]=inter[i-1]+rexp(1,lambda)}
  
plot(c(0,inter),0:length(inter)-1, type='s',xlab='temps',ylab='Ti')


# Question 3
conv=inter/t
conv2=sqrt(t/lambda)*(inter/t-lambda)
