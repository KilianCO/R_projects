A = 200
B = 200
i = 1
p = 0.5
C= A+B
coin <- c("heads", "tails")

awins =0
bwins = 0


vecA <-c(A)
vecB <-c(B)
  
while (A != 0 & B != 0 ){
    i = i +1
    if (sample(coin, size = 1, replace = TRUE, prob = c(p, 1-p)) == "heads"){
      B  = B -1
      A = A +1
      
    }
    else {
      A = A -1
      B = B +1
      
    }
    vecA <- append(vecA,A)
    vecB <- append(vecB,B)
  }
  
  
if(tail(vecA, n=1) == 0){awins = awins +1}
if(tail(vecB, n=1) == 0){bwins = bwins +1}



# Créer une matrice
x<- matrix(c(0.5,0.4,0.1,0.2,0.5,0.3,0.4,0.1,0.5), nrow = 3, ncol=3, byrow=TRUE)

# Afficher
print(x)

# Matrice au carré
x%*%x

# Matrice puissance 5
install.packages("expm")
library(expm) 
x%^%5
x%^%10

