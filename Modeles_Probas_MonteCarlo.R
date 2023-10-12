# TP Monte Carlo

# Intro ####
# Taille de la grille
N=10

# Création de la grille
config = matrix(0, ncol=N+2, nrow=N+2)

# Nombre d'étapes
n=10

# Fonction qui crée une grille de taille N
grille=function(N){
  plot(c(1,N),c(1,N),type='n',axes=FALSE)
  abline(h=1:N,lty=2)
  abline(v=1:N, lty=2)
}

# Fonction qui affiche une grille et ses points
affichage=function(config){
  N=ncol(config)-2
  grille(N)
  for(i in 1:N) {
    pts=which(config[1:N+1,1:N+1]==1,arr.ind=TRUE) #donne un tableau avec x1 y1 en premiere ligne
                                                   #                      x2 y2 en deuxième ligne
                                                   #                      x3 y3 en troisième ligne
    points(pts,cex=1.5)
  }
}

# Création des points
for(i in 1:n){
  point = c(sample(x = 1:N+1, size = 1),sample(x = 1:N+1, size = 1))
  u = runif(1)
  if(u < 0.5){
    if((config[point[1]-1, point[2]] == 0) & (config[point[1]+1, point[2]] == 0) & (config[point[1], point[2]-1] == 0) & (config[point[1], point[2]+1] == 0)){
      config[point[1], point[2]] = 1
    }
  }
  else{
    config[point[1], point[2]] = 0
  }
}








# Tests ####
N=10
grille(N)
affichage(config)


