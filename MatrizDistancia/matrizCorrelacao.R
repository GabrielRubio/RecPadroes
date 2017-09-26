#pontos de exemplo
a <- c(1:10)
b <- a*2
c <- a*(b/3)
d <- -b

#matrix de pontos onde cada coluna eh uma coordenada e cada linha eh um ponto
matrixPontos <- rbind(a,b,c,d)

#funcao que calcula a correlacao 
distCor <- function(pA, pB){ return( 1 - cor(pA, pB))}

#ajuste de correlacao
adjust <- function(matrix){
  d <- dim(matrix)
  for(i in 1:d[1]){
    for(j in 1:d[2]){
      value <- matrix[i,j]
      if(value<0){
        matrix[i,j] <- value + 1
      }else{
        matrix[i,j] <- 1 - value
      }
    }
  }
  return(matrix)
}

#calcula a correlacao gerando uma matriz
distMat <- function(vet){
  n = dim(vet)
  m = matrix(0,n[1],n[1])
  
  for(i in 1:n[1]){
    for(j in i:n[1]){
      m[i,j]<-distCor(vet[i,],vet[j,])
      m[j,i]<-m[i,j]
    }
  }
  
  rownames(m)<-rownames(vet)
  colnames(m)<-rownames(vet)
  # m <- adjust(m)
  return(m)
}

# exemplo
distMat(matrixPontos)

##### Dados reais (wine)
wine.data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
wine.ann <- wine.data[,1]
wine.data <- wine.data[,-1]

#fazendo a matriz de distancia
data = distMat(as.matrix(wine.data))

#convertendo para um objeto dist
data = as.dist(data)

#usando os algoritmos de cluster Pam e Fuzzy
library(cluster)
wine.pam <- pam(scale(data), 3)
erro.pam <- sum(!wine.pam$clustering == wine.ann)

wine.fanny <- fanny(scale(data), 3)
erro.fanny <- sum(!wine.fanny$clustering == wine.ann)

#plotando os graficos com os diferentes cluster e o cluster real
par(mfrow = c(1,3))
plot(wine.data[,1], wine.data[,7], col = wine.fanny$clustering, main = "Cluster Fanny")
plot(wine.data[,1], wine.data[,7], col = wine.pam$clustering, main = "Cluster Pam")
plot(wine.data[,1], wine.data[,7], col = wine.ann, main = "Cluster Real")

#vendo o erro
#quantidade de elementos nos grupos Reais
table(wine.ann)
#quantidade de elementos nos grupos gerados pelo fanny
table(wine.fanny$clustering)
#quantidade de elementos nos grupos gerados pelo pam
table(wine.pam$clustering)
