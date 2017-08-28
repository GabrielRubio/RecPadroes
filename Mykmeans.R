###### making a sample set #### 

doSample<-function(qnts, medias, desvios){
  Vet <- NULL
  n <- length(medias)
  for(i in 1:n){
    X = rnorm(qnts,medias[i],desvios[i])
    if(is.null(Vet)){
      Vet <- X
    }else{
      Vet = cbind(Vet,X)
    }
  }
  return(Vet)
}

# Two separate sample
q <- 200
m <- c(38.6,704.1)
d <- c(3,30)

vetNo <- doSample(q,m,d)

q <- 100
m <- c(45.5,973.8)
d <- c(3,30)

vetCa <- doSample(q,m,d)

vetSep <- rbind(vetNo, vetCa)
vetSep <- as.data.frame(vetSep)
#rm(vetNo,vetCa)

plot(vetSep, col = "black")
points(vetNo, col = "red", add = TRUE)
points(vetCa, col = "blue", add = TRUE)

# Two sample together
q <- 200
m <- c(38.6,704.1)
d <- c(30,100)

vetNo2 <- doSample(q,m,d)

q <- 100
m <- c(45.5,973.8)
d <- c(30,100)

vetCa2 <- doSample(q,m,d)

vetJun = rbind(vetNo2, vetCa2)
vetJun <- as.data.frame(vetJun)

plot(vetJun, col = "black")
points(vetNo, col = "red", add = TRUE)
points(vetCa, col = "blue", add = TRUE)

###### doing  K-means ##############

# para teste
data <- vetSep
k <- 2
inter <- 10

#seleciona dois pontos aleatorios dentro da amostra
doCentralPoints <- function(data, k, d){
  a <- sample(1:dim(data)[1])
  centros <- data[a[1],]
  for(i in 1:(k-1)){
    centros <- rbind(centros, data[a[i+1],])
  }
  return(centros)
}

#calcula a media de cada posicao/dimensao de uma matriz
doMeans <- function(data){
  d<-dim(data)[2]
  med <- matrix(0,1,d)
  for(i in 1:d){
    med[1,i] <- mean(data[,i])
  }
  return(med)
}

#calcula a distancia euclidiana de dois pontos
dist <- function(pA, pB){
  tam = length(pA)
  s = 0
  for(i in 1:tam){
    s = s + (pA[i]-pB[i])**2
  }
  return(sqrt(s))
}

#calcula a matriz de distancia 

distMat <- function(vet){
  n = dim(vet)
  m = matrix(Inf,n[1],n[1])
  for(i in 1:n[1]){
    for(j in i:n[1]){
      if(i==j){
        m[i,j]<-Inf
      }else{
        m[i,j]<-dist(vet[i,],vet[j,])
        m[j,i]<-m[i,j]  
      }
    }
  }
  
  rownames(m)<-rownames(vet)
  colnames(m)<-rownames(vet)
  return(m)
}


#acha uma vetor que diz qual ponto eh mais perto de qual ponto

clust <- function(centros, data){
  l <- dim(data)[1]
  n <- as.integer(dim(centros)[1])
  grupo <- matrix(0,l,1)
  for(i in 1:l){
    A <- data[i,]
    M <- rbind(as.matrix(centros),as.matrix(A))
    D <- distMat(M)
    indexMin <- which.min(D[n+1,])
    grupo[i,1] <- indexMin
  }
  return(grupo)
}

data <-  vetSep
k <- 2
#meu kmeans 
meuKmean <- function(data, k , inter = 10){
  #achando a dimensao em que esta se trabalhando  
  d <- dim(data)[2]
  plot(data)
  centros <- doCentralPoints(data,k,d)
  g <- clust(centros, data)
  for(i in 1:(inter-1)){
    for(j in 1:k){
      clu <- which(g==j)
      centro <- doMeans(data[clu,])
      if(j != 1){
        centrosNovos <- rbind(centro,centrosNovos)
      }else{
        centrosNovos <- centro   
      }
    }
    points(centrosNovos, col = "blue", pch = 20)
    points(data, col = g)
    g <- clust(centrosNovos, data)
  }
  return(g)
}

b <- meuKmean(as.data.frame(scale(vetJun)), 2)
b <- meuKmean(vetJun, 2)
par(mfrow = c(1, 2))
plot(vetJun, col = "black", main = "Meu Kmeans")
points(vetJun, col = b)
points(centrosNovos, col = "blue", pch=20)

plot(vetJun, col = "black", main = "Real")
points(vetCa2, col = "blue")


b <- meuKmean(vetSep, 2)
par(mfrow = c(1, 2))
plot(vetSep, col = b, main = "Meu Kmeans")
points(vetSep, col = b)
points(centrosNovos, col = "blue", pch=20)

plot(vetSep, col = "black", main = "Real")
points(vetCa, col = "blue")









