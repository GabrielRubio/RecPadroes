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

vetNo <- doSample(q,m,d)

q <- 100
m <- c(45.5,973.8)
d <- c(30,100)

vetCa <- doSample(q,m,d)

vetJun = rbind(vetNo, vetCa)
vetJun <- as.data.frame(vetJun)

plot(vetJun, col = "black")
points(vetNo, col = "red", add = TRUE)
points(vetCa, col = "blue", add = TRUE)

###### making  K-means ##############

# para teste
data <- vetSep
k <- 2
inter <- 10

#seleciona dois pontos aleatorios dentro da amostra
criaPonto <- function(data, k, d){
  centros <- matrix(0,k,d)
  for(i in 1:k){
    for(j in 1:d){
      centros[i,j] <- sample(data)[i,j] 
    }
  }
  return(centros)
}

#calcula a media de cada posicao/dimensao de uma matriz
calcMedia <- function(data){
  d<-dim(data)[2]
  med <- matrix(0,1,d)
  for(i in 1:d){
    med[1,i] <- mean(data[,i])
  }
  return(med)
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
agrupa <- function(centros, data){
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

#meu kmeans 
meuKmean <- function(data, k , inter = 10){
  #achando a dimensao em que esta se trabalhando  
  d <- dim(data)[2]
  
  centros <- criaPonto(data,k,d)
  
  g <- menorDist(centros, data)
  
}









