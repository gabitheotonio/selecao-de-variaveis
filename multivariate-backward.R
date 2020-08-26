library(mvtnorm)
library(Matrix)
library(dplyr)
library(tidyverse)
################################################################
head(dados.resposta <- dados.mg[,c(7,14)])
dados.explicativos.multi <- cbind(1,dados.mg[,c(8,9)], x0)
matriz.explicativos.multi <- as.matrix(dados.explicativos.multi)
matriz.grandona <- faz.matriz.grandona(dados.resposta, dados.explicativos.multi)
nova.matriz <- lapply(matriz.grandona$matrizvet, function(xqualquer) {cbind(1, xqualquer)})
nova.matriz[1]
matriz.grandona$matrizgrande
x0 <- rnorm(853)
x.list <- matriz.grandona$matrizvet
z.critico <- qnorm(0.975)

# beta.est <- objeto$`Beta chapeu`
# objeto$`Sigma chapeu`
# covariancia.bet <- objeto$`covariancia de beta`
teste <- estimadores(x.list = x.list, dados.resposta)
teste$`Beta chapeu`
teste$`covariancia de beta`
cor(dados.resposta[,1], dados.explicativos.multi)
mean(dados.resposta[,2])
plot(dados.resposta[,1] ~ dados.explicativos.multi$PEA)
cov(dados.resposta$espeprança.nascer, dados.resposta$renda.pc)
cor(dados.explicativos.multi[,-1])
dados.mg[66,]

multivariate.backward <- function(x.list, dados.resposta, z.critico) {
  objeto.1 <- estimadores(x.list, dados.resposta)
  beta.est <- objeto.1$`Beta chapeu`
  covariancia.bet <- objeto.1$`covariancia de beta`
  
  ep.beta <- sqrt(diag(covariancia.bet))
  print(ep.beta)
  xmenos.multi <- x.list
  dim.beta <- length(beta.est)
  test.t <- abs(beta.est/ep.beta)
  for(i in 1:dim.beta)
  {
    if(test.t[i] == Inf) test.t[i] <- NA
    if(is.nan(test.t[i])) test.t[i] <- NA
  }
  aux <- x.list[[1]]
  aux2 <- ncol(aux)/nrow(aux)
  for(i in 1:ncol(dados.resposta)) {
    test.t[aux2*(i-1)+1] <- NA
  }
  print(test.t)
  
  criterio <- sum(test.t - z.critico > 0, na.rm = TRUE)
  if(criterio > 0){
    t.min <- which.min(test.t)
    historico <- c()
    cont <- sum(is.na(test.t))
   while(criterio < (length(test.t)- cont) && (dim.beta > 1)) {
      print(t.min)
      print(criterio)
      historico <- c(historico,row.names(test.t)[t.min])
      xmenos.multi <- lapply(xmenos.multi, function(x) { x[,-(t.min)]})
      objeto.2 <- estimadores(xmenos.multi, dados.resposta)
      covariancia.bet <- objeto.2$`covariancia de beta`
      beta.est<- objeto.2$`Beta chapeu`
      dim.beta <- length(beta.est)
      ep.beta <- sqrt(diag(covariancia.bet))
      test.t <- abs(beta.est/ep.beta)
      aux <- xmenos.multi[[1]]
      aux2 <- ncol(aux)/nrow(aux)
      for(i in 1:ncol(dados.resposta)) {
        test.t[aux2*(i-1)+1] <- NA
      }
      for(i in 1:dim.beta)
      {
        if(test.t[i] == Inf) test.t[i] <- NA
        if(is.nan(test.t[i])) test.t[i] <- NA
      }
      print(test.t)
      cont <- sum(is.na(test.t))
      t.min <- which.min(test.t)
      criterio <- sum(test.t - z.critico > 0, na.rm = TRUE)
      lista.final <- list(covariancia.bet, xmenos.multi, objeto.2, historico)
    } } else { 
      lista.final <- list(covariancia.bet, xmenos.multi, objeto.1, 0)
    }
  
  names(lista.final) <- c("Covariancia Beta", "X", "Estimadores", "Historico")
  return(lista.final)
}
 
multi.back <- multivariate.backward(x.list = x.list, dados.resposta = dados.resposta, z.critico = qnorm(0.975))

########################################## Simulação

p <- 3
q <- 2
n <- 100
require(mvtnorm)
x=rmvnorm(n=n,mean=runif(p,-10,10), sigma = positiva.definida(p))
x=rmvnorm(n=n, mean = runif(p, 10,20)) #identidade
beta=matrix(runif(q*p,0, 10))
x <- cbind(1,x)
#GERA Y
gera.y.estimadores <- function(x, beta, p, n, q)
{
  y <- matrix(0, n, q)
  X <- faz.matriz.grandona(y,x)$matrizvet
  for (i in 1:n) {
    mu.i <- X[[i]]%*%beta
    y[i,] <- as.vector(mu.i) + rmvnorm(1, mean = rep(0,q), sigma = diag(runif(q, 0.5, 1.5)))
  }
  y <- as.data.frame(y)
  est <- estimadores(X,y)
  return(est)
}
objetos <- gera.y.estimadores(x, beta, p, n, q)

