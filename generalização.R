######### GENERALIZA��O DA REGRESS�O ############
library(mvtnorm)
library(Matrix)
library(dplyr)
library(tidyverse)
# FUN��ES ########################################

bd.resposta <- dados.resposta
bd.explica <- dados
faz.matriz.grandona <- function(bd.resposta, bd.explica)
{
  bd.resposta <- as.data.frame(bd.resposta)
  bd.explica <- as.data.frame(bd.explica)
  #definindo indices
  n <- dim(bd.resposta)[1]
  p <- dim(bd.explica)[2]
  q <- dim(bd.resposta)[2]
  
  nest_data <- bd.explica %>% as_tibble() %>% group_split(row_number(), keep = FALSE) %>% map_df(nest)
  
  vet_list <- nest_data %>% mutate(vet = lapply(data, function(row) {
    matrixzr <- matrix(0, nrow = q, ncol = q*p)
    for(i in 1:q)
    {
      for(j in 1:p)
      {
        matrixzr[i,(p*i - p + j)] <- as.numeric(row[1,j])
      }
    }
    return(as.data.frame(matrixzr))
  }))
  
  unnest_data <- vet_list$vet %>% map(unnest)
  
  vet_matrix_list <- lapply(unnest_data, function(x) as.matrix(x))
  
  matriz.grande <- map_df(vet_matrix_list, ~as.data.frame(.))
  lista.final <- list(matrizgrande = matriz.grande, matrizvet = vet_matrix_list)
  return(lista.final)
}
#####################################################
#x.list <- X
#y.resposta <- y
x.list <- teste$matrizvet
head(x.list)
y.resposta <- dados.resposta
estimadores <- function(x.list, y.resposta)
  t(x.list[[1]])%*%solve(sigma_inicial)%*%x.list[[1]]

{
  sigma_inicial <- cov(y.resposta)
  
  listabep <- lapply(x.list, function(xqualquer) {(t(xqualquer)%*%solve(sigma_inicial)%*%xqualquer)})
  bep.matrix <- solve(apply(simplify2array(listabep), c(1,2), sum))
  
  listapop.pt1 <- lapply(x.list, function(xqualquer) {(t(xqualquer)%*%solve(sigma_inicial))})
  listapop.pt2 <- lapply(1:dim(y.resposta)[1], function(indice) {listapop.pt1[[indice]]%*%t(y.resposta[indice,])})
  pop.matrix <- apply(simplify2array(listapop.pt2), c(1,2), sum)
  
  beta.chapeu_inicial <- bep.matrix%*%pop.matrix
  teta0 <- c(as.vector(beta.chapeu_inicial), vech(sigma_inicial))
  crit <- 1
  cont <- 0
  beta.iteracao <- beta.chapeu_inicial
  sigma.iteracao <- sigma_inicial
  while ((crit>=10^-6) & (cont <= 1000)) {
    
    iteracao.sig1 <- lapply(x.list, function(xqualquer) {xqualquer%*%beta.iteracao})
    iteracao.sig2 <- lapply(1:dim(y.resposta)[1], function(indice) {t(y.resposta[indice,]) - iteracao.sig1[[indice]]})
    iteracao.sig3 <- lapply(1:dim(y.resposta)[1], function(indice) {as.matrix(iteracao.sig2[[indice]])%*%t(as.matrix(iteracao.sig2[[indice]]))})
    sigma.iteracao <- (1/dim(y.resposta)[1])*apply(simplify2array(iteracao.sig3), c(1,2), sum)
    
    
    listabep <- lapply(x.list, function(xqualquer) {(t(xqualquer)%*%solve(sigma.iteracao)%*%xqualquer)})
    bep.matrix <- solve(apply(simplify2array(listabep), c(1,2), sum))
    
    listapop.pt1 <- lapply(x.list, function(xqualquer) {(t(xqualquer)%*%solve(sigma.iteracao))})
    listapop.pt2 <- lapply(1:dim(y.resposta)[1], function(indice) {listapop.pt1[[indice]]%*%t(y.resposta[indice,])})
    pop.matrix <- apply(simplify2array(listapop.pt2), c(1,2), sum)
    
    beta.iteracao <- bep.matrix%*%pop.matrix
    
    teta <- c(as.vector(beta.iteracao), vech(sigma.iteracao))
    crit <- sqrt(sum((teta-teta0)^2))
    teta0 <- teta
    cont <- cont + 1
    
  }
  
  
  listabep <- lapply(x.list, function(xqualquer) {(t(xqualquer)%*%solve(sigma.iteracao)%*%xqualquer)})
  bep.matrix <- solve(apply(simplify2array(listabep), c(1,2), sum))
  trans.x <- lapply(x.list, function(xqualquer) {t(xqualquer)})
  sum.xt <- apply(simplify2array(trans.x), c(1,2), sum)
  
  cov.beta <- bep.matrix%*%sum.xt%*%solve(sigma.iteracao)%*%t(sum.xt)%*%t(bep.matrix)
  
  
  lista.final <- list(beta.iteracao, sigma.iteracao, cont, cov.beta)
  names(lista.final) <- c("Beta chapeu", "Sigma chapeu", "contador", "covariancia de beta")
  return(lista.final)
}  
####################################################################

############# DADOS REAIS

names(dados.mg)
#########
dados.resposta <- dados.mg[,c(7,14)]
head(dados.resposta)
matriz.resposta <- as.matrix(dados.resposta)
head(dados.explicativos.multi <- cbind(1,dados.mg[,c(3,8,12)]))
dados.explicativos.multi <- dados.mg[,c(3,8,12)]
matriz.explicativos.multi <- as.matrix(dados.explicativos.multi)
matriz.grandona <- faz.matriz.grandona(dados.resposta, dados.explicativos.multi %>% rename(x1 = desocupacao, x2 = PEA, x3 = t.analf))
matriz.grandona$matrizgrande
matriz.grandona$matrizvet[1]

objeto <- estimadores(matriz.grandona$matrizvet, dados.resposta)

beta.est <- objeto$`Beta chapeu`
objeto$`Sigma chapeu`
covariancia.bet <- objeto$`covariancia de beta`
ep.beta <- sqrt(diag(covariancia.bet))
beta.est/ep.beta

# dados <- dados.mg[,3]
# teste <- faz.matriz.grandona(dados.resposta, dados)
# estimadores(teste$matrizvet, dados.resposta)

ind <- 1:(length(beta.est))
for(i in 1:ncol(dados.resposta))
{
  ind[ncol(dados.explicativos.multi)*(i-1)+1] <- NA
}
ind
#teste t
ep.beta
beta.est/ep.beta
#######################################################################

#####################  S I M U L A � � O #############################

######################################################################
p <- 3
q <- 2
rm(n)
require(mvtnorm)
x=rmvnorm(n=n,mean=runif(p,-10,10), sigma = positiva.definida(p))
x=rmvnorm(n=n, mean = runif(p, 10,20)) #identidade
beta=matrix(runif(q*p,0, 10))

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

#################################################
#tam <- 10

simulacao <- function(tam, x, beta, p, n, q)
{
  
  replica.tam <- replicate(tam, list(gera.y.estimadores(x, beta, p , n, q)))
  
  pega.beta <- replica.tam %>% sapply("[[", "Beta chapeu")
  
  pega.sigma <- replica.tam %>% lapply("[[", "Sigma chapeu")
  
  #pega.epbeta <- replica.tam %>% lapply("[[", "ep beta")
  
  
  dp.beta <- apply(pega.beta, 1, sd)
  
  media.beta <- apply(pega.beta, 1, mean)
  
  #ep.beta <- apply(pega.epbeta,1,mean) #dp.beta tem que ser aproximadamente ep.beta
  #ambos convergem pra zero
  subtracao <- matrix(0,p*q,tam)
  
  for (i in 1:tam) {
    subtracao[,i] <- (pega.beta[,i] - beta)^2
  }
  
  eqm <- (1/tam)*apply(subtracao, 1, sum)
  
  passo1 <- lapply(pega.sigma, function(x) sqrt(diag(solve(as.matrix(x)))))
  
  passo2 <- t(matrix(unlist(passo1), q,tam))
  
  ep.beta <- apply(passo2, 2, mean)
  
  lista.final <- list(eqm, ep.beta, media.beta, dp.beta, replica.tam)
  
  names(lista.final) <- c("EQM", "Erro Padrao", "Media", "SD", "estimativas")
  
  return(lista.final)
}

tam <- 1000
#Graficos

vet <- c(100, 200, 300, 500, 700, 1000)
beta=matrix(runif(q*p, -3, 3))
beta
simus <- list()

for (i in vet) {
  
  x=rmvnorm(n=vet[which(vet %in% i)], mean = runif(p, 10,20)) #identidade

  gera.y.estimadores(x,beta,p, vet[which(vet %in% i)], q)
  
  print(which(vet %in% i))
  simus[[which(vet %in% i)]] <- simulacao(tam,x,beta,p,vet[which(vet %in% i)],q)
}


par(mfrow = c(3,2))
par(mfrow = c(1,1))


dim(pega.betas)
simus.novo1 <- simus[[1]]
estimativas1 <- simus.novo1[[5]]
pega.betas1 <- estimativas1 %>% sapply("[[", "Beta chapeu") 
dim(pega.betas1)


vies.fun <- function(est, b) {
  
  vies <- matrix(0, ncol = (dim(est)[2]), nrow = 6)
  
  for(i in 1:(dim(est)[2])) {
    vies[,i] <- est[,i] - b
  }
  return(vies)
}
pega.betas1[,2] - beta
pega.betas1[,1] - beta
vies <- vies.fun(pega.betas1, beta)
head(pega.betas1)
vies
plot(vies[1,])

######################
simus.novo <- simus[[6]]
estimativas <- simus.novo[[5]]
pega.betas6 <- estimativas %>% sapply("[[", "Beta chapeu") 
dim(pega.betas6)
vies <- vies.fun(pega.betas6, beta)
vies
plot(pega.betas1[1,])
vies[,1] <- pega.betas6[1,] - beta[1]
plot(vies[1,], type = 'l')

#criar a regress�o (fun��o lista)

