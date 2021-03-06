#Dados mg - fun��es
backwardpv <- function(x,y,alpha){
  reg <- lm(y~x)
  X <- cbind(1,x)
  xn <- X
  aa <- summary(reg)
  bb <- aa$coefficients[,4]
  criterio <- sum(bb>alpha)
  if(criterio > 0){
    xmenos <- c()
    historico <- data.frame(names(bb))
    historico[,2] <- names(bb)
    while (criterio > 0)
    {
      pmaxi <- which.max(bb)
      xmenos <- cbind(xmenos, as.character(historico[which(names(pmaxi) == historico[,2]),1]))
      historico[which(names(pmaxi) == historico[,2]),2] <- NA
      xn <- xn[,-pmaxi]
      reg <- lm(y ~ -1 + xn)
      aa <- summary(reg)
      bb <- aa$coefficients[,4]
      historico[!is.na(historico[,2]),2] <- names(bb) 
      criterio <- sum(bb>alpha)
    }
  }
  names(reg$coefficients) <- as.character(historico[!is.na(historico[,2]),1])
  lista <- list(reg, xmenos)
  names(lista) <- c("Sele��o Backward", "Hist�rico")
  return(lista)
}
##################################################################

forwardpv <- function(y, x, alpha, p) {
  k <- 0
  aux <- array()
  historico <- data.frame()
  xmais <- array()
  teste.3 <- array()
  for(i in 1:p) {
    historico[i,1] <- paste0('x', i)
    historico[i,2] <- paste0('x', i)
  }
  historico[,3] <- 1:p
  #caso base
  for (i in 1:p) {
    teste <- summary(lm(y ~ x[,i]))
    teste.2 <- teste$coefficients[,4]
    teste.3[i] <- teste.2[k+2]
  }
  pmini <- which.min(teste.3)
  aux[k+1] <- pmini
  if(teste.3[pmini] < alpha){
    historico[pmini,2]<- NA
    xmais[k+1] <- historico[pmini,1]
    reg.nova <- lm(y ~ x[,is.na(historico[,2])])
    summary(reg.nova)
    k <- k + 1
  } else {
    reg <- lm(y~1)
    return(reg)
  }
  while(k <= p - 1)
  {
    teste.3 <- array()
    for (i in historico[!is.na(historico[,2]), 3])  {
      teste <- summary(lm(y ~ x[,is.na(historico[,2])] + x[,i])) 
      teste.2 <- teste$coefficients[,4]
      teste.3[i] <- teste.2[k+2]
    }
    pmini <- which.min(teste.3)
    aux[k+1] <- pmini
    if(teste.3[pmini] < alpha){
      historico[pmini,2]<- NA
      xmais[k+1] <- historico[pmini,1]
      reg.nova <- lm(y ~ x[,is.na(historico[,2])])
      summary(reg.nova)
      k <- k + 1
    } else {
      break()
    }
  }
  names(reg.nova$coefficients) <- c("Intercept", as.character(historico[is.na(historico[,2]),1]))
  lista <- list(reg.nova, xmais)
  names(lista) <- c("Sele��o Forward", "Hist�rico")
  return(lista)
}

#########################################################################

#dados MG - analise
dados.mg <- read.csv2(file.choose(), sep = ",", dec = ",", header = TRUE)
str(dados.mg)
names(dados.mg) <- c("mun", "atividade", "desocupacao", "saneamento", "coleta.lixo", 
                     "energia.eletrica", "espepran�a.nascer", "PEA", "fecundidade", 
                     "dependencia", "t.envelhecimento", "t.analf", "fundamental.comp",
                     "renda.pc", "pobres")

#Especialidades - mun�cipios (aceitar til)
#Taxa de atividade - 18 anos ou mais 2010	
#Taxa de desocupa��o - 18 anos ou mais 2010	
#% da popula��o em domic�lios com banheiro e �gua encanada 2010	
#% da popula��o em domic�lios com coleta de lixo 2010	
#% da popula��o em domic�lios com energia el�trica 2010
#Esperan�a de vida ao nascer 2010	
#PEA - 18 anos ou mais 2010	
#Taxa de fecundidade total 2010	
#Raz�o de depend�ncia 2010	
#Taxa de envelhecimento 2010	
#Taxa de analfabetismo - 18 anos ou mais 2010	
#% de 18 anos ou mais com fundamental completo 2010	
#Renda per capita 2010	
#% de pobres 2010

dados.mg$atividade <- as.numeric(dados.mg$atividade)
dados.mg$desocupacao <- as.numeric(dados.mg$desocupacao)
dados.mg$saneamento <- as.numeric(dados.mg$saneamento)
dados.mg$coleta.lixo <- as.numeric(dados.mg$coleta.lixo)
dados.mg$energia.eletrica <- as.numeric(dados.mg$energia.eletrica)
dados.mg$espepran�a.nascer <- as.numeric(dados.mg$espepran�a.nascer)
dados.mg$PEA <- as.numeric(dados.mg$PEA)
dados.mg$fecundidade <- as.numeric(dados.mg$fecundidade)
dados.mg$dependencia<- as.numeric(dados.mg$dependencia)
dados.mg$t.envelhecimento <- as.numeric(dados.mg$t.envelhecimento)
dados.mg$t.analf <- as.numeric(dados.mg$t.analf)
dados.mg$fundamental.comp <- as.numeric(dados.mg$fundamental.comp)
dados.mg$renda.pc <- as.numeric(dados.mg$renda.pc)
dados.mg$pobres <- as.numeric(dados.mg$pobres)
str(dados.mg)

regressao1 <- lm(dados.mg$renda.pc ~ dados.mg[,2])
summary(regressao1)
plot(dados.mg$renda.pc~dados.mg$atividade)
abline(regressao1, col = "red")

regressao2 <- lm(dados.mg$renda.pc ~ dados.mg[,3])
summary(regressao2)
plot(dados.mg$renda.pc ~dados.mg$desocupacao)
abline(regressao2, col = "red")

regressao3 <- lm(dados.mg$renda.pc ~ dados.mg[,4])
summary(regressao3)
plot(dados.mg$renda.pc ~ dados.mg[,4])
abline(regressao3, col = "red")

regressao4 <- lm(dados.mg$renda.pc ~ dados.mg[,5])
summary(regressao4)
plot(dados.mg$renda.pc ~ dados.mg[,5])
abline(regressao4, col = "red")

regressao5 <- lm(dados.mg$renda.pc ~ dados.mg[,6])
summary(regressao5)
plot(dados.mg$renda.pc ~ dados.mg[,6])
abline(regressao5, col = "red")

regressao6 <- lm(dados.mg$renda.pc ~ dados.mg[,7])
summary(regressao6)
plot(dados.mg$renda.pc ~dados.mg[,7])
abline(regressao6, col = "red")

regressao7 <- lm(dados.mg$renda.pc ~ dados.mg[,8])
summary(regressao7)
plot(dados.mg$renda.pc ~ dados.mg[,8])
abline(regressao7, col = "red")

regressao8 <- lm(dados.mg$renda.pc ~ dados.mg[,9])
summary(regressao8)
plot(dados.mg$renda.pc ~ dados.mg[,9])
abline(regressao8, col = "red")

regressao9 <- lm(dados.mg$renda.pc ~ dados.mg[,10])
summary(regressao9)
plot(dados.mg$renda.pc ~ dados.mg[,10])
abline(regressao9, col = "red")

regressao10 <- lm(dados.mg$renda.pc ~ dados.mg[,11])
summary(regressao10)
plot(dados.mg$renda.pc ~ dados.mg[,11])
abline(regressao10, col = "red")

regressao11 <- lm(dados.mg$renda.pc ~ dados.mg[,12])
summary(regressao11)
plot(dados.mg$renda.pc ~ dados.mg[,12])
abline(regressao11, col = "red")

regressao12 <- lm(dados.mg$renda.pc ~ dados.mg[,13])
summary(regressao12)
plot(dados.mg$renda.pc ~ dados.mg[,13])
abline(regressao12, col = "red")

regressao13 <- lm(dados.mg$renda.pc ~ dados.mg[,15])
summary(regressao13)
plot(dados.mg$renda.pc~ dados.mg[,15])
abline(regressao13, col = "red")

#### sele��o backward

regressao.tot <- lm(dados.mg$renda.pc ~ dados.mg$atividade + dados.mg$desocupacao +
                      dados.mg$saneamento + dados.mg$coleta.lixo + dados.mg$energia.eletrica +
                      dados.mg$espepran�a.nascer + dados.mg$PEA + dados.mg$fecundidade +
                      dados.mg$dependencia + dados.mg$t.envelhecimento + dados.mg$t.analf +
                      dados.mg$fundamental.comp + dados.mg$pobres)
summary(regressao.tot)

dados.explicativos <- dados.mg[, c(-1,-14)]
matriz.explicativos <- as.matrix(dados.explicativos)
fit.back.model <- backwardpv(matriz.explicativos, dados.mg$renda.pc, alpha = 0.05)
summary(fit.back.model$`Sele��o Backward`)
fit.back.model$Hist�rico

#### sele��o forward

fit.forward.model <- forwardpv(dados.mg$renda.pc, matriz.explicativos, alpha = 0.05, p = 13)
summary(fit.forward.model$`Sele��o Forward`)
fit.forward.model$Hist�rico

###### REGRESSAO MULTIVARIADA #####
dados.resposta <- dados.mg[,c(7,14)] #expectativa de vida e renda pc
matriz.resposta <- as.matrix(dados.resposta)
dados.explicativos.multi <- dados.mg[,c(3,8,12)]
matriz.explicativos.multi <- as.matrix(dados.explicativos.multi)
regressao.multi <- lm(matriz.resposta ~ matriz.explicativos.multi)
summary(regressao.multi)

#SIMULACAO SIMPLES PRA CRIA��O DA MARIZ GRANDE
rm(x3)
n <- 100
q <-2
p <- 3
x1 <- rmvnorm(n, mean = c(4,5,6))
x2 <- rmvnorm(n, mean = c(1,2,3))
x3 <- rmvnorm(n, mean = c(6,7,8))
x <- cbind(x1,x2,x3)
x1_vet <- rbind(cbind(t(x[1,]), t(rep(0, p))), cbind(t(rep(0, p)),t(x[1,])))
x2_vet <- rbind(cbind(t(x[2,]), t(rep(0, p))), cbind(t(rep(0, p)),t(x[2,])))
x3_vet <- rbind(cbind(t(x[3,]), t(rep(0, p))), cbind(t(rep(0, p)),t(x[3,])))
x4_vet <- rbind(cbind(t(x[4,]), t(rep(0, p))), cbind(t(rep(0, p)),t(x[4,])))
x_total <- matrix(0, ncol = 24, nrow = 8)
x_total <- bdiag(x1_vet,x2_vet,x3_vet,x4_vet)
x_total <- rbind(x1_vet,x2_vet,x3_vet,x4_vet)


dim(x_total)
head(x)
x=rmvnorm(n=n,mean=runif(p,-10,10))
X <- list()
beta=matrix(runif(q*p,-0.5,0.5))
y <- matrix(0, n, q)

for (i in 2:n) {
  X[[i]] <- rbind(cbind(t(x[i,]), t(rep(0, p))), cbind(t(rep(0, p)),t(x[i,]))) 
  mu.i <- X[[i]]%*%beta
  y[i,] <- as.vector(mu.i) + rmvnorm(1, mean = rep(0,q), sigma = diag(runif(q, 0.5, 1.5)))
}


#erro=rmvnorm(n*q, mean = rep(0,q), sigma = diag(runif(q, 0.5, 1.5)))
#dim(erro)
#dim(X)
#dim(beta_1)
#y <-X%*%beta + erro
#y1 <- array()
#y2 <- array()
#y3 <- array()
#y4 <- array()
#y1 <- x1_vet%*%beta_1 + erro1
#y2 <- x2_vet%*%beta_1 + erro2
#y3 <- x3_vet%*%beta_1 + erro3
#y4 <- x4_vet%*%beta_1 + erro4
#y_matrix <- matrix(c(y1,y2,y3,y4), n,q)

#y_bind <- cbind(y1,y2,y3,y4) #q por n

library(mvtnorm)

bep.matrix <- solve((t(x1_vet)%*%solve(sigma_inicial)%*%x1_vet) +
                      (t(x2_vet)%*%solve(sigma_inicial)%*%x2_vet) +
                      (t(x3_vet)%*%solve(sigma_inicial)%*%x3_vet) +
                      (t(x4_vet)%*%solve(sigma_inicial)%*%x4_vet))
pop.matrix <- (t(x1_vet)%*%solve(sigma_inicial)%*%y1) +
  (t(x2_vet)%*%solve(sigma_inicial)%*%y2) +
  (t(x3_vet)%*%solve(sigma_inicial)%*%y3) +
  (t(x4_vet)%*%solve(sigma_inicial)%*%y4)
beta_1
beta_chapeu.inicial <- bep.matrix%*%pop.matrix
teta0 <- c(as.vector(beta_chapeu.inicial), vech(sigma_inicial))
sigma_chapeu.1 <- (1/n)* ((y1-(x1_vet%*%beta_chapeu.inicial))%*%t((y1-(x1_vet%*%beta_chapeu.inicial)))+
                            (y2-(x2_vet%*%beta_chapeu.inicial))%*%t((y2-(x2_vet%*%beta_chapeu.inicial)))+
                            (y3-(x3_vet%*%beta_chapeu.inicial))%*%t((y3-(x3_vet%*%beta_chapeu.inicial))) +
                            (y4-(x4_vet%*%beta_chapeu.inicial))%*%t((y4-(x4_vet%*%beta_chapeu.inicial))))
sigma_chapeu.1 - t(sigma_chapeu.1)
det(sigma_chapeu.1)
eigen(sigma_chapeu.1)
solve(sigma_chapeu.1)
sigma_inicial <- sigma_chapeu.1
sigma.vetorizado <- vech(sigma_inicial)
xpnd(sigma.vetorizado)
sqrt.sigma <- matrix.sqrt(sigma_inicial) 
sqrt.sigma%*%sqrt.sigma
#########  matriz - pratica
library(Matrix)
library(dplyr)
library(tidyverse)

faz.matriz.grandona <- function(bd.resposta, bd.explica)
{
  #definindo indices
  n <- dim(bd.resposta)[1]
  p <- dim(bd.explica)[2]
  q <- dim(bd.resposta)[2]
  
  nest_data <- bd.explica %>% as_tibble() %>% group_split(row_number(), keep = FALSE) %>% map_df(nest)
  
  vet_list <- nest_data %>% mutate(vet = lapply(data, function(row) {rbind(cbind(row, t(rep(0, p))) %>% rename('1' = x1, '2' = x2, '3' = x3, '4' = 1, '5' = 2, '6' = 3), 
                                                                           cbind(t(rep(0, p)),row) %>% rename('1' = x1, '2' = x2, '3' = x3, '4' = 1, '5' = 2, '6' = 3))} ))
  
  unnest_data <- vet_list$vet %>% map(unnest)
  
  vet_matrix_list <- lapply(unnest_data, function(x) as.matrix(x))
  
  #bdiag(vet_matrix_list)
  rbind(vet_matrix_list)
}
matriz.grandona.test <- faz.matriz.grandona(y_matrix, x)

matriz.grandona <- faz.matriz.grandona(dados.resposta, dados.explicativos.multi %>% rename(x1 = desocupacao, x2 = PEA, x3 = t.analf))
head(matriz.grandona)


