## Geração de dados

positiva.definida <- function(p)
{
  mcov <- matrix(runif(p^2, -1,1), ncol = p, nrow = p)
  diag(mcov) <- abs(diag(mcov))
  mcov <- (mcov + t(mcov))/2
  while(sum(eigen(mcov)$values < 0) != 0) {
    mcov <- matrix(runif(p^2, -1,1), ncol = p, nrow = p)
    mcov
    diag(mcov) <- abs(diag(mcov))
    mcov <- (mcov + t(mcov))/2
    
  }
  return(mcov)
}

require(mvtnorm)
n <- 1000
tam <- 5
sigma <- positiva.definida(tam)
x = rmvnorm(n=n,mean = runif(tam,-10,10), sigma = sigma)
x=rmvnorm(n=n,mean=runif(tam,-10,10)) # sigma=matriz identidade
beta=matrix(runif(tam+1,-0.5,0.5))
X=cbind(1,x)
erro=rnorm(n,0,1)
y=X%*%beta + erro
summary(lm(y~x))
fit.model <- forwardpv(y,x,0.05,tam)
summary(fit.model$`Seleção Forward`)
cov(x)
fit.model$Histórico
## Forward
p <- 4
alpha <- 0.05
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
names(lista) <- c("Seleção Forward", "Histórico")
return(lista)
}

