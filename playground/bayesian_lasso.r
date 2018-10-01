## LASSO BAYESIANO
rm(list =ls())
##load("hiv.rda")
##204 binary attributes and 704 observations
library(monomvn)
data(diabetes)
attach(diabetes)
## Ordinary Least Squares regression
reg.ols <- regress(x, y)
## Lasso regression
reg.las <- regress(x, y, method="lasso")
## Bayesian Lasso regression. T: N�m de Muestras a Posteriori
## thin: number of MCMC samples to skip before a sample is collected (via thinning)
reg.blas <- blasso(x, y, T= 2000, thin = 50)
## summarize the beta (regression coefficients) estimates
plot(reg.blas, burnin=200)
points(drop(reg.las$b), col=2, pch=20)
points(drop(reg.ols$b), col=3, pch=18)
legend("topleft", c("blasso-map", "lasso", "lsr"), col=c(2,2,3), pch=c(21,20,18))
## plot the size of different models visited
plot(reg.blas, burnin=200, which="m")
## get the summary
s <- summary(reg.blas, burnin=200)
s$coef
## Residual Variance
s$s2
## calculate the probability that each beta coef != zero
s$bn0
## A summary of the model order
s$m
## Lambda
s$lambda2

### BOOTSTRAP
par(mfrow=c(1,1))
library(glmnet)
require(boot)
##X <- model.matrix(y~.,data=diabetes)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
fit1=glmnet(x,y)
coef(fit1,s=bestlam) # extract coefficients at a single value of lambda
## Funci�n que extrae los coeficientes de LASSO dado el mejor lambda de VC
## Muestra de bootstrap
sample(1:10, size = 10, replace = T)
set.seed(44)
B <- 1000
betas <- matrix(0,B,11)
lambdas <- rep(0, B)
n.x <- nrow(x)
for (i in 1:B){
  mi.muestra <- sample(nrow(x), size = n.x, replace = T)
  x.b <- x[mi.muestra,]
  y.b <- y[mi.muestra]
  cv.out=cv.glmnet(x.b,y.b,alpha=1)
  fit1=glmnet(x.b,y.b,alpha=1)
  coefs <-coef(fit1,s=cv.out$lambda.min)
  betas[i,] <- coefs[,1]
  lambdas[i] <- cv.out$lambda.min
}
boxplot(betas)
abline(h=0, col = "green", pch =20)
media.betas <- apply(betas, 2, mean)
points(drop(media.betas), col=3, pch=18)
sd.betas <- apply(betas, 2, sd)
limite.inferior <- apply(betas, 2, quantile, probs =0.025, na.rm = FALSE)
limite.superior <- apply(betas, 2, quantile, probs =0.975, na.rm = FALSE)
intervalo.percentil = cbind(limite.inferior,limite.superior)
rownames(intervalo.percentil) <- rownames(coefs)
intervalo.percentil
mean(lambdas)
