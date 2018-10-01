---
output: html_document
---
Ejercicio sobre SVD
=======================


SVD
-----------------------------------
Vamos a generar una matriz y ajustar un SVD para completar sus valores faltantes:
```{r}
require(softImpute)
set.seed(1011)
x=matrix(rnorm(30),6,5)
x[sample(1:30,10,replace=FALSE)]=NA
x
fits=softImpute(x,trace=TRUE,type="svd")
fits
ximp=complete(x, fits)
ximp
x
fita=softImpute(x,trace=TRUE)
fita
ximp2=complete(x, fita)
ximp2
```
Vamos a generar una matriz grande, de 100 clientes de Netflix que disponen de 17,700 pel?culas y con 90% de los ratings faltantes:
```{r}
set.seed(08112016)
n=17700
p=100
J=50
np=n*p
missfrac=0.9
x=matrix(rnorm(n*J),n,J)%*%matrix(rnorm(J*p),J,p)+matrix(rnorm(np),n,p)/5
dim(x)
ix=seq(np)
length(ix)
imiss=sample(ix,np*missfrac,replace=FALSE)
xna=x
xna[imiss]=NA
tail(xna)


lam0=lambda0(xna)
lam0
xnaC=as(xna, "Incomplete")
xsc = biScale(xnaC, col.scale=FALSE, row.scale=FALSE, trace=TRUE)
# Hard-Impute:
hard=softImpute(xsc, lambda=lam0, trace=TRUE, type="svd", maxit=1000)
x.hard=complete(xsc, hard, unscale=TRUE)
head(x.hard)

# Algoritmo ganador: Hard-Impute con restricci?n  de Lasso:

soft=softImpute(xsc, lambda=lam0, trace=TRUE,type="als",maxit=1000)
x.soft=complete(xsc,soft,unscale=TRUE)
head(x.soft)
head(x.hard)
head(xsc)

# No fiarse de que esto est? bien, porque si haces:
x.soft-x.hard
# Te sale una matriz de ceros porque ambos son iguales.








```
