---
title: 'Wine preference modeling with econometric methods'
author: "Alex Gonzalez"
date: "7 February 2017"
output:
  html_document: default
  pdf_document: default
---

## Introduction

This project uses the data from the study _Modeling wine preferences by data mining from physicochemical properties_ (Cortez _et al._, 2009). The file was found at the UCI Machine Learning Repository and is available at https://archive.ics.uci.edu/ml/datasets/Wine+Quality.

The problem of the dataset is to predict the quality of a certain wine, in a scale from 1 to 10, based on certain physicochemical features.

```{r global_options, include = FALSE, warning = FALSE, message = FALSE}

require(MASS)
require(leaps)
require(glmnet)
require(pls)
require(lars)
require(caret)
require(selectiveInference)
require(covTest)
require(knitr)

set.seed(1)
```


```{r}
setwd("C:/Users/Alex/Documents/R/Datasets")
wine = read.csv("winequality-white.csv", header = T, sep = ";")
attach(wine)
```

First off, let us divide the dataset into training and validation samples with a 70/30 split.

```{r}
set.seed(2)
train = sample(1:nrow(wine), nrow(wine) * 0.7)
test = (-train)
wine.train = wine[train,]
wine.test = wine[test,]
```

## Estimation of the models

Let us estimate a wide range of models for this data, and supervise their performance.

#### Ordinal Least Squares

Let us fit a regular least squares model first and see how it performs.

```{r}
set.seed(1)
ols = lm(quality ~ ., data = wine, subset = train)
ols
```

The MSE for this model is:

```{r}
ols.pred = predict(ols, wine.test)
mse.ols = mean((wine.test[,"quality"] - ols.pred)^2)
mse.ols
```

#### Best Subset selection

I shall use the following function, found in the lectures:

```{r echo = FALSE, include = FALSE}
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object,id = id)
  mat[,names(coefi)] %*% coefi
}
```


```{r}
set.seed(1)
folds = sample(rep(1:10,length = nrow(wine.train)))
cv.errors = matrix(NA, 10, 11)
for(k in 1:10) {
  best.fit = regsubsets(quality ~ ., data = wine.train[folds != k,], nvmax = 11)
  for(i in 1:11){
    pred = predict.regsubsets(best.fit, wine.train[folds == k,], id = i)
    cv.errors[k,i] = mean((wine.train$quality[folds == k] - pred)^2)
  }
}
```

The MSE of this model is:

```{r}
mse.cv = apply(cv.errors, 2, mean)
plot(mse.cv, pch = 19, type = "b")
abline(v = which.min(mse.cv), lty = "dashed")
mse.ss = mse.cv[which.min(mse.cv)]
mse.ss
```

The model with lower error score has only 8 variables.

```{r}
best.fit = regsubsets(quality ~ ., data = wine.train, nvmax = 8)
summary(best.fit)
ss.pred = predict.regsubsets(best.fit, wine.test, id = 8)
mse.ss = mean((wine.test[,"quality"] - ss.pred)^2)
mse.ss
```

There were 3 variables removed as a result of this algorithm, namely ``citric.acid``, ``chlorides`` and ``total.sulfur.dioxide``.

#### Forward Selection approach

```{r}
set.seed(2)
cv.errorsfw = matrix(NA, 10, 11)
for(k in 1:10){
  best.fitfw = regsubsets(quality ~ ., data = wine.train[folds != k,], nvmax = 11, method = "forward")
  for(i in 1:11){
    pred = predict.regsubsets(best.fitfw, wine.train[folds == k,], id = i, method = "forward")
    cv.errorsfw[k,i] = mean((wine.train$quality[folds == k] - pred)^2)
  }
}
mse.cv.fw = apply(cv.errorsfw, 2, mean)
plot(mse.cv.fw, pch = 19, type = "b")
abline(v = which.min(mse.cv.fw), lty = "dashed")
```

The Forward Selection algorithm also suggests a model with 8 features, namely:

```{r}
best.fitfw = regsubsets(quality ~ ., data = wine.train[folds != k,], nvmax = 8, method = "forward")
summary(best.fitfw)
```

The excluded variables are the same than in the exhaustive selection algorithm. The MSE of the model is:

```{r}
fw.pred = predict.regsubsets(best.fitfw, wine.test, id = 8)
mse.fw = mean((wine.test[,"quality"] - fw.pred)^2)
mse.fw
```


#### Backwards Selection approach

```{r}
set.seed(2)
cv.errorsbw = matrix(NA, 10, 11)
for(k in 1:10){
  best.fit = regsubsets(quality ~ ., data = wine.train[folds != k,], nvmax = 11, method = "backward")
  for(i in 1:11){
    pred = predict.regsubsets(best.fit, wine.train[folds == k,], id = i, method = "backward")
    cv.errorsbw[k,i] = mean((wine.train$quality[folds == k] - pred)^2)
  }
}
mse.cv.bw = (apply(cv.errorsbw,2,mean))
plot(mse.cv.bw, pch = 19, type = "b")
abline(v = which.min(mse.cv.bw), lty = "dashed")
mse.bw = mse.cv.bw[which.min(mse.cv.bw)]
mse.bw
```

Curiously, the best model is achieved again with only 8 features.

```{r}
best.fitbw = regsubsets(quality ~ ., data = wine.train[folds != k,], nvmax = 8, method = "backward")
summary(best.fitbw)
```

``citric.acid``, ``chlorides`` and ``total.sulfur.dioxide`` were excluded for the model again. This gives a strong sense of insignificance for these feature. The Mean Squared Error in this case is:

```{r}
bw.pred = predict.regsubsets(best.fitbw, wine.test, id = 8)
mse.bw = mean((wine.test[,"quality"] - bw.pred)^2)
mse.bw
```

#### Ridge Regression

```{r}
set.seed(101)
x = model.matrix(quality ~ ., wine)[,-1]
y = wine$quality
x.train = x[train,]

ridge.mod = glmnet(x[train,], y[train], alpha = 0)
plot(ridge.mod, label = TRUE)
cv.ridge = cv.glmnet(x, y, alpha = 0)
plot(cv.ridge)
bestlambda = cv.ridge$lambda.min
bestlambda
```

The MSE for the Ridge Regression would be:

```{r}
ridge.pred = predict(ridge.mod, s = 4, newx = x[test,])
mse.ridge = mean((ridge.pred - y[test])^2)
mse.ridge
```

#### LASSO model

```{r}
set.seed(2)
fit.lasso = glmnet(x[train,], y[train], alpha = 1)

cv.lasso = cv.glmnet(x, y, alpha = 1, standardize = FALSE)
plot(cv.lasso) ; coef(cv.lasso)
lasso.bestlam = cv.lasso$lambda.1se
lasso.bestlam
coef(fit.lasso, s = lasso.bestlam)
```

We can see that this model dismisses ``density``, ``pH``, ``citric.acid`` and ``total.sulfur.dioxide``. However, if the best tuning parameter is used, all features are included.

```{r}
set.seed(1)
coefs = predict(fit.lasso, type = "coefficients", s = lasso.bestlam)[1:11,]
length(which(coefs != 0))
```

9 coefficients are selected in the model with the best value (selected by means of cross-validation) for the tuning parameter lambda. The MSE of the LASSO model is:

```{r}
set.seed(1)
lasso.pred = predict(fit.lasso, s = lasso.bestlam, newx = x[test,])
mse.lasso = mean((lasso.pred - y[test])^2)
mse.lasso
```

#### Principal Components regression

```{r}
set.seed(101)
x = model.matrix(quality ~ ., wine)[,-1]
y = wine$quality
x.train = x[train,]

pcr.mod = prcomp(x.train)
plot(pcr.mod, type = "lines", main = "Percentage of explained variance")
```

It seems that three or four components explain most of the variance. Let us go with 3 components for the sake of parsimony. Their coefficients are as follows:

```{r}
pcr.lm = lm(y[train] ~ pcr.mod$x[,1] + pcr.mod$x[,2] + pcr.mod$x[,3])
sm = summary(pcr.lm)
as.table(sm$coefficients[,1])
```

The MSE of this model is:

```{r warning = FALSE}
pcr.pred = predict(pcr.lm, as.data.frame(x[test,]))
mse.pcr = mean((pcr.pred - y[test])^2)
mse.pcr
```


#### Partial Least Squares model

```{r}
set.seed(1)
pls.fit = plsr(quality ~ ., data = wine, subset = train, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP", xlab = "Number of Principal Components", main = "Partial Least Squares Regression")
abline(v = 6, lty = "dashed")
pls.fit = plsr(quality ~ ., data = wine, subset = train, scale = TRUE, ncomp = 6)
summary(pls.fit)
```

The optimal value is 6 components, therefore the MSE for the PLS model is:

```{r}
pls.fit = plsr(quality ~ ., data = wine, subset = train, ncomp = 6)

pls.pred = predict(pls.fit, x[test,], ncomp = 6)
mse.pls = mean((pls.pred - y[test])^2)
mse.pls
```


#### _Least Angle Regression (LAR)_


```{r}
lar.mod = lars(x[train,], y[train], type = "lar", trace = FALSE)
summary(lar.mod)
plot(lar.mod, xvar='step', lwd=2, lty='solid', breaks = FALSE, col = 1:(ncol(x)), main = "Least Angle Regression")

set.seed(1)
cv.lar = cv.lars(x[train,], y[train], plot.it = TRUE, type = "lar", index = 1:11)
idx.lar = which.max(cv.lar$cv - cv.lar$cv.error <= min(cv.lar$cv))
idx.lar
```

```{r}
lar.pred = predict.lars(lar.mod, s = idx.lar ,newx = x[-train,], type = "fit", mode = "step")
mse.lar = mean((lar.pred$fit - y[test])^2)
mse.lar
which((coef(lar.mod)[idx.lar,] != 0))
```

#### _Incremental Forward Stagewise Regression (FS)_

```{r}
fs.mod = lars(x[train,], y[train], type = "forward")
plot(fs.mod, xvar = "step", lwd = 2, lty = "solid", breaks = FALSE, col = 1:ncol(x))

coef(lars(x[train,], y[train], type = "forward"))[idx.lar,]

set.seed(1)
cv.fs = cv.lars(x[train,], y[train], plot.it = TRUE, type = "forward")
idx.fs = cv.fs$index[which.min(cv.fs$cv)]
idx.fs

fs.pred = predict.lars(fs.mod, newx = x[test,], s = idx.fs, type = "fit", mode = "fraction")

mse.fs = mean((fs.pred$fit - y[test])^2)
```

The MSE for the FSe model is:

```{r}
mse.fs
```

#### _LASSO: Elastic Net_


```{r}
# Let's create a grid of lambdas and alphas to try them out and find the best combination
set.seed(1)
lambda.grid = 10^seq(2,-2, length = 100)
alpha.grid = seq(0,1, length = 10)

Control = trainControl(method = "repeatedcv", number = 10)
myGrid = expand.grid(.alpha = alpha.grid, .lambda = lambda.grid)
```

```{r}
set.seed(1)
myTrain = train(quality ~ ., data = wine, method = "glmnet", tuneGrid = myGrid, trControl = Control, standardize = TRUE, maxit = 10000, subset = train)

plot(myTrain)
myTrain$bestTune
elastic.bestlam = myTrain$bestTune[,1]

# According to the standard deviation rule, we will choose the following model:
sd.cv = sd(myTrain$results$RMSE)
sdRule = which.max(myTrain$results$RMSE - sd.cv <= myTrain$bestTune)
myTrain$results[sdRule,]
```



Now, let us use the best combination of _alpha_ and _lambda_ to fit our LASSO model.

```{r}
myModel = myTrain$finalModel
bestModel = glmnet(x, y, alpha=myTrain$bestTune$alpha, lambda = myTrain$bestTune$lambda)
cbind(coef(bestModel, s = myTrain$bestTune$lambda), coef(myModel, s = myTrain$bestTune$lambda))
```

```{r}
set.seed(1)
elastic.pred = predict(bestModel, s = myTrain$bestTune$lambda, newx = x[test,])

mse.elastic = mean((elastic.pred - y[test])^2)
mse.elastic
```


```{r}
errors = matrix(c(mse.ols, mse.ss, mse.fw, mse.bw, mse.ridge, mse.lasso, mse.pcr, mse.pls, mse.lar, mse.fs, mse.elastic), ncol = 11, nrow = 1)
colnames(errors) = c("1. ols", "2. subset", "3. fwd", "3. bwd", "5. ridge", "6. lasso", "7. pcr", "8. pls", "9. lar", "10. fwd step", "11. elasticnet") ; rownames(errors) = "MSE"
errors

which.min(errors)
```
The LASSO model with Elastic Net modification happens to be the best model so far, with the lowest error score.

## Statistical inference

### Ordinal Least Squares

```{r}
summary(ols)
```

The Adjusted R-squared coefficient is 0.2755, which is a very low value. Moreover, we can see that not all variables are significant. Hence, I will remove ``total.sulfur.dioxide``, ``citric.acid`` and ``chlorides`` from the estimation.

```{r}
set.seed(1)
ols = lm(quality ~ . - total.sulfur.dioxide - citric.acid - chlorides - alcohol, data = wine, subset = train)
ols
summary(ols)
```

The Adjusted R-squared is now 0.2756, a marginal increase for the removal of four variables. Let us calculate the MSE for the new model:

```{r}
ols.pred = predict(ols, wine.test)
mse.ols = mean((wine.test[,"quality"] - ols.pred)^2)
mse.ols
```

Removing these two variables increases our MSE slightly, therefore giving us a model with lower performance.

### Best Subset Selection

The variables removed in the final model are ``citric.acid``, ``chlorides`` and ``total.sulfur.dioxide``. Let us run a linear regression without these features and check for significance:

```{r}
ss.ols = lm(quality ~ . - citric.acid - chlorides - total.sulfur.dioxide, data = wine, subset = train)
summary(ss.ols)
ss.ols.pred = predict(ss.ols, wine.test)
mse.ss.ols = mean((wine.test[,"quality"] - ss.ols.pred)^2)
mse.ss.ols
```

```{r}
cbind(coef(best.fit, i = 8), coef(ss.ols))
```

As one can see from the output of the model, all of the features included are significant. Also, as expected, the MSE is the same in both models, as well as the variable coefficients.

### Forward and backward Selection approaches

Since both this methods select the same variables that the exhaustive algorithm chooses, inference for backward and forward subset selection will be skipped to avoid redundancy.

### Ridge regression

```{r warning = FALSE}
set.seed(1)
xinf = scale(x, center = TRUE, scale = TRUE)
yinf = scale(y, center = TRUE, scale = TRUE)
ridgebeta = coef(ridge.mod, s = bestlambda / nrow(yinf), exact = TRUE)[-1]
rs = fixedLassoInf(xinf[train,], yinf[train], ridgebeta, bestlambda)
rs
```

Let us remove variables 3, 5 and 7.

```{r warning = FALSE}
set.seed(1)
ridgex = xinf[, -c(3, 5, 7)]
fit.ridgex = glmnet(ridgex[train,], yinf[train], alpha = 0)
cv.ridgex = cv.glmnet(ridgex, yinf, alpha = 0, standardize = FALSE)
ridgex.bestlam = cv.ridgex$lambda.1se

ridgebetax = coef(fit.ridgex, s = ridgex.bestlam / nrow(yinf), exact = TRUE)[-1]
rs = fixedLassoInf(ridgex[train,], yinf[train], ridgebetax, ridgex.bestlam)
rs
```

All variables of this reduced model are significant. Let us check its performance by means of MSE.

```{r}
set.seed(1)
ridgex.pred = predict(fit.ridgex, s = ridgex.bestlam, newx = ridgex[test,])
mse.ridgex = mean((ridgex.pred - yinf[test])^2)
mse.ridgex
```

### LASSO model

Let us try using selective inference to manually remove insignificant variables. It is important to standardize the variables beforehand.

```{r warning = FALSE}
set.seed(1)
xinf = scale(x, center = TRUE, scale = TRUE)
yinf = scale(y, center = TRUE, scale = TRUE)
beta = coef(fit.lasso, s = lasso.bestlam / nrow(yinf), exact = TRUE)[-1]
rs = fixedLassoInf(xinf[train,], yinf[train], beta, lasso.bestlam)
rs
```

Let us remove variables 3, 5 and 7.

```{r warning = FALSE}
set.seed(1)
lassox = xinf[, -c(3, 5, 7)]
fit.lassox = glmnet(lassox[train,], yinf[train], alpha = 1)
cv.lassox = cv.glmnet(lassox, yinf, alpha = 1, standardize = FALSE)
lassox.bestlam = cv.lassox$lambda.1se

betax = coef(fit.lassox, s = lassox.bestlam / nrow(yinf), exact = TRUE)[-1]
rs = fixedLassoInf(lassox[train,], yinf[train], betax, lassox.bestlam)
rs
```

All variables of this reduced model are significant. Note that the Ridge model came to the same conclusions as the LASSO.

Let us check its performance by means of MSE.

```{r}
set.seed(1)
lassox.pred = predict(fit.lassox, s = lassox.bestlam, newx = lassox[test,])
mse.lassox = mean((lassox.pred - yinf[test])^2)
mse.lassox
```

The model with manual variable selection has an MSE value of 0.7265, which is higher than the value of the first model, as expected.


### Principal Components regression

```{r warning = FALSE}
pcr.fit = lm(y[train] ~ pcr.mod$x[,1] + pcr.mod$x[,2] + pcr.mod$x[,3])
pcr.pred = predict(pcr.fit, as.data.frame(x[test,]))
mse.pcr = mean((pcr.pred - y[test])^2)
mse.pcr
```

### Partial Least Squares model

```{r warning = FALSE}
scores = as.matrix(pls.fit$scores)

pls.mod = lm(y[train] ~ scores)
pls.pred = predict(pls.fit, x[test,])
mse.pls = mean((pls.pred - y[test])^2)
summary(pls.mod)
mse.pls
```

Since the 5th component is not significant, we will remove components 5 and 6 because the fifth component is used to calculate the sixth.

```{r}
scores = as.matrix(pls.fit$scores[,1:4])

pls.mod = lm(y[train] ~ scores)
pls.pred = predict(pls.fit, x[test,])
mse.pls = mean((pls.pred - y[test])^2)
summary(pls.mod)
mse.pls
```

Now all coefficients are significant, although the model has worsened slightly.

### _Least Angle Regression (LAR)_

```{r}
set.seed(1)
lar.fit = lar(x, y)
plot(lar.fit, xvar = "step")

lar.aic = larInf(lar.fit, type = "aic")
lar.aic
```

Since the AIC rule suggests considering all variables, the value of the MSE will be the same as in the previous section:

```{r}
mse.lar
```


### _Incremental Forward Stagewise Regression (FS)_

```{r}
ifsr = fs(x[train,], y[train])
fsInf(ifsr, type = "aic")
```

The Akaike Information Criterion suggests only 8 steps before stopping the process. Let us see the results for a significance threshold of 5%:

```{r}
steps_5percents = forwardStop(fsInf(ifsr)$pv, alpha = 0.05)
cat(c("Estimated stopping point from P-value lower than 0.05 rule = ", steps_5percents))
```

```{r}
fs_aic = mean((predict(ifsr, s = 12, newx = x[-train,], type="fit") - y[test])^2)
fs_5pc = mean((predict(ifsr, s = 5, newx = x[-train,], type="fit") - y[test])^2)

fs_table = cbind(fs_aic, fs_5pc)
colnames(fs_table) = cbind("FS - AIC rule", "FS - 5% rule")
rownames(fs_table) = "MSE"
fs_table
```

There is a significant difference between the values, so it would be wise to follow the 5% significance rule and achieve a more precise and simpler model.

### _LASSO: Elastic Net_

```{r warning = FALSE}
set.seed(1)
xinf = scale(x, center = TRUE, scale = TRUE)
yinf = scale(y, center = TRUE, scale = TRUE)
elasticbeta = coef(fit.lasso, s = lasso.bestlam / nrow(yinf), exact = TRUE)[-1]
rs = fixedLassoInf(xinf[train,], yinf[train], elasticbeta, elastic.bestlam)
rs
```

Again, it looks like the variable 3, 5 and 7 are not significant in this dataset.

```{r warning = FALSE}
set.seed(1)
elasticx = xinf[, -c(3, 5, 7)]
fit.elasticx = glmnet(elasticx[train,], yinf[train], alpha = elastic.bestlam)
cv.elasticx = cv.glmnet(elasticx, yinf, alpha = elastic.bestlam, standardize = FALSE)
elasticx.bestlam = cv.elasticx$lambda.1se

betax = coef(fit.elasticx, s = elasticx.bestlam / nrow(yinf), exact = TRUE)[-1]
rs = fixedLassoInf(elasticx[train,], yinf[train], betax, elasticx.bestlam)
rs
```

All variables of this reduced model are significant. Much like the LASSO and Ridge models, this method chooses the same variables.

Let us check its performance by means of MSE.

```{r}
set.seed(1)
elasticx.pred = predict(fit.elasticx, s = elasticx.bestlam, newx = elasticx[test,])
mse.elasticx = mean((elasticx.pred - yinf[test])^2)
mse.elasticx
```

#### Coefficients

```{r include = FALSE, echo = FALSE}
to_dgCMatrix = function(matA, matB = OLS.coef) {
  values = cbind(matA)
  A = rownames(values)
  B = rownames(matB)
  BminusA = subset(B, !(B %in% A))
  zeros = rep(0, length(BminusA))
  values = c(values, zeros)
  names(values) = c(A, BminusA)
  return(as(as.matrix(values), "dgCMatrix"))
}
```


```{r}
OLS.coef = coef(ols)
BSS.coef = coef(best.fit, 3)
BSR.coef = coef(best.fitbw, 3)
FSS.coef = coef(best.fitfw, 3)
RIDGE.coef = predict(glmnet(x, y, alpha = 0), type = "coefficients", s = bestlambda)
LASSO.coef = coef(fit.lasso, s = lasso.bestlam)
```

```{r warning = FALSE}
coef.matrix = matrix(NA, nrow = 14, ncol = 1)

rownames(coef.matrix) = c("(Intercept)", "fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol", "quality", "MSE")


coef.matrix = cbind(OLS.coef, BSS.coef, BSR.coef, FSS.coef,
                    RIDGE.coef, LASSO.coef, sm$coefficients[,1], coef(pls.mod), 
                    coef(lar.mod)[idx.lar,], coef(fs.mod, s = idx.fs, mode = "fraction"),
                    coef(myTrain$finalModel, s = myTrain$bestTune$lambda))



colnames(coef.matrix) = c("1. ols", "2. subset", "3. fwd", "3. bwd", "5. ridge", "6. lasso",
                          "7. pcr", "8. pls", "9. lar", "10. fwd step", "11. elasticnet")

as.data.frame(as.matrix(rbind(coef.matrix, errors)))
```


