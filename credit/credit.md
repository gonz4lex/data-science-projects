---
title: 'Statistical Learning: final project'
author: "Alex González"
date: "12 January 2017"
output:
  html_document: default
  pdf_document: default
---

```{r global_options, include=FALSE, warning=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE, fig.align='center')
```

```{r include=FALSE, echo=FALSE}
require(tree); require(randomForest); require(gbm); require(MASS)
```

## Introduction

The aim of this project is one of supervised learning: given some features, predict whether a credit card transaction is fraudulent or legitimate.

The dataset contains transactions made by credit cards by european cardholders, where we have 492 frauds out of 284,807 transactions. The dataset is highly unbalanced, since the positive class (frauds) accounts for 0.172% of all transactions.,

Therefore, accuracy measures need to be chosen accordingly. The following models will usually output high accuracy, above 99%. However, given this class imbalance this will not be a reliable measure. One solution would be to check the Area Under Receiving Operating Characteristic (ROC) Curve, which is beyond the scope of the course. Hence, we will focus on analysing the Confusion Matrix and monitoring the rate of false negatives, which is the problematic type of error.

Due to confidentiality issues, the original features are masked except ``Time`` and ``Amount``. Variables V1 to V28 are the principal components of the original data obtained with PCA. ``Time`` contains the seconds elapsed between each transaction and the first transaction in the dataset, while ``Amount`` is the transaction amount. ``Class`` is the response variable and it takes value 1 in case of fraud and 0 otherwise.

For more information, refer to Dal Pazzolo __et al.__ (2015).

#### Load and split the data

```{r}
setwd("C:/Users/alex_/Documents/R/Datasets")
credit = read.csv("creditcard.csv", header = T)
```

First off, let us divide the dataset into training and validation samples with a 70/30 split.

```{r}
set.seed(2)
train = sample(1:nrow(credit), nrow(credit) * 0.7)
test = (-train)
```

```{r}
credit$Class = as.factor(credit$Class)
attach(credit)
```

#### Discriminant analysis

Let us check the performance of a simple linear DA:

```{r}
ldaMod = lda(Class ~., data = credit[train,])
ldaPred = predict(ldaMod, credit[-train,])
table(ldaPred$class, Class[-train], dnn = c("Prediction", "True"))
mean(ldaPred$class == Class[-train])
```

As expected, the accuracy score is 99.95%. However, there are 23 false negatives. That is, instances that are predicted as legitimate when they actually are fraudulent. Let us see if a quadratic DA approach can improve these results:

```{r}
qdaMod = qda(Class ~ ., data = credit[train,])
qdaPred = predict(qdaMod, credit[-train,])
table(qdaPred$class, Class[-train], dnn = c("Prediction", "True"))
mean(qdaPred$class == Class[-train])
```

While we have reduced the number of false negatives by 10, the overall score has decreased to 97.6% by introducing over 2000 false positives. One is inclined to assert that the linear approach is much more reliable.

### Cluster Analysis

Even though this is clearly a supervised classification problem, one question comes to mind: can a cluster analysis correctly identify both classes?

```{r}
km = kmeans(credit[train,], 2, nstart = 15)
length(km$cluster == 1) / dim(credit)[1]
```

We can see that one of the clusters contains almost 70% of the observations, when we know the true split is close to 99% / 1%. It seems that this data is not suitable for unsupervised analysis.

### Decision tree

These simple models can usually give very good results:

```{r}
treeMod = tree(Class ~ ., data = credit[train,])
summary(treeMod)
plot(treeMod, type = "uniform")
text(treeMod, pretty = 0)
```


```{r}
treePred = predict(treeMod, newdata = credit[test,], type = "class")
with(credit[test,], table(treePred, Class))
```

The tree only misclassified 18 transactions as legitimate, improving the previous model. Let us try pruning the tree to see if better predictions can be achieved.


```{r}
treeCv = cv.tree(treeMod, FUN = prune.misclass) ## can't prune
plot(treeCv)
```

It seems that we can grow our tree up to 4 terminal nodes to improve the trade-off between accuracy and interpretability.

```{r}
pruneMod = prune.misclass(treeMod, best = 4)
plot(pruneMod); text(pruneMod, pretty = 0)
```

```{r}
prunePred = predict(pruneMod, newdata = credit[test,], type = "class")
with(credit[test,], table(prunePred, Class))
```

This new tree has the same accuracy with slightly less terminal nodes, giving us a simpler model.

### Tree based methods: bagging, boosting and random forest

#### Bagging and random forest

To train a random forest model, some data has to be dropped in order to avoid memory limitations. Therefore, I will choose a smaller subset of the those subjects with ``Class`` = 0. This will help reduce the class imbalance, although reducing the size of the dataset arbitrarily will greatly harm the results.

In practice, other techniques should be used, such as ensembling (training several smaller subsets of the data and averaging the predictions) or training the model in a cloud computing platform.

```{r}
data.class.1 = subset(credit, credit$Class == 1)
data.class.0 = subset(credit, credit$Class == 0)
data.class.0 = data.class.0[1:80000, ]

rfCredit = rbind(data.class.0, data.class.1)
set.seed(2)
rfTrain = sample(1:nrow(rfCredit), nrow(rfCredit) * 0.7)
rfTest = (-rfTrain)
rfCredit$Class = as.factor(rfCredit$Class)
```

When bagging trees, we consider all features for the tree splits:

```{r warning = FALSE}
set.seed(1)
bag = randomForest(Class ~ ., data = rfCredit, subset = rfTrain, mtry = 30)
bag
```

This model outputs 0.06% Out-of-Bag error with 14 false negatives.

In a random forest, only a small subset of variables is considered:

```{r warning = FALSE}
set.seed(1)
rf = randomForest(Class ~ ., data = rfCredit, subset = rfTrain)
rf
```

This Random Forest approach improves the model, with an Out-of-Bag error of 0.05% and only 10 false negatives, which is slightly better than the bagged model
Note that the parameter ``mtry`` is set to 5 by default, although it can be chosen so as to find the best fit. Also, due to processing times and memory limitations, this models only average a small number of trees, namely 500. Ideally, a higher number should be used, since more trees means less variance, which can be beneficial for predicting.

#### Boosting

```{r}
# ## This code will take a while to run:
# set.seed(1)
# ## 10-fold cross-validation
# boostingCv = gbm(Class ~ ., data = rfCredit[rfTrain,], distribution = "bernoulli", n.trees = 100, shrinkage = 0.01, interaction.depth = 4, cv.folds = 5)
# bestIt = gbm.perf(boostingCv, method = "cv")
# 
# par(mfrow = c(2, 2))
# plot(boosting, i = "V12")
# plot(boosting, i = "V17")
# plot(boosting, i = "V14")
# plot(boosting, i = "V10")
```

The most important variables in this dataset are ``V12``, ``V17``, ``V14`` and ``V10``, as seen on the plot. This feature ranking is useful to cut off features that contribute negligibly to the model.


```{r}
# ntrees = seq(100, 1000, 100)
# boostPred = predict(boosting, newdata = rfCredit[rfTest,], n.trees = ntrees)
# summary(boostingCv, n.trees = 500)
```

```{r warning = FALSE}
# set.seed(1)
# table(boostPred, rfCredit$Class[-rfTrain])
```



### Logistic regression

```{r}
logit = glm(Class ~ ., data = credit[train,], family = binomial, maxit = 80)
## Very small residual deviance, probable reason for overfitted probabilities.
logit$deviance
probs = predict(logit, newdata = credit[-train,], type = "response") 
logitPred = ifelse(probs > 0.5, "0", "1")

table(logitPred, credit$Class[-train])
```

The logit model produces overfitted probabilities, which leads to an excessive number of cases identified as fraud. This model is not suitable for the dataset, probably due to the main issue of class imbalance.

### Concluding remarks

Many of the models report unusually high accuracy, due to the nature of the data. Since this problem is common in financial firms looking to identify which transactions are fraudulent, the focus should be on the accuracy predicting the positive class, rather than overall accuracy. In this regard, the bagging and random forest methods feature some of the best results, with only 14 and 10 false negatives and estimated errors of ~0.05%.


### References

Andrea Dal Pozzolo, Olivier Caelen, Reid A. Johnson and Gianluca Bontempi. __Calibrating Probability with Undersampling for Unbalanced Classification__. In Symposium on Computational Intelligence and Data Mining (CIDM), IEEE, 2015.
