load.libraries <- c('gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'tidyverse', 'ggthemes', 'reshape2')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)

sapply(load.libraries, require, character = TRUE)

bank <- read.csv("./data/bank-full.csv",  sep = ";", na.strings = "unknown")
bank <- na.omit(bank)
index <- 1:nrow(bank)
test_idx <- sample(index, trunc(length(index) * 30 / 100))
test_set <- bank[test_idx,]
train_set <- bank[-test_idx,]

table(train_set$y) / length(train_set$y)
table(test_set$y) / length(test_set$y)


svm_model <- svm(y ~ ., data = train_set)

# Summary of the model

summary(svm_model)

# You will notice the SVM Type classification is used with radial kernel. We can change kernal to be used using 
# kernel = parameter option. Also, scale = TRUE could be used to scale the input variables.
# It also gives the information on number of support vectors for the SVM model.

## Predict using SVM
# The -2 is because the label column to intance classes, V2, is in the second column.

prediction <- predict(svm_model, test_set[,-17])

## Compare Observed and Predicted

table_svm <- table(pred = prediction,
                   B = test_set$y) / length(prediction)

table.svm<-table(pred = prediction,
                 B = test_set$y)/length(prediction)

table_svm

# The classification accuracy is around 98%. And 56% of the predicted B are also observed B.

# To produce the confusion matrix: 

tab <- table(pred = prediction, true = test_set[,17])

tab

# Model accuracy rates can be computed using the classAgreement() function:

classAgreement(tab)

# This means that there are 72 malign instances in test set and all of them were predicted as malign instances. 
# On the other hand, there are 98  bening instances in test set, 96 were predicted rightly and 2 as maling instances.

# Also we can obtain the confusion matrix using caret package in R, using function confusionMatrix
# Output of confusionMatrix has a number of different statistics. In the example, 98.8% is prediction accuracy.

tab1<-confusionMatrix(prediction, test_set$y)

tab1

## PARAMETER TUNING ##
## PARAMETER TUNING ##
## PARAMETER TUNING ##

# using function from the lectures

gamma <- c(10 ^ seq(-5, -1,
                    by = 1))
cost <- c(10 ^ seq(-2, 1,
                   by = 1))
params <- expand.grid(cost = cost, gamma = gamma)

total_accuracy_svm <- function(train_set, test_set) {
  accuracy1 <- NULL
  accuracy2 <- NULL
  for (i in 1:NROW(params)) {
    learn_svm <-
      svm(
        y ~ .,
        data = train_set,
        gamma = params$gamma[i],
        cost = params$cost[i]
      )
    pred_svm <- predict(learn_svm, test_set[, -17])
    accuracy1 <- confusionMatrix(pred_svm, test_set$y)
    accuracy2[i] <- accuracy1$overall[1]
  }
  accuracy2
}

c <- total_accuracy_svm(train_set, test_set)
opt_params <- which(c == max(c))[1]

learn_imp_svm <-
  svm(y ~ .,
      data = train_set,
      cost = params$cost[opt_params],
      gamma = params$gamma[opt_params])

pred_imp_svm <- predict(learn_imp_svm, test_set[, -17])
cm_imp_svm <- confusionMatrix(pred_imp_svm, test_set$y)
cm_imp_svm

sml_set <- train_set[sample(nrow(train_set) * 0.75), ]

tuned <- tune.svm(y ~ ., data = sml_set, gamma = gamma, cost = cost)
summary(tuned)

learntune_imp_svm <- svm(y ~ ., data = sml_set, cost = 1, gamma = 0.01)
pretune_imp_svm <- predict(learntune_imp_svm, test_set[, -17])
cmtune_imp_svm <- confusionMatrix(pretune_imp_svm, test_set$y)
cmtune_imp_svm

tuned <- tune.svm(y ~ ., data = train_set, gamma = 10 ^ (-3:-1), cost = 10^(-1:1))
summary(tuned)
learntune_imp_svm <- svm(y ~ ., data = train_set, cost = 1, gamma = 0.01)
pretune_imp_svm <- predict(learntune_imp_svm, test_set[, -17])
cmtune_imp_svm <- confusionMatrix(pretune_imp_svm, test_set$y)
cmtune_imp_svm

colr <- c("#ed3b3b", "#0099ff")
par(mfrow = c(1, 2))

fourfoldplot(tab1$table,
             color = colr,
             conf.level = 0, 
             margin = 1,
             main = paste("SVM (", round(tab1$overall[1] * 100), "%)",sep = ""))

fourfoldplot(cm_imp_svm$table, 
             color = colr, conf.level = 0, 
             margin = 1,
             main = paste("Tuned SVM (", round(cm_imp_svm$overall[1] * 100), "%)",sep = ""))

library("factoextra")

my_data <- select_if(bank, is.numeric)
res.pca <- prcomp(my_data, scale = TRUE)

fviz_pca_biplot(res.pca,
  col.ind = bank$y,
  col = "black",
  palette = "jco",
  geom = "point",
  repel = TRUE,
  legend.title = "Charges",
  addEllipses = TRUE,
  ylim = c(5, -15)
)

## Pending: fit radial Kernel, tune parameters, fit neural network (& logistic, Random Forest,
## decision tree, lda...) as well as ROC curves

library(moments) # for skewness()
for(i in names(adult)){
  if(is.numeric(adult[,i])){
    if(i != "income"){
      # Enters this block if variable is non-categorical
      skewVal <- skewness(adult[,i])
      print(paste(i, skewVal, sep = ": "))
      if(abs(skewVal) > 0.5){
        skewedVars <- c(skewedVars, i)
      }
    }
  }
}
skewedVars


### NEURAL NET ###

library(nnet)
nn <- nnet(y ~ ., data = train_set, size = c(5, 2), maxit = 1000)

nn_pred <- predict(nn, newdata = test_set, type = 'class')


# confusion matrix 
tb1 <- table(nn_pred, test_set$y)
tb1
confusionMatrix(test_set$y, nn_pred)


## DECISION TREE ##

library(rpart)

tree_model <- rpart(y ~ .,
                    data = train_set,
                    method = "class",
                    minbucket = 20)

tree_predict <- predict(tree_model,
                        test_set,
                        type = "class")

confusionMatrix(test_set$y, tree_predict) 

## RANDOM FOREST ##

library(randomForest)
rf <- randomForest(y ~ ., data = train_set, ntree = 1000)
rf_pred.prob <- predict(rf, newdata = test_set, type = 'prob')
rf_pred <- predict(rf, newdata = test_set, type = 'class')

tb3 <- table(rf_pred, test_set$y)
tb3
confusionMatrix(test_set$y, rf_pred)
