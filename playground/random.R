library(caret)
library(randomForest)
library(tree)
library(Amelia)
library(ggthemes)
library(e1071)
library(corrplot)
library(factoextra)
library(ltm)
library(tidyverse)


my_df <- read.csv("./datasets/combined_20180418.csv", header = T, sep = ";", dec = ",")

str(my_df)
my_df$arrival_month <- as.factor(my_df$arrival_month)

summary(my_df)
my_df %>% 
  is.na() %>%
  sum()

my_df %>% 
  group_by(class) %>% 
  summarise(prop = n() / nrow(my_df) * 100)

zero_var <- 
    my_df %>% 
    select_if(is.numeric) %>% 
    nearZeroVar()
  
drop_cols <- c("basic_markup", "reference",
               "client", "hotel", "arrival_date",
               "pax_str", "net_margin")
my_df <- my_df %>% dplyr::select(-one_of(drop_cols))
my_df <- my_df[, -zero_var]


n <- names(my_df)
f <- as.formula(paste("class ~", paste(n[!n %in% "class"], collapse = " + ")))

# prepare training scheme
control <- trainControl(method = "cv",
                        number = 10)
# train the model
mdl <- train(f,
             data = my_df,
             method = "lvq",
             preProcess = "scale",
             trControl = control)
# estimate variable importance
importance <- varImp(mdl, scale = FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
  
# with(my_df,
#      biserial.cor(net_margin,
#                   class,
#                   level = 2))

# define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs,
                      method = "cv",
                      number = 10)
# run the RFE algorithm
results <-  rfe(my_df[, -11],
                my_df[, 11],
                sizes = c(1:10),
                rfeControl = control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results,
     type = c("g", "o"))


lng_data_pos <- 
  my_df %>% 
  filter(class == 'POSITIVE') %>% 
  select_if(is.numeric) %>% 
  gather()

lng_data_neg <- 
  my_df %>% 
  filter(class == 'NEGATIVE') %>% 
  select_if(is.numeric) %>% 
  gather()

ggplot(data = data.frame(), aes(x = value)) +
  geom_density(data = lng_data_pos, fill = '#377EB8', color = '#377EB8', alpha = .3) +
  geom_density(data = lng_data_neg, fill = '#E41A1C', color = '#E41A1C', alpha = .3) +
  facet_wrap( ~ key, scales = 'free') +
  labs(title = "Density",
       subtitle = 'of each numeric variable') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '',values = c(yes = '#377EB8', no = '#E41A1C')) +
  theme_fivethirtyeight()

lng_data_pos <- 
  my_df %>% 
  filter(class == 'POSITIVE') %>% 
  select_if(negate(is.numeric)) %>% 
  gather()

lng_data_neg <- 
  my_df %>% 
  filter(class == 'NEGATIVE') %>% 
  select_if(negate(is.numeric)) %>% 
  gather()

ggplot(data = data.frame(), aes(x = value)) +
  geom_bar(data = lng_data_pos, fill = '#377EB8', color = '#377EB8', alpha = .3) +
  geom_bar(data = lng_data_neg, fill = '#E41A1C', color = '#E41A1C', alpha = .3) +
  facet_wrap( ~ key, scales = 'free') +
  labs(title = "Density",
       subtitle = 'of each numeric variable') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '',values = c(yes = '#377EB8', no = '#E41A1C')) +
  theme_fivethirtyeight()


correlations <- 
  my_df %>%
  select_if(is.numeric) %>% 
  na.omit() %>% 
  cor()

corrplot(correlations, method = "square", tl.cex = .5)


hc = findCorrelation(correlations,
                     cutoff = 0.7)
hc = sort(hc)
uncorr_df = correlations[, -c(hc)] %>% 
  as.data.frame()
rownames(uncorr_df)

uncorr_df <- my_df[, rownames(uncorr_df)]

    
rs_pca <- prcomp(uncorr_df,
                 scale = TRUE)
fviz_pca_biplot(rs_pca,
                col.ind = my_df$class,
                col = "black",
                palette = "lancet",
                geom = "point",
                repel = TRUE,
                legend.title = "Class",
                addEllipses = TRUE)

rs_pca$rotation

var_pca = rs_pca$sdev ^ 2 #sdev of each factor, squared
pve = var_pca / sum(var_pca) #proportion of explained variance. Variance of the PCs
#divided by total variance. Total variance equals the sum of
#of the variances of all variables (it's 1 because they're
#standardized)

pve %>%
  cumsum() %>%
  matrix(dimnames = list(NULL, c("pve"))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = 1:length(pve), y = pve)) +
  geom_point() +
  geom_line(lty = 'dashed')



index <- 1:nrow(my_df)
test_idx <- sample(index, 
                   trunc(length(index) * 30 / 100))
test_set <- my_df[test_idx,]
train_set <- my_df[-test_idx,]

class_prop <- 
  rbind(table(train_set$class) / length(train_set$class),
        table(test_set$class) / length(test_set$class)) %>% 
  round(4)
rownames(class_prop) <- c("Train", "Test")
class_prop


## Tree

library(rpart)

tree_mdl <- rpart(class ~ .,
                 data = train_set,
                 method = "class",
                 minbucket = 20)

tree_pred <- predict(tree_mdl,
                    test_set,
                    type = "class")

confusionMatrix(test_set$class,
                tree_pred) 

tree_mdl <- rpart(class ~ .,
                  data = train_set,
                  method = 'class',
                  cp = 1e-3)

tree_pred <- predict(tree_mdl,
                     newdata = test_set,
                     type = 'class')

tree_prob <- predict(tree_mdl,
                     newdata = test_set,
                     type = 'prob')


confusionMatrix(tree_pred, test_set$class)

## Neural Net

nn_df <- my_df

nn_df$class <- as.integer(as.character(nn_df$class) == "POSITIVE")

test_nn <- nn_df[test_idx,]
train_nn <- nn_df[-test_idx,]


library(neuralnet)
nn_mdl <- neuralnet(class ~ los + cost + income + product_markup + 
                            override_markup + deduction + gross_margin + fees,
                    data = train_nn,
                    hidden = 30,
                    stepmax = 1000,
                    lifesign = 'minimal',
                    linear.output = FALSE,
                    threshold = 0.1)

nn_pred <- predict(nn_mdl,
                   newdata = test_nn,
                   type = 'raw')

nn_pred <- rep('<=50K',
               length(nn_pred))
nn_pred[pred >= .5] <- '>50K'
# confusion matrix 
tb1 <- table(nn_pred, test_set$class)

tb1
confusionMatrix(test_set$class, nn_pred)



## Neural Network

library(randomForest)
rf3 <- randomForest(class ~ ., data = train_set, ntree = 1000)
rf3.pred.prob <- predict(rf3, newdata = test_set, type = 'prob')
rf3.pred <- predict(rf3, newdata = test_set, type = 'class')
# confusion matrix 
tb3 <- table(rf3.pred, test_set$income)
tb3
confusionMatrix(test_set$income, rf3.pred)

## SVM models
### 
svm_model <- svm(class ~ .,
                 data = train_set,
                 kernel = "radial")

summary(svm_model)


prediction <- predict(svm_model, test_set[,-17])

cm_svm <- confusionMatrix(prediction, test_set$class)
cm_svm

## grid search for parameter tuning

gamma <- c(10 ^ seq(-3, -1,
                    by = 1))
cost <- c(10 ^ seq(-2, 2,
                   by = 1))
params <- expand.grid(cost = cost, gamma = gamma)

total_accuracy_svm <- function(train_set, test_set) {
  accuracy1 <- NULL
  accuracy2 <- NULL
  for (i in 1:NROW(params)) {
    learn_svm <-
      svm(
        f,
        data = train_set,
        gamma = params$gamma[i],
        cost = params$cost[i],
        kernel = 'radial'
      )
    pred_svm <- predict(learn_svm, test_set[, -14])
    accuracy1 <- confusionMatrix(pred_svm, test_set$class)
    accuracy2[i] <- accuracy1$overall[1]
  }
  accuracy2
}

c <- total_accuracy_svm(train_set, test_set)
opt_params <- which(c == max(c))[1]

learn_imp_svm <-
  svm(f,
      data = train_set,
      cost = params$cost[opt_params],
      gamma = params$gamma[opt_params],
      kernel = "radial")

pred_imp_svm <- predict(learn_imp_svm, test_set[, -14])
cm_imp_svm <- confusionMatrix(pred_imp_svm, test_set$class)
cm_imp_svm
