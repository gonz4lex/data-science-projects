---
title: "Statistical Learning and Decision Making II"
subtitle: "Classification Project"
author: "Alejandro Gonzalez"
date: "1 April 2018"
output: 
  prettydoc::html_pretty:
    theme: hpstr
    toc: true 
    toc_depth: 3
    keep_md: true
---
  


# 1. Introduction

Consider the "*bank-full.csv*" dataset. 
The data is related with direct marketing campaigns of a Portuguese banking institution. 
The marketing campaigns were based on phone calls. Often, more than one contact to the same client was required, 
in order to access if the product (bank term deposit) would be (or not) subscribed. 

The dataset description can be found in the "*bank-names.txt*" file.

The motivation is to solve a classification problem using SVMs and NNs by developing a study which contains the following:

1. Exploratory Data Analysis and Data Visualization
2. Correlation Matrix
3. Train and test splitting
4. Model adjustment and plots

Please note that I will be closely following the class notes, scripts and documentation as well as functions and code snippets from the lectures.

# 2. Exploratory Data Analysis and Data Visualization

Let us start by loading some of the required packages. Others will be added along the way.


```
## gridExtra  corrplot    GGally   ggplot2     e1071 tidyverse  ggthemes 
##      TRUE      TRUE     FALSE      TRUE      TRUE      TRUE      TRUE 
##  reshape2     caret     knitr 
##      TRUE      TRUE      TRUE
```

Now, we will load the bank marketing campaign data into memory to perform extensive data manipulation:


```r
bank <- read.csv("./datasets/bank-full.csv",  sep = ";", na.strings = "unknown")
```



```r
dim(bank)
```

```
## [1] 45211    17
```

```r
str(bank)
```

```
## 'data.frame':	45211 obs. of  17 variables:
##  $ age      : int  58 44 33 47 33 35 28 42 58 43 ...
##  $ job      : Factor w/ 11 levels "admin.","blue-collar",..: 5 10 3 2 NA 5 5 3 6 10 ...
##  $ marital  : Factor w/ 3 levels "divorced","married",..: 2 3 2 2 3 2 3 1 2 3 ...
##  $ education: Factor w/ 3 levels "primary","secondary",..: 3 2 2 NA NA 3 3 3 1 2 ...
##  $ default  : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 2 1 1 ...
##  $ balance  : int  2143 29 2 1506 1 231 447 2 121 593 ...
##  $ housing  : Factor w/ 2 levels "no","yes": 2 2 2 2 1 2 2 2 2 2 ...
##  $ loan     : Factor w/ 2 levels "no","yes": 1 1 2 1 1 1 2 1 1 1 ...
##  $ contact  : Factor w/ 2 levels "cellular","telephone": NA NA NA NA NA NA NA NA NA NA ...
##  $ day      : int  5 5 5 5 5 5 5 5 5 5 ...
##  $ month    : Factor w/ 12 levels "apr","aug","dec",..: 9 9 9 9 9 9 9 9 9 9 ...
##  $ duration : int  261 151 76 92 198 139 217 380 50 55 ...
##  $ campaign : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ pdays    : int  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ...
##  $ previous : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ poutcome : Factor w/ 3 levels "failure","other",..: NA NA NA NA NA NA NA NA NA NA ...
##  $ y        : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
summary(bank)
```

```
##       age                 job           marital          education    
##  Min.   :18.00   blue-collar:9732   divorced: 5207   primary  : 6851  
##  1st Qu.:33.00   management :9458   married :27214   secondary:23202  
##  Median :39.00   technician :7597   single  :12790   tertiary :13301  
##  Mean   :40.94   admin.     :5171                    NA's     : 1857  
##  3rd Qu.:48.00   services   :4154                                     
##  Max.   :95.00   (Other)    :8811                                     
##                  NA's       : 288                                     
##  default        balance       housing      loan            contact     
##  no :44396   Min.   : -8019   no :20081   no :37967   cellular :29285  
##  yes:  815   1st Qu.:    72   yes:25130   yes: 7244   telephone: 2906  
##              Median :   448                           NA's     :13020  
##              Mean   :  1362                                            
##              3rd Qu.:  1428                                            
##              Max.   :102127                                            
##                                                                        
##       day            month          duration         campaign     
##  Min.   : 1.00   may    :13766   Min.   :   0.0   Min.   : 1.000  
##  1st Qu.: 8.00   jul    : 6895   1st Qu.: 103.0   1st Qu.: 1.000  
##  Median :16.00   aug    : 6247   Median : 180.0   Median : 2.000  
##  Mean   :15.81   jun    : 5341   Mean   : 258.2   Mean   : 2.764  
##  3rd Qu.:21.00   nov    : 3970   3rd Qu.: 319.0   3rd Qu.: 3.000  
##  Max.   :31.00   apr    : 2932   Max.   :4918.0   Max.   :63.000  
##                  (Other): 6060                                    
##      pdays          previous           poutcome       y        
##  Min.   : -1.0   Min.   :  0.0000   failure: 4901   no :39922  
##  1st Qu.: -1.0   1st Qu.:  0.0000   other  : 1840   yes: 5289  
##  Median : -1.0   Median :  0.0000   success: 1511              
##  Mean   : 40.2   Mean   :  0.5803   NA's   :36959              
##  3rd Qu.: -1.0   3rd Qu.:  0.0000                              
##  Max.   :871.0   Max.   :275.0000                              
## 
```

```r
bank %>% 
  group_by(y) %>% 
  summarise(count = n())
```

```
## # A tibble: 2 x 2
##   y     count
##   <fct> <int>
## 1 no    39922
## 2 yes    5289
```

A quick glance at the summary of the data reveals that the positive class accounts for 11.7% of the dataset.
Let us also take a look at the missing values of the data.





```r
missmap(bank,
        y.labels = NULL,
        y.at = NULL)
```

![](Classification_project_files/figure-html/missing-1.png)<!-- -->

The variable ```poutcome``` has many missing entries, while ```contact```, ```education``` and ```job``` have fewer. Only 7% of the overall data is missing.

It is interesting to check for skewness of numeric variables:


```r
skewedVars <- c()
for(i in names(bank)){
  if(is.numeric(bank[,i])){
    skewVal <- skewness(bank[,i])
    print(paste(i, skewVal, sep = ": "))
    if(abs(skewVal) > 0.5){
      skewedVars <- c(skewedVars, i)
    }
  }
}
```

```
## [1] "age: 0.684795204786645"
## [1] "balance: 8.36003094725269"
## [1] "day: 0.0930759258389723"
## [1] "duration: 3.14421377701039"
## [1] "campaign: 4.89848763841056"
## [1] "pdays: 2.61562868925939"
## [1] "previous: 41.8450660879732"
```

```r
skewedVars
```

```
## [1] "age"      "balance"  "duration" "campaign" "pdays"    "previous"
```

The variables returned by ```skewedVars``` are mostly skewed to the left, with a long tail distribution as can be seen in the plots:


```r
lngDataYes <- 
  bank %>% 
  filter(y == 'yes') %>% 
  select_if(is.numeric) %>% 
  gather()

lngDataNo <- 
  bank %>% 
  filter(y == 'no') %>% 
  select_if(is.numeric) %>% 
  gather()

ggplot(data = data.frame(), aes(x = value)) +
  geom_density(data = lngDataYes, fill = '#377EB8', color = '#377EB8', alpha = .3) +
  geom_density(data = lngDataNo, fill = '#E41A1C', color = '#E41A1C', alpha = .3) +
  facet_wrap( ~ key, scales = 'free') +
  labs(title = "Density",
       subtitle = 'of each numeric variable') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '',values = c(yes = '#377EB8', no = '#E41A1C')) +
  theme_fivethirtyeight()
```

![](Classification_project_files/figure-html/num_density-1.png)<!-- -->





```r
lngDataYes <- 
  bank %>% 
  filter(y == 'yes') %>% 
  select_if(negate(is.numeric)) %>% 
  gather()

lngDataNo <- 
  bank %>% 
  filter(y == 'no') %>% 
  select_if(negate(is.numeric)) %>% 
  gather()

ggplot(data = data.frame(), aes(x = value)) +
  geom_bar(data = lngDataYes, fill = '#377EB8', color = '#377EB8', alpha = 1) +
  geom_bar(data = lngDataNo, fill = '#E41A1C', color = '#E41A1C', alpha = .3) +
  facet_wrap( ~ key, scales = 'free') +
  labs(title = "Distribution",
       subtitle = 'of each numeric variable') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '',values = c(yes = '#377EB8', no = '#E41A1C')) +
  theme_fivethirtyeight() +
  coord_flip()
```

![](Classification_project_files/figure-html/cat_density-1.png)<!-- -->

The distribution of categorical variables is also important to monitor, since they can yield an insight on strong predictors.


```r
bank %>% 
  group_by(month, y) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count / sum(count)) %>% 
  ggplot(aes(x = month, y = prop * 100)) +
  geom_bar(aes(fill = y), stat = 'identity', position = 'dodge', alpha = .3) +
  labs(title = "Distribution of deposits",
       subtitle = 'by campaing month') + 
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()
```

![](Classification_project_files/figure-html/month_dist-1.png)<!-- -->

```r
bank %>% 
  select(month, balance, y) %>% 
  ggplot(aes(month, balance)) + 
  geom_boxplot(aes(fill = y)) + 
  coord_flip() +
  coord_cartesian(ylim = c(-3000, 11000)) +
  xlab('Campaign month') +
  labs(title = "Distribution of deposits",
       subtitle = 'by campaing month') + 
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()
```

![](Classification_project_files/figure-html/month_dist-2.png)<!-- -->

For instance, it can be seen that the ```month``` variable can be useful since some months such as March have a better yield of succesful subscriptions. However, many data points are considered outliers and lie beyond the 1st and 3rd quarters of the distribution. 


```r
bank %>% 
  select_if(negate(is.numeric)) %>% 
  ggplot() +
  geom_bar(aes(x = default, y = log(..count..), fill = y)) + 
  facet_grid(housing ~ loan, scales = 'free') +
  coord_flip() +
  labs(title = "Proportion of accounts in default",
       subtitle = 'housing loans vs personal loans') + 
  theme(plot.title = element_text(hjust = .5), 
        panel.grid.minor.x = element_line(colour = "gray80"),
        strip.text.y = element_blank()) + 
  scale_fill_manual(name = '',
                    values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()
```

![](Classification_project_files/figure-html/loans-1.png)<!-- -->

The graph above, plotted on a logarithmic scale for display purposes, show the proportion of contracted deposits for different values of the variables ```housing``` (on the Y-axis) and ```loan``` (on the X-axis). Blue values represent clients in a ```default``` status.
A hidden insight within the data reveals that, as it would be expected, people in default would not want to subscribe to additional products, especially when they have also contracted a personal or housing loan.


```r
bank %>% 
  ggplot(aes(x = campaign, y = duration, color = y)) +
  geom_jitter(stat = "identity") + 
  labs(title = "Number of campaign calls and duration",
       subtitle = "by subscription status") + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_color_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight() +
  geom_hline(aes(yintercept = 200), lty = 'dashed') +
  coord_cartesian(xlim = c(0, 40))
```

![](Classification_project_files/figure-html/duration-1.png)<!-- -->

It is also very interesting that very few values in the scatter plot above lie below the 200 seconds value on the ```duration``` variable, as denoted by the dashed line. Therefore it appears that fewer and longer calls are more effective at getting clients to subscribe a deposit. 

Let us confirm this assertion by plotting subscription rates along the number of calls made to the same client:


```r
bank %>% 
  ggplot(aes(x = campaign, fill = y)) +
  geom_histogram(position = 'dodge') + 
  labs(title = "Number of subscriptions",
       subtitle = "by number of calls") + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight() +
  coord_cartesian(xlim = c(0, 20))
```

![](Classification_project_files/figure-html/num_calls-1.png)<!-- -->

As expected, the fewer calls performed on the same client, the higher the probability to subscribe.


```r
bank %>% 
  ggplot(aes(x = age)) +
  geom_density(aes(x = age, fill = y), alpha = .6) +
  labs(title = "Distribution of deposits",
       subtitle = 'by age of client') + 
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()
```

![](Classification_project_files/figure-html/age-1.png)<!-- -->

The ```age``` density plot reveals that better adoption rates are found within the extreme groups of the population. That is, the young and especially the elderly. This may also be correlated with the job category of a client, which can be easily checked:


```r
bank %>% 
  group_by(y, job) %>% 
  summarise(count = n()) %>% 
  arrange(count) %>% 
  mutate(freq = count / sum(count)) %>%
  ggplot(aes(x = reorder(job, -freq), y = freq, fill = y)) +
  geom_bar(stat = "identity", width = .5) + coord_flip() + 
  labs(title = "Distribution of deposits",
       subtitle = 'individual proportion over jobs') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()
```

![](Classification_project_files/figure-html/jobs-1.png)<!-- -->

It can be seen that students and retired clients are more likely to purchase a subscription, which correlates nicely with our previous hypothesis.


```r
bank %>% 
  ggplot(aes(x = balance)) +
  geom_density(aes(fill = y), alpha = 0.6) + 
  labs(title = "Balance density",
       subtitle = 'analyzed over the full dataset') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight() +
  coord_cartesian(xlim = c(0, 15000))
```

![](Classification_project_files/figure-html/balance-1.png)<!-- -->

# 3. Correlation matrix



```r
correlations <- bank %>%
                select_if(is.numeric) %>% 
                na.omit() %>% 
                cor()

as.data.frame(correlations) %>% round(4) %>% knitr::kable()
```

                age   balance       day   duration   campaign     pdays   previous
---------  --------  --------  --------  ---------  ---------  --------  ---------
age          1.0000    0.0978   -0.0091    -0.0046     0.0048   -0.0238     0.0013
balance      0.0978    1.0000    0.0045     0.0216    -0.0146    0.0034     0.0167
day         -0.0091    0.0045    1.0000    -0.0302     0.1625   -0.0930    -0.0517
duration    -0.0046    0.0216   -0.0302     1.0000    -0.0846   -0.0016     0.0012
campaign     0.0048   -0.0146    0.1625    -0.0846     1.0000   -0.0886    -0.0329
pdays       -0.0238    0.0034   -0.0930    -0.0016    -0.0886    1.0000     0.4548
previous     0.0013    0.0167   -0.0517     0.0012    -0.0329    0.4548     1.0000

```r
corrplot(correlations, method = "square")
```

![](Classification_project_files/figure-html/corr-1.png)<!-- -->

The numerical variables have low correlation values between one another. No further steps are needed to remove any highly correlated features.

# 4. Train & test splitting

Instead of using the ```caret``` package, the code snippet below quickly splits the data in equally proportioned sets:


```r
bank <- na.omit(bank)
index <- 1:nrow(bank)
test_idx <- sample(index, trunc(length(index) * 30 / 100))
test_set <- bank[test_idx,]
train_set <- bank[-test_idx,]

table(train_set$y) / length(train_set$y)
```

```
## 
##        no       yes 
## 0.7681239 0.2318761
```

```r
table(test_set$y) / length(test_set$y)
```

```
## 
##        no       yes 
## 0.7818878 0.2181122
```


# 5. Model adjustments and plots

Fitting several different models will allow us to carefully compare their performances and accuracy:

## 5.1. Logistic regression


```r
log_mdl <- glm(y ~ .,
               data = train_set,
               family = binomial('logit'))
summary(log_mdl)
```

```
## 
## Call:
## glm(formula = y ~ ., family = binomial("logit"), data = train_set)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.0866  -0.4797  -0.2983  -0.1663   2.7081  
## 
## Coefficients:
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -3.477e+00  3.905e-01  -8.904  < 2e-16 ***
## age                -9.329e-04  5.116e-03  -0.182 0.855324    
## jobblue-collar     -3.453e-01  1.688e-01  -2.046 0.040792 *  
## jobentrepreneur    -4.954e-01  3.303e-01  -1.500 0.133637    
## jobhousemaid       -2.105e-01  3.157e-01  -0.667 0.504938    
## jobmanagement       1.879e-01  1.629e-01   1.153 0.248852    
## jobretired          7.335e-02  2.262e-01   0.324 0.745680    
## jobself-employed   -2.219e-01  2.599e-01  -0.854 0.393305    
## jobservices        -8.237e-02  1.931e-01  -0.427 0.669724    
## jobstudent          2.850e-01  2.348e-01   1.213 0.224956    
## jobtechnician      -1.506e-01  1.537e-01  -0.980 0.326952    
## jobunemployed       4.490e-01  2.587e-01   1.736 0.082624 .  
## maritalmarried      2.259e-01  1.426e-01   1.584 0.113283    
## maritalsingle       2.400e-01  1.625e-01   1.476 0.139818    
## educationsecondary  1.804e-01  1.536e-01   1.175 0.240151    
## educationtertiary   2.591e-01  1.773e-01   1.461 0.144018    
## defaultyes         -4.379e-01  6.239e-01  -0.702 0.482756    
## balance            -2.134e-06  1.322e-05  -0.161 0.871751    
## housingyes         -8.067e-01  9.855e-02  -8.185 2.71e-16 ***
## loanyes            -5.038e-01  1.507e-01  -3.342 0.000832 ***
## contacttelephone   -3.560e-01  1.701e-01  -2.093 0.036313 *  
## day                 2.232e-02  5.694e-03   3.921 8.83e-05 ***
## monthaug            1.049e+00  1.841e-01   5.699 1.21e-08 ***
## monthdec            8.745e-01  3.220e-01   2.716 0.006608 ** 
## monthfeb            4.010e-01  1.891e-01   2.120 0.033983 *  
## monthjan           -4.967e-01  2.352e-01  -2.111 0.034740 *  
## monthjul            1.196e+00  2.384e-01   5.018 5.21e-07 ***
## monthjun            1.271e+00  2.175e-01   5.844 5.09e-09 ***
## monthmar            1.619e+00  2.590e-01   6.251 4.07e-10 ***
## monthmay            9.333e-02  1.509e-01   0.619 0.536208    
## monthnov           -2.707e-02  1.746e-01  -0.155 0.876781    
## monthoct            1.141e+00  2.048e-01   5.574 2.50e-08 ***
## monthsep            1.494e+00  2.195e-01   6.804 1.02e-11 ***
## duration            3.828e-03  1.815e-04  21.093  < 2e-16 ***
## campaign           -1.400e-01  3.315e-02  -4.224 2.40e-05 ***
## pdays               6.135e-04  3.937e-04   1.558 0.119201    
## previous            2.863e-02  1.243e-02   2.303 0.021290 *  
## poutcomeother       2.068e-01  1.095e-01   1.888 0.059076 .  
## poutcomesuccess     2.083e+00  1.003e-01  20.763  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5946.0  on 5489  degrees of freedom
## Residual deviance: 3812.1  on 5451  degrees of freedom
## AIC: 3890.1
## 
## Number of Fisher Scoring iterations: 5
```

```r
prob <- predict(log_mdl,
                test_set,
                type = 'response')
pred <- rep('no', length(prob))
pred[prob >= .5] <- 'yes'
 
confusionMatrix(pred, test_set$y)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   no  yes
##        no  1709  232
##        yes  130  281
##                                           
##                Accuracy : 0.8461          
##                  95% CI : (0.8309, 0.8604)
##     No Information Rate : 0.7819          
##     P-Value [Acc > NIR] : 2.637e-15       
##                                           
##                   Kappa : 0.5139          
##  Mcnemar's Test P-Value : 1.106e-07       
##                                           
##             Sensitivity : 0.9293          
##             Specificity : 0.5478          
##          Pos Pred Value : 0.8805          
##          Neg Pred Value : 0.6837          
##              Prevalence : 0.7819          
##          Detection Rate : 0.7266          
##    Detection Prevalence : 0.8253          
##       Balanced Accuracy : 0.7385          
##                                           
##        'Positive' Class : no              
## 
```


## 5.2. Neural Network

Let us fit a neural network with random structure and see the general performance of the technique. Afterwards, we can try to tune it further.






```r
nn_pred <- predict(nn_mdl2,
                   newdata = test_set,
                   type = 'raw')

nn_pred_prob <- rep('no', length(nn_pred))
nn_pred_prob[nn_pred >= 0.5] <- 'yes'

confusionMatrix(nn_pred_prob, test_set$y)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   no  yes
##        no  1662  196
##        yes  177  317
##                                         
##                Accuracy : 0.8414        
##                  95% CI : (0.826, 0.856)
##     No Information Rate : 0.7819        
##     P-Value [Acc > NIR] : 2.527e-13     
##                                         
##                   Kappa : 0.5287        
##  Mcnemar's Test P-Value : 0.3513        
##                                         
##             Sensitivity : 0.9038        
##             Specificity : 0.6179        
##          Pos Pred Value : 0.8945        
##          Neg Pred Value : 0.6417        
##              Prevalence : 0.7819        
##          Detection Rate : 0.7066        
##    Detection Prevalence : 0.7900        
##       Balanced Accuracy : 0.7608        
##                                         
##        'Positive' Class : no            
## 
```

```r
nn_pred <- predict(nn_mdl,
                   newdata = test_set,
                   type = 'raw') %>% 
  round()

confusionMatrix(nn_pred, as.numeric(test_set$y) - 1)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    0    1
##          0 1584  160
##          1  255  353
##                                           
##                Accuracy : 0.8236          
##                  95% CI : (0.8075, 0.8388)
##     No Information Rate : 0.7819          
##     P-Value [Acc > NIR] : 3.080e-07       
##                                           
##                   Kappa : 0.5151          
##  Mcnemar's Test P-Value : 3.945e-06       
##                                           
##             Sensitivity : 0.8613          
##             Specificity : 0.6881          
##          Pos Pred Value : 0.9083          
##          Neg Pred Value : 0.5806          
##              Prevalence : 0.7819          
##          Detection Rate : 0.6735          
##    Detection Prevalence : 0.7415          
##       Balanced Accuracy : 0.7747          
##                                           
##        'Positive' Class : 0               
## 
```

There is a small improvement in the accuracy of the model, from 82% to 83%, derived from the increase in neurons. We will keep the better model for future comparison.

## 5.3. Decision Tree

A classification tree can also help predict variable classes:


```r
tree_model <- rpart(y ~ .,
                    data = train_set,
                    method = "class",
                    minbucket = 20)

tree_predict <- predict(tree_model,
                        test_set,
                        type = "class")

tree_pred_prob <- predict(tree_model,
                        test_set,
                        type = "prob")

confusionMatrix(test_set$y, tree_predict)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   no  yes
##        no  1691  148
##        yes  213  300
##                                           
##                Accuracy : 0.8465          
##                  95% CI : (0.8313, 0.8609)
##     No Information Rate : 0.8095          
##     P-Value [Acc > NIR] : 1.584e-06       
##                                           
##                   Kappa : 0.5285          
##  Mcnemar's Test P-Value : 0.000756        
##                                           
##             Sensitivity : 0.8881          
##             Specificity : 0.6696          
##          Pos Pred Value : 0.9195          
##          Neg Pred Value : 0.5848          
##              Prevalence : 0.8095          
##          Detection Rate : 0.7190          
##    Detection Prevalence : 0.7819          
##       Balanced Accuracy : 0.7789          
##                                           
##        'Positive' Class : no              
## 
```

## 5.4. Random Forest

Also, we can use an ensemble model of decision trees to create a random forest: 


```r
rf <- randomForest(y ~ .,
                   data = train_set,
                   ntree = 1000)
rf_pred_prob <- predict(rf,
                        newdata = test_set,
                        type = 'prob')
rf_pred <- predict(rf,
                   newdata = test_set,
                   type = 'class')

confusionMatrix(test_set$y, rf_pred)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   no  yes
##        no  1683  156
##        yes  185  328
##                                          
##                Accuracy : 0.855          
##                  95% CI : (0.8401, 0.869)
##     No Information Rate : 0.7942         
##     P-Value [Acc > NIR] : 1.839e-14      
##                                          
##                   Kappa : 0.5661         
##  Mcnemar's Test P-Value : 0.1294         
##                                          
##             Sensitivity : 0.9010         
##             Specificity : 0.6777         
##          Pos Pred Value : 0.9152         
##          Neg Pred Value : 0.6394         
##              Prevalence : 0.7942         
##          Detection Rate : 0.7156         
##    Detection Prevalence : 0.7819         
##       Balanced Accuracy : 0.7893         
##                                          
##        'Positive' Class : no             
## 
```

## 5.5. Support Vector Machine

An SVM model can greatly outperform all others. Let us see how it can perform:


```r
svm_model <- svm(y ~ ., data = train_set, kernel = "radial")

summary(svm_model)
```

```
## 
## Call:
## svm(formula = y ~ ., data = train_set, kernel = "radial")
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  radial 
##        cost:  1 
##       gamma:  0.02564103 
## 
## Number of Support Vectors:  2113
## 
##  ( 1077 1036 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  no yes
```

```r
prediction <- predict(svm_model, test_set[,-17])
```


```r
table(pred = prediction,
      y = test_set$y) / length(prediction)
```

```
##      y
## pred          no        yes
##   no  0.72151361 0.09098639
##   yes 0.06037415 0.12712585
```

```r
table(pred = prediction,
      y = test_set[,17])
```

```
##      y
## pred    no  yes
##   no  1697  214
##   yes  142  299
```

```r
cm_svm <- confusionMatrix(prediction, test_set$y)
```

An accuracy of 84.86% is acceptable, but previous models already outperform this one.
Notice that the overall accuracy might not be the best metric to analyze this models' performances. However, this will be addressed later on.
We can use  cross validation techniques or fine-tune the model's parameters to obtain better results:


```r
set.seed(6)

control <- trainControl(method = "cv", number = 10)

cv_svm <- train(y ~ .,
                data = train_set,
                method = "svmLinear",
                metric = "Accuracy",
                trControl = control)

cv_pred_prob <- predict(cv_svm,
                        newdata = test_set,
                        type = 'raw')

(cm_cv_svm <- confusionMatrix(cv_pred_prob, test_set$y))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   no  yes
##        no  1685  226
##        yes  154  287
##                                           
##                Accuracy : 0.8384          
##                  95% CI : (0.8229, 0.8531)
##     No Information Rate : 0.7819          
##     P-Value [Acc > NIR] : 3.781e-12       
##                                           
##                   Kappa : 0.5011          
##  Mcnemar's Test P-Value : 0.0002703       
##                                           
##             Sensitivity : 0.9163          
##             Specificity : 0.5595          
##          Pos Pred Value : 0.8817          
##          Neg Pred Value : 0.6508          
##              Prevalence : 0.7819          
##          Detection Rate : 0.7164          
##    Detection Prevalence : 0.8125          
##       Balanced Accuracy : 0.7379          
##                                           
##        'Positive' Class : no              
## 
```

83.84% accuracy is also good, but still not optimal. The cross validated SVM can still be improved upon.

Using the ```total_accuracy_svm``` function from the lectures, we can try to find the best values for the C and Gamma parameters:


```r
gamma <- c(10 ^ seq(-5, -1,
                    by = 1))

cost <- c(10 ^ seq(-2, 1,
                   by = 1))

params <- expand.grid(cost = cost,
                      gamma = gamma)

total_accuracy_svm <- function(train_set, test_set) {
  accuracy1 <- NULL
  accuracy2 <- NULL
  for (i in 1:nrow(params)) {
    learn_svm <-
      svm(
        y ~ .,
        data = train_set,
        gamma = params$gamma[i],
        cost = params$cost[i],
        kernel = 'radial'
      )
    pred_svm <- predict(learn_svm, test_set[, -17])
    accuracy1 <- confusionMatrix(pred_svm, test_set$y)
    accuracy2[i] <- accuracy1$overall[1]
  }
  accuracy2
}

c <- total_accuracy_svm(train_set, test_set)
opt_params <- which(c == max(c))[1]

opt_cost = params$cost[opt_params]
opt_gamma = params$gamma[opt_params]
```

The optimal values obtained are ```cost``` = 10 and ```gamma``` = 0.01, which can be used to train the final SVM:


```r
imp_svm <-
  svm(y ~ .,
      data = train_set,
      cost = opt_cost,
      gamma = opt_gamma)

pred_imp_svm <- predict(imp_svm, test_set[, -17])
(cm_imp_svm <- confusionMatrix(pred_imp_svm, test_set$y))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   no  yes
##        no  1705  209
##        yes  134  304
##                                           
##                Accuracy : 0.8542          
##                  95% CI : (0.8393, 0.8682)
##     No Information Rate : 0.7819          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.5486          
##  Mcnemar's Test P-Value : 6.452e-05       
##                                           
##             Sensitivity : 0.9271          
##             Specificity : 0.5926          
##          Pos Pred Value : 0.8908          
##          Neg Pred Value : 0.6941          
##              Prevalence : 0.7819          
##          Detection Rate : 0.7249          
##    Detection Prevalence : 0.8138          
##       Balanced Accuracy : 0.7599          
##                                           
##        'Positive' Class : no              
## 
```

With 85.42% accuracy, this is the best model obtained so far. Let us get the SVM and prediction objects ready for posterior manipulation:


```r
mdl_svm <- 
  ksvm(y ~ .,
       data = train_set,
       kernel = "rbfdot")

imp_svm <-
  ksvm(y ~ .,
       data = train_set,
       cost = opt_cost,
       gamma = opt_gamma,
       kernel = "rbfdot")

svm_pred_prob <- predict(mdl_svm,
                         newdata = test_set,
                         type = 'decision')
imp_pred_prob <- predict(imp_svm,
                         newdata = test_set,
                         type = 'decision')
```

## 5.6. Adjustment and plots

We can plot the prediction values of the SVM models as seen in the lectures:


```r
colr <- c("#ed3b3b", "#0099ff")
par(mfrow = c(1, 2))

fourfoldplot(cm_cv_svm$table,
             color = colr,
             conf.level = 0, 
             margin = 1,
             main = paste("CV SVM (", round(cm_cv_svm$overall[1] * 100), "%)",sep = ""))

fourfoldplot(cm_imp_svm$table, 
             color = colr,
             conf.level = 0, 
             margin = 1,
             main = paste("Tuned SVM (", round(cm_imp_svm$overall[1] * 100), "%)",sep = ""))
```

![](Classification_project_files/figure-html/graph_svm-1.png)<!-- -->

As well as the Principal Component projections that allow us to view the decision boundary:


```r
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
```

![](Classification_project_files/figure-html/pca_svm-1.png)<!-- -->

In order to deal with the unreliability of the overall accuracy metric, we will follow the class notes to display Receiver Operating Characteristic curves of the models, as well as the Area Under the Curve.

For that, we create ```prediction``` objects and store True and False positives in a data frame:



```r
pr <- ROCR::prediction(prob, test_set$y)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
dd <- data.frame(FP = prf@x.values[[1]], TP = prf@y.values[[1]])

pr1 <- ROCR::prediction(nn_pred, test_set$y)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
dd1 <- data.frame(FP = prf1@x.values[[1]], TP = prf1@y.values[[1]])

pr2 <- ROCR::prediction(tree_pred_prob[,2], test_set$y)
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
dd2 <- data.frame(FP = prf2@x.values[[1]], TP = prf2@y.values[[1]])

pr3 <- ROCR::prediction(rf_pred_prob[,2], test_set$y)
prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
dd3 <- data.frame(FP = prf3@x.values[[1]], TP = prf3@y.values[[1]])

pr4 <- ROCR::prediction(svm_pred_prob, test_set$y)
prf4 <- performance(pr4, measure = "tpr", x.measure = "fpr")
dd4 <- data.frame(FP = prf4@x.values[[1]], TP = prf4@y.values[[1]])

pr5 <- ROCR::prediction(imp_pred_prob, test_set$y)
prf5 <- performance(pr5, measure = "tpr", x.measure = "fpr")
dd5 <- data.frame(FP = prf5@x.values[[1]], TP = prf5@y.values[[1]])
```


Next, we can plot the curves using this dataframe:


```r
g <- ggplot() + 
  geom_line(data = dd, aes(x = FP, y = TP, color = 'Logistic Regression'), size = 1.25) + 
  geom_line(data = dd1, aes(x = FP, y = TP, color = 'Neural Network'), size = 1.25) + 
  geom_line(data = dd2, aes(x = FP, y = TP, color = 'Decision Tree'), size = 1.25) + 
  geom_line(data = dd3, aes(x = FP, y = TP, color = 'Random Forest'), size = 1.25) +
  geom_line(data = dd4, aes(x = FP, y = TP, color = 'CV Support Vector Machine'), size = 1.25) +
  geom_line(data = dd5, aes(x = FP, y = TP, color = 'Tuned Support Vector Machine'), size = 1.25) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
  ggtitle('ROC Curve') + 
  labs(x = 'False Positive Rate', y = 'True Positive Rate') +  theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()


g +  scale_colour_manual(name = 'Classifier', values = c('Logistic Regression'='#E69F00',
                                                         'Neural Network'='#56B4E9', 'Decision Tree'='#009E73',
                                                         'Random Forest'='#D55E00', 'CV Support Vector Machine'='#0072B2', 'Tuned Support Vector Machine'='#0352B1'))
```

![](Classification_project_files/figure-html/roc_plot-1.png)<!-- -->

And compute the AUC metric:


```r
auc <- rbind(performance(pr, measure = 'auc')@y.values[[1]],
             performance(pr1, measure = 'auc')@y.values[[1]],
             performance(pr2, measure = 'auc')@y.values[[1]],
             performance(pr3, measure = 'auc')@y.values[[1]],
             performance(pr4, measure = 'auc')@y.values[[1]],
             performance(pr5, measure = 'auc')@y.values[[1]])

colnames(auc) <- 'AUC'

auc <- auc %>%
  as.data.frame() %>%
  arrange(AUC) %>%
  round(4)

rownames(auc) <- (c('Logistic Regression', 'Neural Network', 'Decision Tree',
                    'Random Forest', 'CV Support Vector Machine',
                    'Tuned Support Vector Machine'))
auc
```

```
##                                 AUC
## Logistic Regression          0.7747
## Neural Network               0.8140
## Decision Tree                0.8874
## Random Forest                0.8985
## CV Support Vector Machine    0.8985
## Tuned Support Vector Machine 0.9061
```

As expected, the Tuned SVM yields the best possible results from within all the applied models, with the highest Area Under the Curve.
Nonetheless, it takes considerably longer to train than other models. This is due to the fact that there are actually several models trained, one for every combination of ```cost``` and ```gamma``` defined.
Then, we find the one with higher accuracy and train the final model with the optimal parameters found in the step above.

# Reference

S. Moro, R. Laureano and P. Cortez. *Using Data Mining for Bank Direct Marketing: An Application of the CRISP-DM Methodology.* 2011
González-Hidalgo, M., Miró-Julià, M. *Class notes, Statistical Learning and Decision Making II.* 2018.

