-   [1. Introduction](#introduction)
-   [2. Exploratory Data Analysis and Data
    Visualization](#exploratory-data-analysis-and-data-visualization)
-   [3. Correlation matrix](#correlation-matrix)
-   [4. Train & test splitting](#train-test-splitting)
-   [5. Model adjustments and plots](#model-adjustments-and-plots)
    -   [5.1. Logistic regression](#logistic-regression)
    -   [5.2. Neural Network](#neural-network)
    -   [5.3. Decision Tree](#decision-tree)
    -   [5.4. Random Forest](#random-forest)
    -   [5.5. Support Vector Machine](#support-vector-machine)
    -   [5.6. Adjustment and plots](#adjustment-and-plots)
-   [Reference](#reference)

1. Introduction
===============

Consider the "*bank-full.csv*" dataset. The data is related with direct
marketing campaigns of a Portuguese banking institution. The marketing
campaigns were based on phone calls. Often, more than one contact to the
same client was required, in order to access if the product (bank term
deposit) would be (or not) subscribed.

The dataset description can be found in the "*bank-names.txt*" file.

The motivation is to solve a classification problem using SVMs and NNs
by developing a study which contains the following:

1.  Exploratory Data Analysis and Data Visualization
2.  Correlation Matrix
3.  Train and test splitting
4.  Model adjustment and plots

Please note that I will be closely following the class notes, scripts
and documentation as well as functions and code snippets from the
lectures.

2. Exploratory Data Analysis and Data Visualization
===================================================

Let us start by loading some of the required packages. Others will be
added along the way.

    ## gridExtra  corrplot    GGally   ggplot2     e1071 tidyverse  ggthemes 
    ##      TRUE      TRUE     FALSE      TRUE      TRUE      TRUE      TRUE 
    ##  reshape2     caret     knitr 
    ##      TRUE      TRUE      TRUE

Now, we will load the bank marketing campaign data into memory to
perform extensive data manipulation:

    bank <- read.csv("./datasets/bank-full.csv",  sep = ";", na.strings = "unknown")

    dim(bank)

    ## [1] 45211    17

    str(bank)

    ## 'data.frame':    45211 obs. of  17 variables:
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

    summary(bank)

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

    bank %>% 
      group_by(y) %>% 
      summarise(count = n())

    ## # A tibble: 2 x 2
    ##   y     count
    ##   <fct> <int>
    ## 1 no    39922
    ## 2 yes    5289

A quick glance at the summary of the data reveals that the positive
class accounts for 11.7% of the dataset. Let us also take a look at the
missing values of the data.

    missmap(bank,
            y.labels = NULL,
            y.at = NULL)

![](Classification_project_files/figure-markdown_strict/missing-1.png)

The variable `poutcome` has many missing entries, while `contact`,
`education` and `job` have fewer. Only 7% of the overall data is
missing.

It is interesting to check for skewness of numeric variables:

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

    ## [1] "age: 0.684795204786645"
    ## [1] "balance: 8.36003094725269"
    ## [1] "day: 0.0930759258389723"
    ## [1] "duration: 3.14421377701039"
    ## [1] "campaign: 4.89848763841056"
    ## [1] "pdays: 2.61562868925939"
    ## [1] "previous: 41.8450660879732"

    skewedVars

    ## [1] "age"      "balance"  "duration" "campaign" "pdays"    "previous"

The variables returned by `skewedVars` are mostly skewed to the left,
with a long tail distribution as can be seen in the plots:

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

![](Classification_project_files/figure-markdown_strict/num_density-1.png)

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

![](Classification_project_files/figure-markdown_strict/cat_density-1.png)

The distribution of categorical variables is also important to monitor,
since they can yield an insight on strong predictors.

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

![](Classification_project_files/figure-markdown_strict/month_dist-1.png)

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

![](Classification_project_files/figure-markdown_strict/month_dist-2.png)

For instance, it can be seen that the `month` variable can be useful
since some months such as March have a better yield of succesful
subscriptions. However, many data points are considered outliers and lie
beyond the 1st and 3rd quarters of the distribution.

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

![](Classification_project_files/figure-markdown_strict/loans-1.png)

The graph above, plotted on a logarithmic scale for display purposes,
show the proportion of contracted deposits for different values of the
variables `housing` (on the Y-axis) and `loan` (on the X-axis). Blue
values represent clients in a `default` status. A hidden insight within
the data reveals that, as it would be expected, people in default would
not want to subscribe to additional products, especially when they have
also contracted a personal or housing loan.

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

![](Classification_project_files/figure-markdown_strict/duration-1.png)

It is also very interesting that very few values in the scatter plot
above lie below the 200 seconds value on the `duration` variable, as
denoted by the dashed line. Therefore it appears that fewer and longer
calls are more effective at getting clients to subscribe a deposit.

Let us confirm this assertion by plotting subscription rates along the
number of calls made to the same client:

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

![](Classification_project_files/figure-markdown_strict/num_calls-1.png)

As expected, the fewer calls performed on the same client, the higher
the probability to subscribe.

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

![](Classification_project_files/figure-markdown_strict/age-1.png)

The `age` density plot reveals that better adoption rates are found
within the extreme groups of the population. That is, the young and
especially the elderly. This may also be correlated with the job
category of a client, which can be easily checked:

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

![](Classification_project_files/figure-markdown_strict/jobs-1.png)

It can be seen that students and retired clients are more likely to
purchase a subscription, which correlates nicely with our previous
hypothesis.

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

![](Classification_project_files/figure-markdown_strict/balance-1.png)

3. Correlation matrix
=====================

    correlations <- bank %>%
                    select_if(is.numeric) %>% 
                    na.omit() %>% 
                    cor()

    as.data.frame(correlations) %>% round(4) %>% knitr::kable()

<table>
<thead>
<tr class="header">
<th></th>
<th align="right">age</th>
<th align="right">balance</th>
<th align="right">day</th>
<th align="right">duration</th>
<th align="right">campaign</th>
<th align="right">pdays</th>
<th align="right">previous</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>age</td>
<td align="right">1.0000</td>
<td align="right">0.0978</td>
<td align="right">-0.0091</td>
<td align="right">-0.0046</td>
<td align="right">0.0048</td>
<td align="right">-0.0238</td>
<td align="right">0.0013</td>
</tr>
<tr class="even">
<td>balance</td>
<td align="right">0.0978</td>
<td align="right">1.0000</td>
<td align="right">0.0045</td>
<td align="right">0.0216</td>
<td align="right">-0.0146</td>
<td align="right">0.0034</td>
<td align="right">0.0167</td>
</tr>
<tr class="odd">
<td>day</td>
<td align="right">-0.0091</td>
<td align="right">0.0045</td>
<td align="right">1.0000</td>
<td align="right">-0.0302</td>
<td align="right">0.1625</td>
<td align="right">-0.0930</td>
<td align="right">-0.0517</td>
</tr>
<tr class="even">
<td>duration</td>
<td align="right">-0.0046</td>
<td align="right">0.0216</td>
<td align="right">-0.0302</td>
<td align="right">1.0000</td>
<td align="right">-0.0846</td>
<td align="right">-0.0016</td>
<td align="right">0.0012</td>
</tr>
<tr class="odd">
<td>campaign</td>
<td align="right">0.0048</td>
<td align="right">-0.0146</td>
<td align="right">0.1625</td>
<td align="right">-0.0846</td>
<td align="right">1.0000</td>
<td align="right">-0.0886</td>
<td align="right">-0.0329</td>
</tr>
<tr class="even">
<td>pdays</td>
<td align="right">-0.0238</td>
<td align="right">0.0034</td>
<td align="right">-0.0930</td>
<td align="right">-0.0016</td>
<td align="right">-0.0886</td>
<td align="right">1.0000</td>
<td align="right">0.4548</td>
</tr>
<tr class="odd">
<td>previous</td>
<td align="right">0.0013</td>
<td align="right">0.0167</td>
<td align="right">-0.0517</td>
<td align="right">0.0012</td>
<td align="right">-0.0329</td>
<td align="right">0.4548</td>
<td align="right">1.0000</td>
</tr>
</tbody>
</table>

    corrplot(correlations, method = "square")

![](Classification_project_files/figure-markdown_strict/corr-1.png)

The numerical variables have low correlation values between one another.
No further steps are needed to remove any highly correlated features.

4. Train & test splitting
=========================

Instead of using the `caret` package, the code snippet below quickly
splits the data in equally proportioned sets:

    bank <- na.omit(bank)
    index <- 1:nrow(bank)
    test_idx <- sample(index, trunc(length(index) * 30 / 100))
    test_set <- bank[test_idx,]
    train_set <- bank[-test_idx,]

    table(train_set$y) / length(train_set$y)

    ## 
    ##       no      yes 
    ## 0.770674 0.229326

    table(test_set$y) / length(test_set$y)

    ## 
    ##        no       yes 
    ## 0.7759354 0.2240646

5. Model adjustments and plots
==============================

Fitting several different models will allow us to carefully compare
their performances and accuracy:

5.1. Logistic regression
------------------------

    log_mdl <- glm(y ~ .,
                   data = train_set,
                   family = binomial('logit'))
    summary(log_mdl)

    ## 
    ## Call:
    ## glm(formula = y ~ ., family = binomial("logit"), data = train_set)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.5022  -0.4736  -0.2966  -0.1645   2.7141  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -3.594e+00  3.989e-01  -9.010  < 2e-16 ***
    ## age                 1.459e-03  5.218e-03   0.280 0.779726    
    ## jobblue-collar     -3.256e-01  1.702e-01  -1.913 0.055704 .  
    ## jobentrepreneur    -8.769e-01  3.298e-01  -2.659 0.007831 ** 
    ## jobhousemaid       -3.851e-01  3.195e-01  -1.205 0.228063    
    ## jobmanagement       1.966e-02  1.615e-01   0.122 0.903144    
    ## jobretired         -4.977e-02  2.295e-01  -0.217 0.828316    
    ## jobself-employed   -3.130e-02  2.516e-01  -0.124 0.901004    
    ## jobservices        -4.437e-02  1.921e-01  -0.231 0.817392    
    ## jobstudent          3.008e-01  2.359e-01   1.275 0.202133    
    ## jobtechnician      -1.188e-01  1.538e-01  -0.773 0.439564    
    ## jobunemployed       1.782e-01  2.596e-01   0.686 0.492483    
    ## maritalmarried      1.790e-01  1.435e-01   1.247 0.212416    
    ## maritalsingle       1.358e-01  1.639e-01   0.829 0.407248    
    ## educationsecondary  1.376e-01  1.540e-01   0.894 0.371568    
    ## educationtertiary   3.320e-01  1.777e-01   1.869 0.061688 .  
    ## defaultyes         -6.949e-01  6.914e-01  -1.005 0.314873    
    ## balance             1.243e-05  1.227e-05   1.013 0.310918    
    ## housingyes         -7.742e-01  9.912e-02  -7.811 5.67e-15 ***
    ## loanyes            -5.543e-01  1.551e-01  -3.575 0.000351 ***
    ## contacttelephone   -1.638e-01  1.648e-01  -0.994 0.320234    
    ## day                 2.237e-02  5.767e-03   3.878 0.000105 ***
    ## monthaug            1.146e+00  1.876e-01   6.109 1.00e-09 ***
    ## monthdec            1.064e+00  3.151e-01   3.377 0.000733 ***
    ## monthfeb            5.234e-01  1.894e-01   2.763 0.005731 ** 
    ## monthjan           -7.547e-01  2.516e-01  -2.999 0.002707 ** 
    ## monthjul            1.384e+00  2.384e-01   5.806 6.38e-09 ***
    ## monthjun            1.237e+00  2.139e-01   5.781 7.42e-09 ***
    ## monthmar            1.692e+00  2.604e-01   6.496 8.27e-11 ***
    ## monthmay           -5.637e-02  1.538e-01  -0.367 0.713915    
    ## monthnov            8.083e-03  1.740e-01   0.046 0.962949    
    ## monthoct            9.743e-01  2.129e-01   4.577 4.72e-06 ***
    ## monthsep            1.372e+00  2.193e-01   6.256 3.95e-10 ***
    ## duration            3.945e-03  1.889e-04  20.884  < 2e-16 ***
    ## campaign           -1.320e-01  3.247e-02  -4.065 4.81e-05 ***
    ## pdays               9.179e-04  4.013e-04   2.287 0.022183 *  
    ## previous            2.979e-02  1.230e-02   2.422 0.015420 *  
    ## poutcomeother       2.130e-01  1.098e-01   1.940 0.052399 .  
    ## poutcomesuccess     2.087e+00  1.017e-01  20.528  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 5912.3  on 5489  degrees of freedom
    ## Residual deviance: 3769.8  on 5451  degrees of freedom
    ## AIC: 3847.8
    ## 
    ## Number of Fisher Scoring iterations: 5

    prob <- predict(log_mdl,
                    test_set,
                    type = 'response')
    pred <- rep('no', length(prob))
    pred[prob >= .5] <- 'yes'
     
    confusionMatrix(pred, test_set$y)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   no  yes
    ##        no  1674  229
    ##        yes  151  298
    ##                                           
    ##                Accuracy : 0.8384          
    ##                  95% CI : (0.8229, 0.8531)
    ##     No Information Rate : 0.7759          
    ##     P-Value [Acc > NIR] : 2.791e-14       
    ##                                           
    ##                   Kappa : 0.5095          
    ##  Mcnemar's Test P-Value : 7.815e-05       
    ##                                           
    ##             Sensitivity : 0.9173          
    ##             Specificity : 0.5655          
    ##          Pos Pred Value : 0.8797          
    ##          Neg Pred Value : 0.6637          
    ##              Prevalence : 0.7759          
    ##          Detection Rate : 0.7117          
    ##    Detection Prevalence : 0.8091          
    ##       Balanced Accuracy : 0.7414          
    ##                                           
    ##        'Positive' Class : no              
    ## 

5.2. Neural Network
-------------------

Let us fit a neural network with random structure and see the general
performance of the technique. Afterwards, we can try to tune it further.

    nn_pred <- predict(nn_mdl2,
                       newdata = test_set,
                       type = 'raw')

    nn_pred_prob <- rep('no', length(nn_pred))
    nn_pred_prob[nn_pred >= 0.5] <- 'yes'

    confusionMatrix(nn_pred_prob, test_set$y)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   no  yes
    ##        no  1634  184
    ##        yes  191  343
    ##                                           
    ##                Accuracy : 0.8406          
    ##                  95% CI : (0.8251, 0.8551)
    ##     No Information Rate : 0.7759          
    ##     P-Value [Acc > NIR] : 3.464e-15       
    ##                                           
    ##                   Kappa : 0.5436          
    ##  Mcnemar's Test P-Value : 0.7567          
    ##                                           
    ##             Sensitivity : 0.8953          
    ##             Specificity : 0.6509          
    ##          Pos Pred Value : 0.8988          
    ##          Neg Pred Value : 0.6423          
    ##              Prevalence : 0.7759          
    ##          Detection Rate : 0.6947          
    ##    Detection Prevalence : 0.7730          
    ##       Balanced Accuracy : 0.7731          
    ##                                           
    ##        'Positive' Class : no              
    ## 

    nn_pred <- predict(nn_mdl,
                       newdata = test_set,
                       type = 'raw') %>% 
      round()

    confusionMatrix(nn_pred, as.numeric(test_set$y) - 1)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1510  122
    ##          1  315  405
    ##                                           
    ##                Accuracy : 0.8142          
    ##                  95% CI : (0.7979, 0.8297)
    ##     No Information Rate : 0.7759          
    ##     P-Value [Acc > NIR] : 3.113e-06       
    ##                                           
    ##                   Kappa : 0.5272          
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.8274          
    ##             Specificity : 0.7685          
    ##          Pos Pred Value : 0.9252          
    ##          Neg Pred Value : 0.5625          
    ##              Prevalence : 0.7759          
    ##          Detection Rate : 0.6420          
    ##    Detection Prevalence : 0.6939          
    ##       Balanced Accuracy : 0.7979          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

There is a small improvement in the accuracy of the model, from 82% to
83%, derived from the increase in neurons. We will keep the better model
for future comparison.

5.3. Decision Tree
------------------

A classification tree can also help predict variable classes:

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

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   no  yes
    ##        no  1693  132
    ##        yes  227  300
    ##                                           
    ##                Accuracy : 0.8474          
    ##                  95% CI : (0.8322, 0.8617)
    ##     No Information Rate : 0.8163          
    ##     P-Value [Acc > NIR] : 3.978e-05       
    ##                                           
    ##                   Kappa : 0.531           
    ##  Mcnemar's Test P-Value : 7.008e-07       
    ##                                           
    ##             Sensitivity : 0.8818          
    ##             Specificity : 0.6944          
    ##          Pos Pred Value : 0.9277          
    ##          Neg Pred Value : 0.5693          
    ##              Prevalence : 0.8163          
    ##          Detection Rate : 0.7198          
    ##    Detection Prevalence : 0.7759          
    ##       Balanced Accuracy : 0.7881          
    ##                                           
    ##        'Positive' Class : no              
    ## 

5.4. Random Forest
------------------

Also, we can use an ensemble model of decision trees to create a random
forest:

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

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   no  yes
    ##        no  1666  159
    ##        yes  181  346
    ##                                           
    ##                Accuracy : 0.8554          
    ##                  95% CI : (0.8406, 0.8694)
    ##     No Information Rate : 0.7853          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.578           
    ##  Mcnemar's Test P-Value : 0.2548          
    ##                                           
    ##             Sensitivity : 0.9020          
    ##             Specificity : 0.6851          
    ##          Pos Pred Value : 0.9129          
    ##          Neg Pred Value : 0.6565          
    ##              Prevalence : 0.7853          
    ##          Detection Rate : 0.7083          
    ##    Detection Prevalence : 0.7759          
    ##       Balanced Accuracy : 0.7936          
    ##                                           
    ##        'Positive' Class : no              
    ## 

5.5. Support Vector Machine
---------------------------

An SVM model can greatly outperform all others. Let us see how it can
perform:

    svm_model <- svm(y ~ ., data = train_set, kernel = "radial")

    summary(svm_model)

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
    ## Number of Support Vectors:  2075
    ## 
    ##  ( 1053 1022 )
    ## 
    ## 
    ## Number of Classes:  2 
    ## 
    ## Levels: 
    ##  no yes

    prediction <- predict(svm_model, test_set[,-17])

    table(pred = prediction,
          y = test_set$y) / length(prediction)

    ##      y
    ## pred          no        yes
    ##   no  0.70535714 0.09311224
    ##   yes 0.07057823 0.13095238

    table(pred = prediction,
          y = test_set[,17])

    ##      y
    ## pred    no  yes
    ##   no  1659  219
    ##   yes  166  308

    cm_svm <- confusionMatrix(prediction, test_set$y)

An accuracy of 83.63% is acceptable, but previous models already
outperform this one. Notice that the overall accuracy might not be the
best metric to analyze this models' performances. However, this will be
addressed later on. We can use cross validation techniques or fine-tune
the model's parameters to obtain better results:

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

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   no  yes
    ##        no  1647  232
    ##        yes  178  295
    ##                                           
    ##                Accuracy : 0.8257          
    ##                  95% CI : (0.8097, 0.8408)
    ##     No Information Rate : 0.7759          
    ##     P-Value [Acc > NIR] : 1.551e-09       
    ##                                           
    ##                   Kappa : 0.4797          
    ##  Mcnemar's Test P-Value : 0.008858        
    ##                                           
    ##             Sensitivity : 0.9025          
    ##             Specificity : 0.5598          
    ##          Pos Pred Value : 0.8765          
    ##          Neg Pred Value : 0.6237          
    ##              Prevalence : 0.7759          
    ##          Detection Rate : 0.7003          
    ##    Detection Prevalence : 0.7989          
    ##       Balanced Accuracy : 0.7311          
    ##                                           
    ##        'Positive' Class : no              
    ## 

82.57% accuracy is also good, but still not optimal. The cross validated
SVM can still be improved upon.

Using the `total_accuracy_svm` function from the lectures, we can try to
find the best values for the C and Gamma parameters:

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

The optimal values obtained are `cost` = 10 and `gamma` = 0.01, which
can be used to train the final SVM:

    imp_svm <-
      svm(y ~ .,
          data = train_set,
          cost = opt_cost,
          gamma = opt_gamma)

    pred_imp_svm <- predict(imp_svm, test_set[, -17])
    (cm_imp_svm <- confusionMatrix(pred_imp_svm, test_set$y))

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   no  yes
    ##        no  1667  211
    ##        yes  158  316
    ##                                           
    ##                Accuracy : 0.8431          
    ##                  95% CI : (0.8278, 0.8576)
    ##     No Information Rate : 0.7759          
    ##     P-Value [Acc > NIR] : 2.555e-16       
    ##                                           
    ##                   Kappa : 0.5321          
    ##  Mcnemar's Test P-Value : 0.006789        
    ##                                           
    ##             Sensitivity : 0.9134          
    ##             Specificity : 0.5996          
    ##          Pos Pred Value : 0.8876          
    ##          Neg Pred Value : 0.6667          
    ##              Prevalence : 0.7759          
    ##          Detection Rate : 0.7088          
    ##    Detection Prevalence : 0.7985          
    ##       Balanced Accuracy : 0.7565          
    ##                                           
    ##        'Positive' Class : no              
    ## 

With 84.31% accuracy, this is the best model obtained so far. Let us get
the SVM and prediction objects ready for posterior manipulation:

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

5.6. Adjustment and plots
-------------------------

We can plot the prediction values of the SVM models as seen in the
lectures:

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

![](Classification_project_files/figure-markdown_strict/graph_svm-1.png)

As well as the Principal Component projections that allow us to view the
decision boundary:

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

![](Classification_project_files/figure-markdown_strict/pca_svm-1.png)

In order to deal with the unreliability of the overall accuracy metric,
we will follow the class notes to display Receiver Operating
Characteristic curves of the models, as well as the Area Under the
Curve.

For that, we create `prediction` objects and store True and False
positives in a data frame:

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

Next, we can plot the curves using this dataframe:

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

![](Classification_project_files/figure-markdown_strict/roc_plot-1.png)

And compute the AUC metric:

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

    ##                                 AUC
    ## Logistic Regression          0.7979
    ## Neural Network               0.8128
    ## Decision Tree                0.8906
    ## Random Forest                0.8961
    ## CV Support Vector Machine    0.8962
    ## Tuned Support Vector Machine 0.9151

As expected, the Tuned SVM yields the best possible results from within
all the applied models, with the highest Area Under the Curve.
Nonetheless, it takes considerably longer to train than other models.
This is due to the fact that there are actually several models trained,
one for every combination of `cost` and `gamma` defined. Then, we find
the one with higher accuracy and train the final model with the optimal
parameters found in the step above.

Reference
=========

S. Moro, R. Laureano and P. Cortez. *Using Data Mining for Bank Direct
Marketing: An Application of the CRISP-DM Methodology.* 2011

Gonzalez-Hidalgo, M., Miro-Julia, M. *Class notes.* Statistical Learning
and Decision Making II. 2018.
