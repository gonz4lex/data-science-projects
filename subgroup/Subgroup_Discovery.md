-   [Introduction](#introduction)
    -   [Quality measures](#quality-measures)
    -   [Quality measures](#quality-measures-1)
        -   [Complexity measures](#complexity-measures)
        -   [Generality measures](#generality-measures)
        -   [Accuracy measures](#accuracy-measures)
        -   [Interest measures](#interest-measures)
        -   [Hybrid measures](#hybrid-measures)
-   [Reference](#reference)

Introduction
============

Consider the dataset *USCensus1990.txt*, which contains 2,458,285
records on United States citizens over 68 qualitative variables. The aim
of this project is to apply several algorithms for subgroup discovery
and analyze meaningful groups and their interpretation. The project
heavily relies on the `rsubgroup` package, although some other methods
will be applied throughout the study.

First, let us load the dataset into memory and have a glance at its
structure:

    library(rsubgroup)
    library(tidyverse)
    library(subgroup.discovery)

    us1990 <- read.csv("./data/USCensus1990.txt",
                       sep = ",",
                       header = T)

    vars <- c("dAge", "iAvail", "iCitizen", "iClass", "iDisabl1",
              "iEnglish", "iFertil", "dHispanic", "dHours", "iLang1",
              "iMarital", "iMay75880", "iLooking", "iMeans",
              "iMilitary", "iMobility", "dPoverty",
              "iRlabor", "iRspouse", "iSchool", "iSex", "iVietnam",
              "iWork89", "iYearsch", "dYrsserv")
    # us1990 <- select(us1990, vars)

    set.seed(420)

    r_idx <- 1:nrow(us1990)

    r_idx <- sample(r_idx, trunc(length(r_idx) * 0.015))

    us1990 <- us1990[r_idx, ]

This is a massive dataset, and due to memory limitations we will select
a representative sample of 36874 rows which allows for a reduction in
computing time while keeping the meaningfulness of the analysis.
However, please note that this is not good practice and should be
avoided if possible because loss of information in the data may occur.

Although there are no missing data, when running some of the
`DiscoverSubgroups` tasks we will set the parameter `nodefaults = TRUE`
so that the first level of each variable is not taken into account.
According to the data description, many features use this level
correspondingly with the *Not Applies* label. Therefore, this extra step
allows for the removal of missing values in the dataset. However, this
setting will be overriden at times to allow for the study of two level
factors present within the data, such as in the case of the `iSex`
variable.

Moreover, since the data is read as integer values we will also convert
all of the features to factors so that they can be handled by the
discovery algorithm. We will also select a reduced number of variables
to use in the discovery tasks. Some variables holding no analytical
insight such as `caseid` are removed, as well as others considered to
have low predictive values. When using the `dIncome1` variable as
target, we also remove highly correlated features such as `dRearning`
and other `dIncome` variables.

    us1990[sapply(us1990, is.integer)] <- lapply(us1990[sapply(us1990, is.integer)], 
                                           as.factor)

Let us run a subgroup discovery task using the *Beam-Search* algorithm
and the *lift* quality function. *Lift* is correlation measure which
denotes interest: values greater than 1 indicate that the features used
in the task are positively correlated to the target variable. Lastly, we
will restrict the minimum size of each subgroup so that those with high
quality but containing a very low number of instances are excluded from
the analysis.

    inc_rs <- DiscoverSubgroups(us1990, 
                           as.target("dIncome1", "4"),
                           new("SDTaskConfig",
                               attributes = vars,
                               qf = "lift",
                               method = "beam",
                               minsize = 1500))

    ToDataFrame(inc_rs) %>%
      as.matrix()

    ##       quality p      size  
    ##  [1,] "8.49"  "0.17" "1509"
    ##  [2,] "8.49"  "0.17" "1509"
    ##  [3,] "8.49"  "0.17" "1509"
    ##  [4,] "8.49"  "0.17" "1509"
    ##  [5,] "8.47"  "0.17" "1512"
    ##  [6,] "8.47"  "0.17" "1512"
    ##  [7,] "8.47"  "0.17" "1512"
    ##  [8,] "8.47"  "0.17" "1512"
    ##  [9,] "8.44"  "0.17" "1500"
    ## [10,] "8.44"  "0.17" "1500"
    ## [11,] "8.44"  "0.17" "1500"
    ## [12,] "8.44"  "0.17" "1500"
    ## [13,] "8.44"  "0.17" "1518"
    ## [14,] "8.44"  "0.17" "1518"
    ## [15,] "8.44"  "0.17" "1518"
    ## [16,] "8.44"  "0.17" "1518"
    ## [17,] "8.44"  "0.17" "1518"
    ## [18,] "8.44"  "0.17" "1518"
    ## [19,] "8.44"  "0.17" "1518"
    ## [20,] "8.44"  "0.17" "1518"
    ##       description                                                                       
    ##  [1,] "dHours=5, iFertil=0, iMarital=0, iEnglish=0, dPoverty=2, iRlabor=1, iWork89=1"   
    ##  [2,] "dHours=5, iFertil=0, iMarital=0, iLang1=2, iRlabor=1, dPoverty=2, iWork89=1"     
    ##  [3,] "dHours=5, iSex=0, iMarital=0, iEnglish=0, iRlabor=1, dPoverty=2, iWork89=1"      
    ##  [4,] "dHours=5, iSex=0, iMarital=0, iLang1=2, dPoverty=2, iRlabor=1, iWork89=1"        
    ##  [5,] "dHours=5, iFertil=0, iMarital=0, iEnglish=0, dPoverty=2, iRlabor=1, dHispanic=0" 
    ##  [6,] "dHours=5, iFertil=0, iMarital=0, iLang1=2, iRlabor=1, dPoverty=2, dHispanic=0"   
    ##  [7,] "dHours=5, iSex=0, iMarital=0, iEnglish=0, iRlabor=1, dPoverty=2, dHispanic=0"    
    ##  [8,] "dHours=5, iSex=0, iMarital=0, iLang1=2, dPoverty=2, iRlabor=1, dHispanic=0"      
    ##  [9,] "dHours=5, iFertil=0, iMarital=0, iEnglish=0, dPoverty=2, iDisabl1=2, dHispanic=0"
    ## [10,] "dHours=5, iFertil=0, iMarital=0, iLang1=2, dPoverty=2, iDisabl1=2, dHispanic=0"  
    ## [11,] "dHours=5, iSex=0, iMarital=0, iEnglish=0, dPoverty=2, iDisabl1=2, dHispanic=0"   
    ## [12,] "dHours=5, iSex=0, iMarital=0, iLang1=2, dPoverty=2, iDisabl1=2, dHispanic=0"     
    ## [13,] "dHours=5, iFertil=0, iMarital=0, iEnglish=0, dPoverty=2, iRlabor=1, iAvail=0"    
    ## [14,] "dHours=5, iFertil=0, iMarital=0, iEnglish=0, dPoverty=2, iRlabor=1, iSex=0"      
    ## [15,] "dHours=5, iFertil=0, iMarital=0, iEnglish=0, dPoverty=2, iRlabor=1, iLang1=2"    
    ## [16,] "dHours=5, iFertil=0, iMarital=0, iLang1=2, iRlabor=1, dPoverty=2, iLooking=0"    
    ## [17,] "dHours=5, iFertil=0, iMarital=0, iLang1=2, iRlabor=1, dPoverty=2, iAvail=0"      
    ## [18,] "dHours=5, iFertil=0, iMarital=0, iLang1=2, iRlabor=1, dPoverty=2, iSex=0"        
    ## [19,] "dHours=5, iSex=0, iMarital=0, iEnglish=0, iRlabor=1, dPoverty=2, iLooking=0"     
    ## [20,] "dHours=5, iSex=0, iMarital=0, iEnglish=0, iRlabor=1, dPoverty=2, iAvail=0"

A few of the patterns give us a glimpse of some of the subgroups in the
data:

*Workers that work the most hours, working in the private for profit
sector. *Workers that are not Hispanic and were working in the previos
year. \*

Some of the variables, however, offer redundant information:
`dPoverty = 2` indicates people not in the poverty spectrum, which is
obvious when we are targetting the top values of the `dIncome1`
variable. Also `iSchool = 1` targets those who are not currently
attending school, which is also indicative of an adult in a top earning
position.

Also, please take note of the many appearances of `dHispanic = 0`, which
encloses workers that are *not* Hispanic. This points towards the fact
that South American workers are held in lower regard and are rarely
found among top earners.

Therefore, let us perform a small tweak to the attributes so that
irrelevant features are not taken into account:

    vars <- c("dAge", "iAvail", "iCitizen", "iClass", "iDisabl1",
              "iEnglish", "iFertil", "dHispanic", "dHours", "iLang1",
              "iMarital", "iMay75880", "iLooking", "iMeans",
              "iMilitary", "iMobility", "iRlabor",
              "iRspouse", "iSex", "iVietnam",
              "iWork89", "iYearsch", "dYrsserv")

We can also target the top groups of the `dRearning` variable, which is
defined as the total personal earnings and was excluded from the
previous task due to high correlation with the target `dIncome1`.

    earn_rs <- DiscoverSubgroups(us1990, 
                           as.target("dRearning", "4"),
                           new("SDTaskConfig",
                               attributes = vars,
                               qf = "lift",
                               method = "beam",
                               minsize = 1500))

    ToDataFrame(earn_rs) %>%
      as.matrix()

    ##       quality p      size  
    ##  [1,] "4.25"  "0.43" "1506"
    ##  [2,] "4.25"  "0.43" "1506"
    ##  [3,] "4.25"  "0.43" "1506"
    ##  [4,] "4.25"  "0.43" "1506"
    ##  [5,] "4.24"  "0.43" "1511"
    ##  [6,] "4.24"  "0.43" "1511"
    ##  [7,] "4.24"  "0.43" "1511"
    ##  [8,] "4.24"  "0.43" "1511"
    ##  [9,] "4.24"  "0.43" "1511"
    ## [10,] "4.24"  "0.43" "1511"
    ## [11,] "4.24"  "0.43" "1511"
    ## [12,] "4.24"  "0.43" "1511"
    ## [13,] "4.24"  "0.43" "1511"
    ## [14,] "4.24"  "0.43" "1511"
    ## [15,] "4.24"  "0.43" "1511"
    ## [16,] "4.24"  "0.43" "1511"
    ## [17,] "4.24"  "0.43" "1511"
    ## [18,] "4.24"  "0.43" "1511"
    ## [19,] "4.24"  "0.43" "1511"
    ## [20,] "4.24"  "0.43" "1511"
    ##       description                                                                  
    ##  [1,] "iSex=0, dAge=5, iMeans=1, dHispanic=0, iEnglish=0, iDisabl1=2, iWork89=1"   
    ##  [2,] "iSex=0, dAge=5, iMeans=1, dHispanic=0, iLang1=2, iDisabl1=2, iWork89=1"     
    ##  [3,] "dAge=5, iFertil=0, iMeans=1, dHispanic=0, iEnglish=0, iDisabl1=2, iWork89=1"
    ##  [4,] "dAge=5, iFertil=0, iMeans=1, dHispanic=0, iLang1=2, iDisabl1=2, iWork89=1"  
    ##  [5,] "iSex=0, dAge=5, iMeans=1, iEnglish=0, iDisabl1=2, iWork89=1"                
    ##  [6,] "iSex=0, dAge=5, iMeans=1, iDisabl1=2, iLang1=2, iWork89=1"                  
    ##  [7,] "dAge=5, iFertil=0, iMeans=1, iEnglish=0, iDisabl1=2, iWork89=1"             
    ##  [8,] "dAge=5, iFertil=0, iMeans=1, iDisabl1=2, iLang1=2, iWork89=1"               
    ##  [9,] "iSex=0, dAge=5, iMeans=1, iEnglish=0, iDisabl1=2, iLooking=0, iWork89=1"    
    ## [10,] "iSex=0, dAge=5, iMeans=1, iEnglish=0, iDisabl1=2, iAvail=0, iWork89=1"      
    ## [11,] "iSex=0, dAge=5, iMeans=1, iEnglish=0, iDisabl1=2, iFertil=0, iWork89=1"     
    ## [12,] "iSex=0, dAge=5, iMeans=1, iEnglish=0, iDisabl1=2, iLang1=2, iWork89=1"      
    ## [13,] "iSex=0, dAge=5, iMeans=1, iDisabl1=2, iLang1=2, iLooking=0, iWork89=1"      
    ## [14,] "iSex=0, dAge=5, iMeans=1, iDisabl1=2, iLang1=2, iAvail=0, iWork89=1"        
    ## [15,] "iSex=0, dAge=5, iMeans=1, iDisabl1=2, iLang1=2, iFertil=0, iWork89=1"       
    ## [16,] "dAge=5, iFertil=0, iLooking=0, iEnglish=0, iMeans=1, iDisabl1=2, iWork89=1" 
    ## [17,] "dAge=5, iFertil=0, iMeans=1, iEnglish=0, iDisabl1=2, iWork89=1, iAvail=0"   
    ## [18,] "dAge=5, iFertil=0, iMeans=1, iEnglish=0, iDisabl1=2, iWork89=1, iLang1=2"   
    ## [19,] "dAge=5, iFertil=0, iMeans=1, iDisabl1=2, iLang1=2, iWork89=1, iLooking=0"   
    ## [20,] "dAge=5, iFertil=0, iMeans=1, iDisabl1=2, iLang1=2, iWork89=1, iAvail=0"

When accounting for the default values, we can see some different
groups:

For instance, married workers that commute by car (`iMeans = 1`) and are
not disabled (`iDisabl1 = 2`) tend to be top earners. From this task, we
can derive two pieces of analytical insight: *Owning a car could be a
sign of high purchasing power. *The presence of many `iVietnam = 1`,
`iFertil = 0` and `iSex = 0` indicates that males are much more likely
to earn high wages. Coincidentally, many of this cases are also married
individuals in the 40 year old range as denoted by `dAge = 5` and
`iMarital = 0`, which may suggest that a family unit of working parents
which higher amounts of income to the household.

It is also fairly surprising that those who only speak English at their
homes also tend to be in the upper income brackets as denoted by
`iLang1 = 2` and `iEnglish = 0`. It would be straight-forward to assert
that this may be due to the immigrant status of some workers. For
example, US born citizens speak only English as they may have no need to
learn a second language, while immigrants in the States need to have
working proficiency of the English language. This assumption goes along
with the fact that mostly non-Hispanic workers earn comparatively more
than others as found in the previous section, as they might normally
speak Spanish at their homes.

    beam_rs <- DiscoverSubgroups(us1990, 
                           as.target("dIncome1", "4"),
                           new("SDTaskConfig",
                               attributes = vars,
                               qf = "lift",
                               method = "beam",
                               minsize = 1500,
                               nodefaults = TRUE))

    ToDataFrame(beam_rs) %>%
      as.matrix()

    ##       quality p      size  
    ##  [1,] "7.51"  "0.15" "1593"
    ##  [2,] "7.44"  "0.15" "1633"
    ##  [3,] "7.41"  "0.15" "1613"
    ##  [4,] "7.37"  "0.15" "1623"
    ##  [5,] "7.35"  "0.15" "1653"
    ##  [6,] "7.32"  "0.15" "1763"
    ##  [7,] "7.30"  "0.15" "1665"
    ##  [8,] "7.28"  "0.14" "1643"
    ##  [9,] "7.26"  "0.14" "1806"
    ## [10,] "7.22"  "0.14" "1787"
    ## [11,] "7.22"  "0.14" "1685"
    ## [12,] "7.21"  "0.14" "1805"
    ## [13,] "7.19"  "0.14" "1795"
    ## [14,] "7.16"  "0.14" "1830"
    ## [15,] "7.13"  "0.14" "1840"
    ## [16,] "7.11"  "0.14" "1857"
    ## [17,] "7.11"  "0.14" "1829"
    ## [18,] "7.10"  "0.14" "1819"
    ## [19,] "7.08"  "0.14" "1839"
    ## [20,] "7.03"  "0.14" "1864"
    ##       description                                                                 
    ##  [1,] "dHours=5, iMeans=1, iRspouse=1, iDisabl1=2, iLang1=2, iWork89=1, iRlabor=1"
    ##  [2,] "dHours=5, iMeans=1, iRspouse=1, iLang1=2, iWork89=1, iRlabor=1"            
    ##  [3,] "dHours=5, iMeans=1, iRspouse=1, iDisabl1=2, iLang1=2, iRlabor=1"           
    ##  [4,] "dHours=5, iMeans=1, iRspouse=1, iDisabl1=2, iLang1=2, iWork89=1"           
    ##  [5,] "dHours=5, iMeans=1, iRspouse=1, iLang1=2, iRlabor=1"                       
    ##  [6,] "dHours=5, iMeans=1, iRspouse=1, iDisabl1=2, iWork89=1, iRlabor=1"          
    ##  [7,] "dHours=5, iMeans=1, iRspouse=1, iLang1=2, iWork89=1"                       
    ##  [8,] "dHours=5, iMeans=1, iRspouse=1, iDisabl1=2, iLang1=2"                      
    ##  [9,] "dHours=5, iMeans=1, iRspouse=1, iWork89=1, iRlabor=1"                      
    ## [10,] "dHours=5, iMeans=1, iRspouse=1, iDisabl1=2, iRlabor=1"                     
    ## [11,] "dHours=5, iMeans=1, iRspouse=1, iLang1=2"                                  
    ## [12,] "dHours=5, iRspouse=1, iDisabl1=2, iLang1=2, iWork89=1, iRlabor=1"          
    ## [13,] "dHours=5, iMeans=1, iRspouse=1, iDisabl1=2, iWork89=1"                     
    ## [14,] "dHours=5, iMeans=1, iRspouse=1, iRlabor=1"                                 
    ## [15,] "dHours=5, iMeans=1, iRspouse=1, iWork89=1"                                 
    ## [16,] "dHours=5, iRspouse=1, iLang1=2, iWork89=1, iRlabor=1"                      
    ## [17,] "dHours=5, iRspouse=1, iDisabl1=2, iLang1=2, iRlabor=1"                     
    ## [18,] "dHours=5, iMeans=1, iRspouse=1, iDisabl1=2"                                
    ## [19,] "dHours=5, iRspouse=1, iDisabl1=2, iLang1=2, iWork89=1"                     
    ## [20,] "dHours=5, iMeans=1, iRspouse=1"

The `iClass = 1`component of most of the rules indicate that employees
of private companies tend to have earning in higher levels than state
workers or self employed individuals.

There are also a lot of high income subgroups that contain workers
without any disabilities that were also working during the previous year
at a private, for profit company.

As a sidenote, take into account that using another measure such as the
Weighted Relative Accuracy gives extremely low quality values, around
0.02 for all subgroups.

Let us apply a different method and quality function now: the *SD-Map*
algorithm and the *Piatetsky-Shapiro* quality function:

    sdmap_rs <- DiscoverSubgroups(us1990, 
                           as.target("dIncome1", "4"),
                           new("SDTaskConfig",
                               attributes = vars,
                               qf = "ps",
                               method = "sdmap",
                               minsize = 1500,
                               nodefaults = TRUE))

    ToDataFrame(sdmap_rs) %>%
      as.matrix()

    ##       quality  p      size   
    ##  [1,] "400.62" "0.06" "10820"
    ##  [2,] "399.48" "0.05" "11430"
    ##  [3,] "397.33" "0.06" " 9830"
    ##  [4,] "395.59" "0.06" " 9465"
    ##  [5,] "393.53" "0.06" "10021"
    ##  [6,] "392.11" "0.06" " 9640"
    ##  [7,] "381.65" "0.06" " 9613"
    ##  [8,] "380.86" "0.06" "10155"
    ##  [9,] "376.89" "0.06" " 8747"
    ## [10,] "374.57" "0.06" " 8411"
    ## [11,] "373.84" "0.06" " 8900"
    ## [12,] "371.77" "0.06" " 8552"
    ## [13,] "367.61" "0.04" "15543"
    ## [14,] "365.79" "0.04" "16187"
    ## [15,] "359.70" "0.04" "15940"
    ## [16,] "357.27" "0.04" "16615"
    ## [17,] "356.15" "0.06" " 8834"
    ## [18,] "354.24" "0.06" " 8930"
    ## [19,] "353.27" "0.06" " 8979"
    ## [20,] "353.26" "0.06" " 8527"
    ##       description                                             
    ##  [1,] "iRspouse=1, iWork89=1, iDisabl1=2"                     
    ##  [2,] "iRspouse=1, iWork89=1"                                 
    ##  [3,] "iRspouse=1, iRlabor=1, iWork89=1"                      
    ##  [4,] "iRspouse=1, iRlabor=1, iDisabl1=2, iWork89=1"          
    ##  [5,] "iRspouse=1, iRlabor=1"                                 
    ##  [6,] "iRspouse=1, iRlabor=1, iDisabl1=2"                     
    ##  [7,] "iRspouse=1, iWork89=1, iLang1=2, iDisabl1=2"           
    ##  [8,] "iRspouse=1, iWork89=1, iLang1=2"                       
    ##  [9,] "iRspouse=1, iRlabor=1, iLang1=2, iWork89=1"            
    ## [10,] "iRspouse=1, iRlabor=1, iLang1=2, iDisabl1=2, iWork89=1"
    ## [11,] "iRspouse=1, iRlabor=1, iLang1=2"                       
    ## [12,] "iRspouse=1, iRlabor=1, iLang1=2, iDisabl1=2"           
    ## [13,] "iRlabor=1, iDisabl1=2, iWork89=1"                      
    ## [14,] "iRlabor=1, iWork89=1"                                  
    ## [15,] "iRlabor=1, iDisabl1=2"                                 
    ## [16,] "iRlabor=1"                                             
    ## [17,] "iMeans=1, iRspouse=1, iWork89=1, iRlabor=1"            
    ## [18,] "iMeans=1, iRspouse=1, iWork89=1"                       
    ## [19,] "iMeans=1, iRspouse=1, iRlabor=1"                       
    ## [20,] "iMeans=1, iRspouse=1, iDisabl1=2, iWork89=1, iRlabor=1"

Most of these insights are identical or very similar to the ones found
in the approach above, validating both methods as efficient in the
discovery of relevant subgroups.

We can also set the parameter `postfilter = sig-improve-set`, which
applies a post-processing filter that removes patterns that do not
significantly improve at a 5% confidence interval with regards to all
generalizations in the result set.

    sig_rs <- DiscoverSubgroups(us1990, 
                           as.target("dIncome1", "4"),
                           new("SDTaskConfig",
                               attributes = vars,
                               qf = "ps",
                               method = "sdmap",
                               minsize = 1500,
                               nodefaults = TRUE,
                               postfilter = "sig-improve-set"))

    ToDataFrame(sig_rs) %>%
      as.matrix()

    ##       quality  p      size   
    ##  [1,] "400.62" "0.06" "10820"
    ##  [2,] "399.48" "0.05" "11430"
    ##  [3,] "397.33" "0.06" " 9830"
    ##  [4,] "395.59" "0.06" " 9465"
    ##  [5,] "393.53" "0.06" "10021"
    ##  [6,] "392.11" "0.06" " 9640"
    ##  [7,] "381.65" "0.06" " 9613"
    ##  [8,] "380.86" "0.06" "10155"
    ##  [9,] "376.89" "0.06" " 8747"
    ## [10,] "374.57" "0.06" " 8411"
    ## [11,] "373.84" "0.06" " 8900"
    ## [12,] "371.77" "0.06" " 8552"
    ## [13,] "367.61" "0.04" "15543"
    ## [14,] "365.79" "0.04" "16187"
    ## [15,] "359.70" "0.04" "15940"
    ## [16,] "357.27" "0.04" "16615"
    ## [17,] "354.24" "0.06" " 8930"
    ##       description                                             
    ##  [1,] "iRspouse=1, iWork89=1, iDisabl1=2"                     
    ##  [2,] "iRspouse=1, iWork89=1"                                 
    ##  [3,] "iRspouse=1, iRlabor=1, iWork89=1"                      
    ##  [4,] "iRspouse=1, iRlabor=1, iDisabl1=2, iWork89=1"          
    ##  [5,] "iRspouse=1, iRlabor=1"                                 
    ##  [6,] "iRspouse=1, iRlabor=1, iDisabl1=2"                     
    ##  [7,] "iRspouse=1, iWork89=1, iLang1=2, iDisabl1=2"           
    ##  [8,] "iRspouse=1, iWork89=1, iLang1=2"                       
    ##  [9,] "iRspouse=1, iRlabor=1, iLang1=2, iWork89=1"            
    ## [10,] "iRspouse=1, iRlabor=1, iLang1=2, iDisabl1=2, iWork89=1"
    ## [11,] "iRspouse=1, iRlabor=1, iLang1=2"                       
    ## [12,] "iRspouse=1, iRlabor=1, iLang1=2, iDisabl1=2"           
    ## [13,] "iRlabor=1, iDisabl1=2, iWork89=1"                      
    ## [14,] "iRlabor=1, iWork89=1"                                  
    ## [15,] "iRlabor=1, iDisabl1=2"                                 
    ## [16,] "iRlabor=1"                                             
    ## [17,] "iMeans=1, iRspouse=1, iWork89=1"

Note that this discovery task return less groups than the previous ones,
after eliminating those not significative at 5% level.

Quality measures
----------------

    table(us1990$dIncome1 == "4") # target variable  

    ## 
    ## FALSE  TRUE 
    ## 36140   734

    table(us1990$iRspouse == "1" 
          & us1990$iWork89 == "1" 
          & us1990$iDisabl1 == "2" 
          & us1990$dPoverty == "2") # satisfy antecedent

    ## 
    ## FALSE  TRUE 
    ## 26413 10461

    n_cond <- 14007 # true of previous table
    ns <- nrow(us1990) # total

    table(us1990$iRspouse == "1"
          & us1990$iWork89 == "1"
          & us1990$iDisabl1 == "2"
          & us1990$dPoverty == "2"
          & us1990$dIncome1 == "4") # satisfy whole rule

    ## 
    ## FALSE  TRUE 
    ## 36258   616

    n_target_cond <- 821 # true of previous table

    table(us1990$iRspouse == "1"
          & us1990$iWork89 == "1"
          & us1990$iDisabl1 == "2"
          & us1990$dPoverty == "2"
          & us1990$dIncome1 != "4") # satisfy antecent but not consequent

    ## 
    ## FALSE  TRUE 
    ## 27029  9845

    no_n_target_cond <- 13186 # true of previous table
    n_target <- nrow(us1990) # total rows

    true_pos <- 54 # true positives
    all_pos <- nrow(us1990) # all positives

    # table(us1990$dIncome1 == "0")
    # table(us1990$dIncome1 == "1")
    # table(us1990$dIncome1 == "2")
    # table(us1990$dIncome1 == "3")
    table(us1990$dIncome1 != "4")

    ## 
    ## FALSE  TRUE 
    ##   734 36140

    false_pos <- 956 # false positives
    all_false <- 24614 + 11337+ 7585 + 4652 # all 

    table(us1990$iRspouse != "1"
          & us1990$iWork89 != "1"
          & us1990$iDisabl1 != "2"
          & us1990$dPoverty != "2"
          & us1990$dIncome1 == "4") # satisfy target variable but not antecedent

    ## 
    ## FALSE 
    ## 36874

    table(us1990$iRspouse != "1"
          & us1990$iWork89 != "1"
          & us1990$iDisabl1 != "2"
          & us1990$dPoverty != "2"
          & us1990$dIncome1 != "4") # do not satisfy any antecedent or consequent of rule

    ## 
    ## FALSE  TRUE 
    ## 34491  2383

    true_neg <- 0 
    no_n_target_no_cond <- 3208

    c <- 10
    g <- 100

Quality measures
----------------

Let us take one of the models and analyze the quality measures found in
the lecture notes. For this section, we will be referring to the
improved significance model `sig_rs`, configured with a
*Piatetsky-Shapiro* quality function and the *SD-Map* algorithm and its
first rule, *"iRspouse=1, iWork89=1, iDisabl1=2, dPoverty=2"*.

### Complexity measures

    rule_num <- 18 # number rules returned by the task
    num_var <- (4 + 3 + 4 + 3 + 5 + 3 + 2 + 4 + 3
                + 5 + 4 + 4 + 4 + 2 + 3 + 5 + 4 + 4) # variables within each of the rules

    rule_num / num_var

    ## [1] 0.2727273

### Generality measures

*Coverage* measures the average proportion of examples covered, while
*support* measures the percentage of correctly classified cases covered
by the rule.

    cover <- (n_cond / ns) *100

    support <- n_target_cond / ns

    cover ; support

    ## [1] 37.98611

    ## [1] 0.02226501

### Accuracy measures

This measures are concerned with the precision of the subgroups and are
used to extract association and classification rules.

-Accuracy measures the relative frequency of examples that satisfy the
rules completely, over those that only satisfy the antecedent.

-Precision (Qc): measures the balance among true and false positives
covered with a linear function.

-Precision (Qg) measures the balance between the number of cases
perfectly classified and the rarity of their distributions.

Both precision measures have generalization parameters *c* and *g*,
which are set to 10 and 100 for this project.

    accuracy <- n_target_cond / n_cond
    qc <- true_pos - (c * no_n_target_cond)
    qg <- true_pos / (false_pos + g)

    accuracy ; qc ; qg

    ## [1] 0.05861355

    ## [1] -131806

    ## [1] 0.05113636

### Interest measures

These measures select and sort patterns according to the user's
interest.

-Novelty is used to detect unusual and novel groups.

    novelty <- n_target_cond - (n_target * n_cond)

    novelty

    ## [1] -516493297

### Hybrid measures

These measures try to balance generality, interest and accuracy

-Sensitivity is the proportion of individuals with the correct value of
the target variable that have been correctly classified. -False Alarm is
a measure that covers the examples that do not satisfy the target
variable. -Rarity is a measure defined as the weighted relative accuracy
of a rule.

    sensitivity <- true_pos / all_pos

    false_alarm <- false_pos / (all_false)

    specificity <- true_neg / (true_neg + no_n_target_no_cond)

    wracc <- cover * (accuracy - (n_target / ns))

    sensitivity ; false_alarm ; specificity ; wracc

    ## [1] 0.001464446

    ## [1] 0.01983896

    ## [1] 0

    ## [1] -35.75961

Reference
=========

Massanet, S. *Subgroup Discovery*. Class notes, *New Trends in Data
Mining*. 2017
