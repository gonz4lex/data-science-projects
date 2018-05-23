---
title: "New Trends in Data Mining"
subtitle: "Subgroup Discovery"
author: "Alex Gonzalez"
date: "23 de mayo de 2018"
output: html_document:
  keep_md: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, fig.align = 'center', fig.path = "figures/")
```

# Introduction

Consider the dataset *USCensus1990.txt*, which contains 2,458,285 records on United States citizens over 68 qualitative variables. The aim of this project is to apply several algorithms for subgroup discovery and analyze meaningful groups and their interpretation. The project heavily relies on the ```rsubgroup``` package, although some other methods will be applied throughout the study.

First, let us load the dataset into memory and have a glance at its structure:

```{r library, echo = TRUE}
library(rsubgroup)
library(tidyverse)
```


```{r data}
us1990 <- read.csv("./data/USCensus1990.txt",
                   sep = ",",
                   header = T)
```

```{r init}
# dim(us1990)
# str(us1990)
# 
# us1990 %>% 
#   is.na() %>% 
#   sum()
```

```{r}
r_idx <- 1:nrow(us1990) ; c_idx <- 1:ncol(us1990)

r_idx <- sample(r_idx, trunc(length(r_idx) * 0.001))
c_idx <- sample(c_idx, trunc(length(c_idx) * 0.3))

us1990 <- us1990[, c_idx]
```

```{r}
us1990[sapply(us1990, is.integer)] <- lapply(us1990[sapply(us1990, is.integer)], 
                                       as.factor)

```



```{r}
rs = DiscoverSubgroups(us1990, 
                       as.target("dIncome1", "4"),
                       new("SDTaskConfig",
                           attributes = names(us1990)))
ToDataFrame(rs)

```

```{r}
names(us1990)
```

