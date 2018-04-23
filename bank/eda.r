

load.libraries <- c('gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'tidyverse', 'ggthemes', 'reshape2')
# install.lib <- load.libraries[!load.libraries %in% installed.packages()]
# for(libs in install.lib) install.packages(libs, dependences = TRUE)

sapply(load.libraries, require, character = TRUE)

bank <- read.csv("./data/bank-full.csv",  sep = ";", na.strings = "unknown")

dim(bank)
str(bank)
summary(bank)

bank %>% 
  group_by(y) %>% 
  summarise(count = n())

## function to convert to month number

abb2m <- function(x) match(tolower(x), tolower(month.abb))
bank$month <- as.factor(abb2m(bank$month))

# Lines of report
# 
# Plot missing data

plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + 
    geom_tile(aes(x = x, y = y, fill = factor(m))) + 
    scale_fill_manual(values = c("white", "black"), name = "Missing\n(0=Yes, 1=No)") + 
    theme_light() + 
    ylab("") + xlab("") + 
    ggtitle(title)
}


plot_Missing(bank[,colSums(is.na(bank)) > 0])

# 
# 
# 
# 
# 
# Show distributions of numerical variables.

'my_plots <- lapply(names(bank), function(var_x){
  p <- 
    ggplot(bank) +
    aes_string(var_x) + 
    labs(subtitle = var_x)
  
  if(is.numeric(bank[[var_x]])) {
    p <- p + geom_density()
      
    
  } else {
    p <- p + geom_bar() + 
      coord_flip()
  } 
  
  p  +
    theme(plot.title = element_text(hjust = .5), 
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_line(colour = "gray80")) + 
    theme_fivethirtyeight()
  
})

plot_grid(plotlist = my_plots)'

# Show distribution of target variable by month.
# What percentage of potential clients accepted to subscribe?
# Check interesting traits of month age and job. Plot distributions.
# Plot density of each target group.


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

## This variables, except day and age, have extremley longtailed distibutions

## plot non numerical variables similar to above

## random test for facet grid

bank %>% 
  select_if(negate(is.numeric)) %>% 
  ggplot() +
  geom_bar(aes(x = default, fill = y)) + 
  facet_grid(housing ~ loan, scales = 'free') +
  coord_flip() +
  labs(title = "Distribution of target variable",
       subtitle = 'individual proportion over jobs') + 
  theme(plot.title = element_text(hjust = .5), 
        panel.grid.minor.x = element_line(colour = "gray80"),
        strip.text.y = element_blank()) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()


# Plot scatter of explanatory variable against count of clients, color by target variable.



# Plot jitter of campaign calls against duration of call, color by tv.

bank %>% 
  ggplot(aes(x = duration, y = campaign, color = y)) + 
  geom_jitter() + 
  labs(title = "placeholder",
       subtitle = 'placeholder') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x= element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = 'blue', no ='red')) + 
  theme_fivethirtyeight()

bank %>% 
  ggplot(aes(x = campaign, y = duration, color = y)) +
  geom_jitter(stat = "identity") + 
  labs(title = "placeholder",
       subtitle = 'placeholder') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_color_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()


# Plot histogram of count of clients by tv distributed by number of calls, add loess.

bank %>% 
  ggplot(aes(x = campaign, fill = y)) +
  geom_histogram(position = 'dodge') + 
  labs(title = "placeholder",
       subtitle = 'placeholder') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight() +
  coord_cartesian(xlim = c(0, 20))

# Plot density of job status by tv.


# Plot distribution over loan status.



# Age pyramid?

bank %>% 
  ggplot(aes(x = age)) +
  geom_density(aes(x = age, fill = y), alpha = .6) +
  labs(title = "placeholder",
       subtitle = 'placeholder') + 
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()

## older and younger clients tend to subscribe more often than midde aged.
## correlated with job? 



## other stuff

bank %>% 
  ggplot(aes(y)) +
  geom_bar(aes(fill = y)) +
  xlab("Frequency of target variable") +
  ylab("Absolute frequency") + 
  theme_fivethirtyeight() + 
  labs(title = "placeholder",
       subtitle = 'placeholder') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()

myCorr <- function(data, mapping, method = "loess", ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_jitter(size = .25) +
    geom_smooth(method = method, color = 'red', size = .5)
  p
}

# ggpairs(bank %>% 
#           filter(y == 'yes') %>%
#           select(age, job, marital, education, default, y),
#           lower = list(continuous = myCorr)) + theme_fivethirtyeight(10)
  
bank %>% 
  group_by(y, job) %>% 
  summarise(count = n()) %>% 
  arrange(count) %>% 
  mutate(freq = count / sum(count)) %>%
  ggplot(aes(x = reorder(job, -freq), y = freq, fill = y)) +
  geom_bar(stat = "identity", width = .5) + coord_flip() + 
  labs(title = "Distribution of target variable",
       subtitle = 'individual proportion over jobs') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()

## interesting that retired and unemployed seem to have higher percentage of suscription

bank %>% 
  group_by(y, loan) %>% 
  summarise(count = n()) %>% 
  arrange(count) %>%
  ggplot() +
  geom_bar(x = loan)

bank %>% 
  ggplot(aes(x = balance)) +
  geom_density(aes(fill = y)) + 
  labs(title = "Balance density",
       subtitle = 'analyzed over the full dataset') + 
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight() +
  coord_cartesian(xlim = c(0, 50000))

##↑ seems that there is a higher tendency to reject a deposit when the balance is lower

bank %>% 
  select(month, balance, y) %>% 
  ggplot(aes(month, balance)) + 
  geom_boxplot(aes(fill = y)) + 
  coord_flip() +
  coord_cartesian(ylim = c(-3000, 11000)) +
  xlab('Campaign month') +
  labs(title = "placeholder",
       subtitle = 'placeholder') + 
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()




bank %>% 
  group_by(month, y) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count / sum(count)) %>% 
  ggplot(aes(x = month, y = prop * 100)) +
  geom_bar(aes(fill = y), stat = 'identity') +
  # geom_density(aes(fill = y), alpha = .6) +
  labs(title = "placeholder",
       subtitle = 'placeholder') + 
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_fill_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight()


## enormous quantity of outliers

correlations <- 
  bank %>%
  select_if(is.numeric) %>% 
  na.omit() %>% 
  cor()


# correlations are very low
corrplot(correlations, method = "square")


bank %>% 
  group_by(month, y) %>% 
  summarise(count = n()) %>% 
  ggplot() +
  geom_point(aes(x = month, y = count, color = y)) +
  labs(title = "placeholder",
       subtitle = 'placeholder') + 
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_line(colour = "gray80")) + 
  scale_color_manual(name = '', values = c(yes = '#377EB8', no ='#E41A1C')) + 
  theme_fivethirtyeight() +
  coord_flip()
