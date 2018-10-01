#The Rejection Sampling method is usually used to simulate data from an unknown distribution. To do this 
#one samples from a distribution that covers the suport of the unknown distribution and use certain 
#criteria for accepting/rejecting the sampled values. One way to do this is as follows (Rice, p 92).

# Step 1: Generate T with density m.

# Step 2: Generate U, uniform on [0,1] and independent of T. If M(T)*U ??? f(T), then let X = T (accept T). 
#Otherwise, go to Step 1 (Reject T).

#Where M(x) is a function such that M(x) ??? f(x) on [a,b].

#To keep things simple for myself I will be simulating a Beta distribution with 
#parameters 6 and 3 (f). To do this I will sample T's from a scaled uniform distribution (M), 
#and reject sampled values where M(T)*U ??? f(T). 


#In a plot of the beta distribution with parameters 6 and 3 we can see that the f(x) never goes above 3.
#For this reason I chose to scale the uniform distribution M by multiplying it by 3.
#Here is the R code to implement rejection sampling for 100,000 observations in this example

library(combinat) # combinatorics utilities
library(datasets) # The R Datasets Package
library(base)
library(stats)
library(graphics)
library(nmle)
library(lattice)
library(mcsm)
library(ggplot2)
library(reshape2)


x = seq(0, 1, by = 0.001)

y1 = pcauchy(x, 0, 1)
y2 = pnorm(x, 0, 1)
df <- data.frame(x, y1, y2)

g <- ggplot(df, aes(x))
g <- g + geom_line(aes(y = y1), colour = "red")
g <- g + geom_line(aes(y = y2), colour = "green")
g


sample.x = rcauchy(100000, 0, 1)
sample.x <- subset(sample.x, sample.x > -3 & sample.x < 3)
accept = c()

for(i in 1:length(sample.x)){
  U = runif(1, 0, 1)
  if(dcauchy(sample.x[i], 0, 1) * ((sqrt(2 * pi) * (exp(1)) ^ (-0.5))) * U <= dnorm(sample.x[i], 0, 1)) { 
    accept[i] = 'Yes' 
  } 
  else
    accept[i] = 'No'
}

# ((sqrt(2 * pi) * (exp(1)) ^ (-0.5))) = 1.520347

t = data.frame(sample.x, accept = factor(accept, levels= c('Yes','No')))

summary(t)


hist(t[,1][t$accept == 'Yes'], breaks = seq(-3, 3, 0.01), freq = FALSE, main = 'Histogram of X', xlab = 'X')
points(sample.x, dnorm(sample.x, 0, 1), col = "red")


#With 100,000 observations sampled, the data fits very well.
#We can look at the densities of both the accepted and rejected values to get an idea of what's going on.

qplot(sample.x, data = t, geom = 'density', color = accept)  
par(new = TRUE)
# qplot(x, db, geom = "line")


norm <- pnorm(sample.x)
simulated <- t
df <- data.frame(x = sample.x, norm, simulated)

# melt the data to a long format
df2 <- melt(data = df, id.vars = "x")

# plot, using the aesthetics argument 'colour'
# ggplot(data = df2, aes(x = x, y = value, colour = variable)) + geom_line()


df <- data.frame(x = rnorm(1000))
ggplot(df, aes(x)) + stat_density(kernel = "biweight")

x <- seq(-3, 3,length = 1000)
db <- dnorm(x, 0, 1)


qplot(x, db, geom = "line")

x <- seq(-3, 3, length = 100)
db <- 1.53 * dcauchy(x, 0, 1)


qplot(x, db, geom = "line")


ggplot() + geom_line(aes(x, db))

ggplot(data.frame(x = c(-2, 2)), aes(x)) + stat_function(fun = rnorm)
print(qplot(sample.x, data = T, geom = 'histogram', fill = accept, binwidth = 0.01))

#In fact, when I ran this example I got 33,114 accepted values and 66,886 rejected values.
#I probably could have chosen a better value than 3 to scale the uniform distribution, but ideally
#rejection sampling uses a known distribution that is only slightly different from the unknown 
#distribution we're trying to estimate.

