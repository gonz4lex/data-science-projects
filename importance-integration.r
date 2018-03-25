rm(list = ls(all = TRUE))

library(ggplot2)


# Calculate the integral from 0 to 1 of:

# f(x) = sqrt(1 - x ^ 2)


# Case 1: 
# G(x) = sqrt(1 - x ^ 2) / ((3 / 2) * (1 - x ^ 2))
# f(x) = (3 / 2) * (1 - x ^ 2)

x = seq(0, 1, by = 0.001)

f = function(x) {
  (3 / 2) * (1 - x ^ 2)}

optimize(f, c(-100, 100), maximum = TRUE)

y1 = 1.5 * dunif(x, 0, 1)
y2 = f(x)
df <- data.frame(x, y1, y2)

g <- ggplot(df, aes(x)) + 
  geom_line(aes(y = y1), colour = "red") + 
  geom_line(aes(y = y2), colour = "green")
g

x = seq(0, 10, by = 0.1)

hx = function(x) {
  ((3 / 2) * (1 - x ^ 2)) / dunif(x, 0, 1)
  }
y1 = hx(x)
df <- data.frame(x, y1)
g <- ggplot(df, aes(x)) + 
  geom_line(aes(y = y1), colour = "blue")
g

optimize(hx, c(0, 1), maximum = TRUE)
## hx is maxed at x = 1.5

sample.x = runif(10000,0,1)
accept = c()
sample.accept = c()



for(i in 1:length(sample.x)){
  U = runif(1, 0, 1)
  if(dunif(sample.x[i], 0, 1) * (1.5) * U <= f(sample.x[i])) { 
    accept[i] = 'Yes'
    sample.accept[i] = sample.x[i]
  } 
  else {
    accept[i] = 'No'
    sample.accept[i] = 0
  }
}



t = data.frame(sample.x, 
               accept = factor(accept, levels = c('Yes', 'No')),
               sample.accept)

t_acc = t[accept == 'Yes',]


G = function(x) {
  (sqrt(1 - x ^ 2)) / ((1.5) * (1 - x ^ 2))
}

n_acc = length(which(t$accept == 'Yes'))

I = sum(G(t_acc$sample.accept)) / n_acc
I

ff = function(x) {
  (sqrt(1 - x ^ 2))
}

Ireal = integrate(ff, 0, 1)
Ireal

# Variance
S = sqrt(sum((G(t_acc$sample.accept) 
              - sum(G(t_acc$sample.accept)) / n_acc) ^ 2) / (n_acc - 1))
S



# Case 2:
# G(x) =  sqrt(1 - x ^ 2) / ((5 / 4) * (1 - x ^ 4))
# f(x) = (5 / 4) * (1 - x ^ 4)


x = seq(0, 1, by = 0.001)

f = function(x) {
  (5 / 4) * (1 - x ^ 4)
}

optimize(f, c(-100, 100), maximum = TRUE)
## f is maxed at x = 1.25

y1 = 1.25 * dunif(x, 0, 1)
y2 = f(x)
df <- data.frame(x, y1, y2)

g <- ggplot(df, aes(x)) + 
  geom_line(aes(y = y1), colour = "red") + 
  geom_line(aes(y = y2), colour = "green")
g

x = seq(0, 10, by = 0.1)

hx = function(x) {
  ((5 / 4) * (1 - x ^ 4)) / dunif(x, 0, 1)
}

optimize(hx, c(0, 1), maximum = TRUE)
## hx is maxed at x = 1.25

y1 = hx(x)
df <- data.frame(x, y1)
g <- ggplot(df, aes(x)) + 
  geom_line(aes(y = y1), colour = "blue")
g

sample.x = runif(10000,0,1)
accept = c()
sample.accept = c()


for(i in 1:length(sample.x)){
  U = runif(1, 0, 1)
  if(dunif(sample.x[i], 0, 1) * (1.25) * U <= f(sample.x[i])) { 
    accept[i] = 'Yes'
    sample.accept[i] = sample.x[i]
  } 
  else {
    accept[i] = 'No'
    sample.accept[i] = 0
  }
}



t = data.frame(sample.x, 
               accept = factor(accept, levels = c('Yes', 'No')),
               sample.accept)

t_acc = t[accept == 'Yes',]


G = function(x) {
  sqrt(1 - x ^ 2) / ((5 / 4) * (1 - x ^ 4))
}

n_acc = length(which(t$accept == 'Yes'))

I = sum(G(t_acc$sample.accept)) / n_acc
I

Ireal = integrate(ff, 0, 1)
Ireal

# Variance
S2 = sqrt(sum((G(t_acc$sample.accept) 
              - sum(G(t_acc$sample.accept)) / n_acc) ^ 2) / (n_acc - 1))
S2


# Case 3:
# G(x) =  sqrt(1 - x ^ 2) / ((3 / 4) * (2 - 2 * x ^ 2))
# f(x) = (3 / 4) * (2 - 2 * x ^ 2)


x = seq(0, 1, by = 0.001)

f = function(x) {
  (3 / 4) * (2 - 2 * x ^ 2)
}

optimize(f, c(0, 1), maximum = TRUE)
## f is maxed at x = 1.5

y1 = 1.5 * dunif(x, 0, 1)
y2 = f(x)
df <- data.frame(x, y1, y2)

g <- ggplot(df, aes(x)) + 
  geom_line(aes(y = y1), colour = "red") + 
  geom_line(aes(y = y2), colour = "green")
g

x = seq(0, 10, by = 0.1)

hx = function(x) {
  ((3 / 4) * (2 - 2 * x ^ 2)) / dunif(x, 0, 1)
}

optimize(hx, c(0, 1), maximum = TRUE)
## hx is maxed at x = 1.5

y1 = hx(x)
df <- data.frame(x, y1)
g <- ggplot(df, aes(x)) + 
  geom_line(aes(y = y1), colour = "blue")
g

sample.x = runif(10000,0,1)
accept = c()
sample.accept = c()


for(i in 1:length(sample.x)){
  U = runif(1, 0, 1)
  if(dunif(sample.x[i], 0, 1) * (1.5) * U <= f(sample.x[i])) { 
    accept[i] = 'Yes'
    sample.accept[i] = sample.x[i]
  } 
  else {
    accept[i] = 'No'
    sample.accept[i] = 0
  }
}



t = data.frame(sample.x, 
               accept = factor(accept, levels = c('Yes', 'No')),
               sample.accept)

t_acc = t[accept == 'Yes',]


G = function(x) {
  sqrt(1 - x ^ 2) / ((3 / 4) * (2 - 2 * x ^ 2))
}

n_acc = length(which(t$accept == 'Yes'))

I = sum(G(t_acc$sample.accept)) / n_acc
I

Ireal = integrate(ff, 0, 1)
Ireal

# Variance
S3 = sqrt(sum((G(t_acc$sample.accept) 
               - sum(G(t_acc$sample.accept)) / n_acc) ^ 2) / (n_acc - 1))
S3

S ; S2 ; S3

# Using the importance integration method, the best decomposition 
# of the integral function is the one used in Case 2.
