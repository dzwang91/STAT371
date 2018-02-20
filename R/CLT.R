#-----------CLT Simulation Code--------------#
# Author: Duzhe Wang
# Only used for STAT371-L3 Spring 2018.


rm(list=ls())
set.seed(0311)

# 1) Bimodal

## First: we create the population distribution: a mixture of two normal distributions. We flip an unfair coin(head probability 0.3, 
## tail probability 0.7), if the coin is a head, then we draw the sample from N(-5.3, 2^2), if the coin is a tail, then we draw the sample
## from N(10.7, 3^2). 

coin = rbinom(10000, 1, 0.3)
Population = coin*rnorm(10000, mean = -5.3, sd = 2) + (1-coin)*rnorm(10000, mean = 10.7, sd = 3)

density = function(x) 0.3*dnorm(x, mean = -5.3, sd = 2) + 0.7*dnorm(x, mean = 10.7, sd = 3)

hist(Population, freq = F, main = "Population Distribution, n = 1", xlab = "x", ylab = "Density", ylim = c(0, density(0) + 0.1))
curve(density, col = "blue", add = TRUE)

## Next: we simulate the CLT. 

n1=2
mean = replicate(10000, mean(sample(Population, n1, replace = TRUE)))
norm_approx = function(x) dnorm(x, mean = mean(Population), sd = sqrt(var(Population)/n1))
hist(mean, freq = F, main = paste("Distribution of Sample Mean, n =", n1, sep = " "), ylim = c(0, norm_approx(mean(Population))+0.1))
curve(norm_approx, col = "red", add = TRUE)

n2=10

mean = replicate(10000, mean(sample(Population, n2, replace = TRUE)))
norm_approx = function(x) dnorm(x, mean = mean(Population), sd = sqrt(var(Population)/n2))
hist(mean, freq = F, main = paste("Distribution of Sample Mean, n =", n2, sep = " "), ylim = c(0, norm_approx(mean(Population))+0.1))
curve(norm_approx, col = "red", add = TRUE)

n3=50

mean = replicate(10000, mean(sample(Population, n3, replace = TRUE)))
norm_approx = function(x) dnorm(x, mean = mean(Population), sd = sqrt(var(Population)/n3))
hist(mean, freq = F, main = paste("Distribution of Sample Mean, n =", n3, sep = " "), ylim = c(0, norm_approx(mean(Population))+0.1))
curve(norm_approx, col = "red", add = TRUE)

n4=100

mean = replicate(10000, mean(sample(Population, n4, replace = TRUE)))
norm_approx = function(x) dnorm(x, mean = mean(Population), sd = sqrt(var(Population)/n4))
hist(mean, freq = F, main = paste("Distribution of Sample Mean, n =", n4, sep = " "), ylim = c(0, norm_approx(mean(Population))+0.1))
curve(norm_approx, col = "red", add = TRUE)

n5=1000

mean = replicate(10000, mean(sample(Population, n5, replace = TRUE)))
norm_approx = function(x) dnorm(x, mean = mean(Population), sd = sqrt(var(Population)/n5))
hist(mean, freq = F, main = paste("Distribution of Sample Mean, n =", n5, sep = " "), ylim = c(0, norm_approx(mean(Population))+0.1))
curve(norm_approx, col = "red", add = TRUE)




