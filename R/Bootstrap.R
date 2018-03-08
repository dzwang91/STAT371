## STAT371-L3, Spring 2018
## Bootstrap code for second hand smoke example 


#########################################################
## You can run bootstrap CI using the following chunk. 
#########################################################

data = c(29,30,53,75,34,21,12,58,117,119,115,134,253,289,287)
n = length(data)
xbar = mean(data)
s = sd(data)

b = 1000       # number of bootstrap samples
alpha = 0.05   #for 95% CI

resamples = matrix(sample(data, n*b, replace = TRUE), nrow = b, ncol = n) 
#bootstrap samples collected in matrix

t_hat_stat = function(x) {
             (mean(x) - xbar)/(sd(x)/sqrt(length(x))) #define the statistic for calculating t_hat
             }

t_hats = sort(apply(resamples, 1, t_hat_stat)) 
#You can google what apply function does. 
#compute t_hat for each row of resamples. This should yield b values of t_hat


t_lower = t_hats[b*alpha/2]    #Calculate critical values
t_upper = t_hats[b*(1-alpha/2)] 

CI_lower = xbar-t_upper*s/sqrt(n)
CI_upper = xbar-t_lower*s/sqrt(n) 

print(c(CI_lower, CI_upper))


# the following two lines are used to plot the histogram
hist(t_hats, freq = F, main = "Bootstrap Distribution of T", xlab = "t", ylab = "density", col = "gray")
abline(v = c(t_lower, t_upper), col = "red")

#########################################################################
## You can also run bootstrap CI using the following different chunk. 
#########################################################################

rm(list=ls( ))   # clear space

data = c(29,30,53,75,34,21,12,58,117,119,115,134,253,289,287)
xbar = mean(data)
s = sd(data)
b = 1000 # number of bootstrap samples
alpha = 0.05 #for 95% CI

# build a bootstrap function
bootest=function(data,number) {
  bootstat=NULL
  truemean=mean(data)
  for(i in 1:number) {
    samp=sample(data, size = length(data), replace = T)
    bootmean=mean(samp)
    bootsd=sd(samp)
    bootstat[i]=(bootmean - truemean)/(bootsd/sqrt(length(data)))
  }
  return(bootstat)
}

# run bootstrap 1000 times
bootdata=bootest(data,b)  
lower=quantile(bootdata, probs=alpha/2)
upper=quantile(bootdata, probs=1-alpha/2)
CI_lower=xbar-upper*s/sqrt(n)
CI_upper=xbar-lower*s/sqrt(n) 
print(c(CI_lower, CI_upper))








