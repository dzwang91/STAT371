
#########################################################################
#  Confidence Intervals             
#  STAT 371-3 Spring 2018
#  Author: Duzhe Wang                  
#########################################################################

##Case1:CIs for a normal mean, standard deviation sigma is known #####

mu = 5
sigma = 1
n = 25
alpha = 0.05

lowers = NULL
uppers = NULL
covers = 0
for (i in 1:100) {
  data = rnorm(n,mean = mu, sd = sigma); 
  samp_mean = mean(data); 
  zvalue = qnorm(alpha/2, lower.tail = F); 
  lower = samp_mean - zvalue*sigma/sqrt(n); 
  upper = samp_mean + zvalue*sigma/sqrt(n); 
  lowers = c(lowers, lower); 
  uppers = c(uppers, upper); 
  plot(1, type="n", xlab="x", ylab="CI #", xlim=c(min(lowers)-0.5, max(uppers)+0.5), ylim=c(1, 100), 
       main = "Long Run CI coverage for population mean");
  abline(v = mu, col = "red"); 
  for(j in 1:i){
  segments(x0 = lowers[j], y0 = j, x1 = uppers[j], y1 = j, col = "blue", lwd = 2)
    }; 
  covers = covers + sum(mu>=lower&mu<=upper); 
  print(c(samp_mean,100*covers/i))
}


##Case 2: CIs for a normal mean, standard deviatioin is unknown #######
rm(list=ls())
mu = 1.25
sigma = 0.5
n = 16
alpha = 0.05

lowers = NULL
uppers = NULL
covers = 0
for (i in 1:100) {
  data = rnorm(n,mean = mu, sd = sigma); 
  samp_mean = mean(data); 
  samp_sd = sd(data);
  tvalue = qt(alpha/2,df=n-1, lower.tail = F); 
  lower = samp_mean - tvalue*samp_sd/sqrt(n); 
  upper = samp_mean + tvalue*samp_sd/sqrt(n); 
  lowers = c(lowers, lower); 
  uppers = c(uppers, upper); 
  plot(1, type="n", xlab="x", ylab="CI #", xlim=c(min(lowers)-0.5, max(uppers)+0.5), ylim=c(1, 100), 
       main = "Long Run CI coverage for population mean");
  abline(v = mu, col = "red"); 
  for(j in 1:i){
    segments(x0 = lowers[j], y0 = j, x1 = uppers[j], y1 = j, col = "blue", lwd = 2)
  }; 
  covers = covers + sum(mu>=lower&mu<=upper); 
  print(c(samp_mean,100*covers/i))
}






