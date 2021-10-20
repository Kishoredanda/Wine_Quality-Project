red_wine <- read.csv(file = "E:/Stat Methods/winequality-red.csv", header = T, sep = ";")
str(red_wine)
summary(red_wine)

#changing the margin size
par("mar")
par(mar=c(1,1,1,1))

#plot histogram using ggplot 
library(ggplot2)
ggplot(red_wine) +
#  aes(x = red_wine$fixed.acidity) +
#  aes(x = red_wine$volatile.acidity) +
#  aes(x = red_wine$citric.acid) +
#  aes(x = red_wine$residual.sugar) +
#  aes(x = red_wine$chlorides) +
#  aes(x = red_wine$free.sulfur.dioxide) +
#  aes(x = red_wine$total.sulfur.dioxide) +
#  aes(x = red_wine$density) +
#  aes(x = red_wine$pH) +
#  aes(x = red_wine$sulphates) +
   aes(x = red_wine$alcohol) +
#  aes(x = red_wine$quality) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()
#plot box plots using ggplot
library(ggplot2)
ggplot(red_wine) +
  #  aes(x = "",y = red_wine$fixed.acidity) +
  #  aes(x = "",y = red_wine$volatile.acidity) +
  #  aes(x = "",y = red_wine$citric.acid) +
  #  aes(x = "",y = red_wine$residual.sugar) +
  #  aes(x = "",y = red_wine$chlorides) +
  #  aes(x = "",y = red_wine$free.sulfur.dioxide) +
  #  aes(x = "",y = red_wine$total.sulfur.dioxide) +
  #  aes(x = "",y = red_wine$density) +
  #  aes(x = "",y = red_wine$pH) +
  #  aes(x = "",y = red_wine$sulphates) +
    aes(x = "",y = red_wine$alcohol) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


apply(red_wine,2,sd)

attach(red_wine)
#sample mean
mu <- mean(residual.sugar)
mu
#plot all observations in the data
hist(residual.sugar,col='blue',main='Histogram of Residual Sugar'
     ,xlab='Residual Sugar')
abline(v=mu,col='red',lty=1)

#######################Central Limit Theorem#####################
sample <- c()
n <- length(residual.sugar)
sample <- sample(residual.sugar,1*n)
sample_mu <- mean(residual.sugar)
sample_mu
#variability of mu from only 1 sample mean
sd.sample_mu <- sd(sample)/sqrt(length(sample)) 
format(sd.sample_mu,scientific = F)
#95% CI for sample mean using CLT
c(sample_mu-2*sd.sample_mu,sample_mu+2*sd.sample_mu)
################################################################
####################BootStrap###################################
sample.set <- c()
for(i in 1:n){
  sample.bootstrap <- sample(sample,size=1*n,replace=T)
  x <- mean(sample.bootstrap)
  sample.set[i]<-x
}
sd(sample.set)
mean(sample.set)
hist(sample.set,freq = FALSE)
lines(density(sample.set),lwd=5,col='blue')
#95% CI for sample mean using bootstrapping
quantile(sample.set,probs = c(0.025,0.975))
###############################################################

hist(density,col='blue',main='Histogram of desnsity',xlab='Density')

density_new <- density*10
density_new
hist(density_new,col='blue',main='Histogram of desnsity_new',xlab='Density_new')

install.packages("stats4")
library(stats4)
minuslog.lik <- function(mu,sigma){
  log.lik <- 0
  for(m in 1:n){
    log.lik <- log.lik + log(dlnorm(residual.sugar[m],mean=mu,sd=sigma))
  }
  return(-log.lik)
}
est.lognorm <- stats4::mle(minuslogl = minuslog.lik,start=list(
                                                  mu=log(mean(residual.sugar)),
                                               sigma=log(sd(residual.sugar))))
summary(est.lognorm)

sugar.simulate <- rlnorm(n,meanlog = 0.8502, sdlog = 0.3573)

hist(sugar.simulate,main="Residual Sugar Simulated data",col = 'blue',
     xlab='Simulated Sugar',breaks = seq(from = 0, to= 20, by= 1))
hist(residual.sugar,main="Residual Sugar",col = 'blue',
     xlab='Residual Sugar',breaks = seq(from = 0, to= 20, by= 1))


excellent <- as.numeric(quality > 6)
prob <- mean(excellent)
prob
variabiity.excellent <- sqrt(prob*(1-prob) / length(excellent))
#95% CI for sample mean using CLT
c(prob-2*variabiity.excellent,prob+2*variabiity.excellent)

p.set <- NULL
for (l in 1:n){
  p.boot <- sample(excellent,size=n,replace=T)
  boot_p <- mean(p.boot)
  p.set[l] <- boot_p
}

sd(p.set)
quantile(p.set,probs = c(0.025,0.975))


minuslog.lik <- function(p){
  log.lik <- 0
  for(m in 1:n){
    log.lik <- log.lik + log(dbinom(excellent[m],size=1,prob=p))
  }
  return(-log.lik)
}
est.prob <- stats4::mle(minuslogl = minuslog.lik,start=list(p=prob))
summary(est.prob)







