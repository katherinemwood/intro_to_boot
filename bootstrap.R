library(effsize)
#Set the seed for reproducable results
set.seed(123)
#Generate some random normal data
x <- rnorm(50, 1.25, .5)
mean(x)
sd(x)
#Calculate the SE the old fashioned way
se <- sd(x)/sqrt(50)
#Get 95% CIs with the formula
lb_se <- mean(x) - 1.96*se
ub_se <- mean(x) + 1.96*se
#sample histogram
hist(x,  breaks=10)
#bootstrap our sample
boot <- replicate(1000, mean(sample(x, 50, replace=TRUE)))
#Use percentile method to get the 95% confidence interval
lb <- sort(boot)[.025*1000]
ub <- sort(boot)[.975*1000]
#Plot a histogram of the bootstrapped distribution, the
#mean, and the 95% bootstrapped CI
hist(boot, breaks=50)
abline(v=c(mean(boot), lb, ub), col=rep('red', 3), lty=c(1, 2, 2))
#Generate gamma data
groups <- data.frame('exp'=rnorm(50, .5, 1), 'control'=rnorm(50))
es <- cohen.d(groups$exp, groups$control)
#bootstrap the effect size; sample each group independently because
#we assume independence between groups here
boot_es <- replicate(1000, cohen.d(sample(groups$exp, 50, replace=TRUE), 
                                   sample(groups$control, 50, replace=TRUE))$estimate)
lb_es <- sort(boot_es)[.025*1000]
ub_es <- sort(boot_es)[.975*1000]
hist(boot_es, breaks=50)
abline(v=c(mean(boot_es), lb_es, ub_es), col=rep('red', 3), lty=c(1, 2, 2))


