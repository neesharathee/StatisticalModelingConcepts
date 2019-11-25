############## t-tests: examine if the difference in means is significant or not

normdis<-rnorm(n=1000, m=30, sd=3)
hist(normdis)

data("ToothGrowth")
head(ToothGrowth)
str(ToothGrowth)

hist(ToothGrowth$len)

#qqplot
qqnorm(ToothGrowth$len)
qqline(ToothGrowth$len)
# Shapiro-Wilk normality test 
#H0: data are normally distributed
shapiro.test(ToothGrowth$len) #data are normally distributed cz p value is greater thn 0.05

#z=(x-mean)/sd
#pnorm(z) for probability of value less thn in bell curve
#1-pnorm(z) for probability of value more thn in bell curve

#confidence interval of mean
head(ToothGrowth)
summary(ToothGrowth)
s=sd(ToothGrowth$len) # standard deviation
s
SE=s/sqrt(length(ToothGrowth$len)) # standard error
SE

zval=qnorm(0.975) #z value
zval #1.96 for 95% CI

moe=zval*SE # margin of error

xbar = mean(ToothGrowth$len)
xbar+c(-moe,moe)

#t based CI (generally used when n<30)
n = length(ToothGrowth$len)

tval = qt(0.975,df=n-1) # critical t value
tval

moe = tval*SE
xbar = mean(ToothGrowth$len)
xbar
xbar+c(-moe,moe)

####or
t.test(ToothGrowth$len) #95% CI

t.test(ToothGrowth$len,conf.level = 0.9) # 90% CI

