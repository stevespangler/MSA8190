# Problem 1
############
sigma <- 20
# a
n <- 10
xbar <- 1000
xbar-qnorm(0.975)*sigma/sqrt(n)
xbar+qnorm(0.975)*sigma/sqrt(n)
# b
n <- 25
xbar-qnorm(0.975)*sigma/sqrt(n)
xbar+qnorm(0.975)*sigma/sqrt(n)
# c
n <- 10
xbar-qnorm(0.995)*sigma/sqrt(n)
xbar+qnorm(0.995)*sigma/sqrt(n)
# d
n <- 25
xbar-qnorm(0.995)*sigma/sqrt(n)
xbar+qnorm(0.995)*sigma/sqrt(n)

# Problem 2
############
sigma <- 0.001
n <- 15
xbar <- 74.036
# a
xbar-qnorm(0.995)*sigma/sqrt(n)
xbar+qnorm(0.995)*sigma/sqrt(n)
# b
xbar-qnorm(0.95)*sigma/sqrt(n)

# Problem 3
############
n <- 15
xbar <- 2.78
sigma <- 0.9
# a
mu <- 3
(xbar-mu)/(sigma/sqrt(n))
qnorm(0.975)
# b
library(pwr)
pwr.norm.test(d=(3.25-3)/sigma,n=n,sig.level = 0.05,alternative = 'two.sided')$power
# c
pwr.norm.test(d=(3.75-3)/sigma, sig.level = 0.05, power = 0.9)$n

# Problem 4
############
# a
pnorm(0,1,sqrt(3),lower.tail = FALSE)
# b
pt(sqrt(2),1)
# d
pchisq(0.5,df=2,lower.tail = FALSE)

# Problem 5
############
library(xlsx)
p5 <- read.xlsx('HW6_data.xlsx',sheetName='Problem 5')
# a
boxplot(p5$X)
qqnorm(p5$X); qqline(p5$X)
# b
shapiro.test(p5$X)
# c
xbar <- mean(p5$X)
s <- sd(p65X)
n <- length(p65X)
xbar-qt(0.975,n-1)*s/sqrt(n)
xbar+qt(0.975,n-1)*s/sqrt(n)
# d
xbar-qt(0.95,n-1)*s/sqrt(n)

# Problem 6
############
sigma2 <- 18
n <- 10
s <- 4.8
# a
(n-1)*s^2/sigma2
qchisq(0.975,n-1)
# b
2*min(c(1-pchisq(11.52,n-1),pchisq(11.52,n-1)))
# c
((n-1)*s^2)/qchisq(0.975,n-1)
((n-1)*s^2)/qchisq(0.025,n-1)
# d
((n-1)*s^2)/qchisq(0.95,n-1)

# Problem 7
############
p7 <- (read.xlsx('HW6_data.xlsx',sheetName='Problem 7'))$Rainfall
# a
xbar <- mean(p7)
n <- length(p7)
s <- sd(p7)
(xbar-25)/(s/sqrt(n))
qt(0.99,n-1)
# b
shapiro.test(p7)
# c
pwr.t.test(d=(27-25)/s, sig.level = 0.01, type = 'one.sample', alternative = 'greater', n = 20)
# d
pwr.t.test(d=(27.5-25)/s, sig.level = 0.01, power = 0.9, type = 'one.sample', alternative = 'greater')

# Problem 8
############
p8 <- (read.xlsx('HW6_data.xlsx',sheetName='Problem 8', header = FALSE))$X1
# a
xbar <- mean(p8)
s <- sd(p8)
n <- length(p8)
(xbar-130)/(s/sqrt(n))
qt(0.975,n-1)
# b
shapiro.test(p8)
# c
pwr.t.test(d=(130.5-130)/s,n=n,type='one.sample',alternative='two.sided')
# d
pwr.t.test(d=(130.1-130)/s,power=0.75,type='one.sample',alternative='two.sided')

# Problem 9
############
phat <- 16/200
p0 <- 0.1
n <- 200
# a
(phat-p0)/sqrt(p0*(1-p0)/n)
qnorm(0.99)
# b
pnorm(-0.943, lower.tail = FALSE)

# Problem 10
############
phat <- 15/5000
p0 <- 0.002
n <- 5000
(phat-p0)/sqrt(p0*(1-p0)/n)
qnorm(0.99)

# Problem 11
############
p11 <- (read.xlsx('HW6_data.xlsx',sheetName='Problem 11'))$x
oi <- hist(p11,breaks = c(0,1,2,3,4),right = FALSE)$counts
lambda <- mean(p11)
dpois(0,lambda = lambda)
dpois(1,lambda = lambda)
dpois(2,lambda = lambda)
1-ppois(2,lambda = lambda)

ei <- c(dpois(0,lambda = lambda)*75,
        dpois(1,lambda = lambda)*75,
        dpois(2,lambda = lambda)*75,
        (1-ppois(2,lambda = lambda))*75)
sum((oi-ei)^2/ei)
pchisq(0.532,2, lower.tail = FALSE)

# Problem 12
############
p12 <- (read.xlsx('HW6_data.xlsx',sheetName='Problem 12'))$x
# a
qqnorm(p12);qqline(p12)
# b
library(nortest)
pearson.test(p12, n.classes = 8, adjust = TRUE)
# double checking this new package's results
xbar <- mean(p12)
s <- sd(p12)
oi <- hist(p12, breaks = c(min(p12)-1,xbar-1.15*s,xbar-0.675*s,xbar-0.32*s,
                            xbar,xbar+0.32*s,xbar+0.675*s,xbar+1.15*s,max(p12)+1),
                            right = FALSE)$counts
ei <- rep(1/8,8)
chisq.test(oi,p=ei)
# c
shapiro.test(p12)

# Problem 13
############
p13 <- (read.xlsx('HW6_data.xlsx',sheetName='Problem 15'))$x
# a
qqnorm(p13);qqline(p13)
pearson.test(p13,n.classes = 8, adjust = TRUE)
shapiro.test(p13)
# b
lambda <- 1/mean(p13)
oi <- hist(p13, breaks = c(0,(-1/lambda)*log(1-0.1), (-1/lambda)*log(1-0.2), (-1/lambda)*log(1-0.3),
                       (-1/lambda)*log(1-0.4), (-1/lambda)*log(1-0.5), (-1/lambda)*log(1-0.6),
                       (-1/lambda)*log(1-0.7), (-1/lambda)*log(1-0.8), (-1/lambda)*log(1-0.9),
                        max(p13)+1), right = FALSE)$counts
ei <- rep(10,10)
chisq.test(oi, p=ei/sum(ei))
pchisq(8,8,lower.tail = FALSE)
