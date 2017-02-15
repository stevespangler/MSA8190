library(xlsx)

# Problem 1
#############
n <- 15
sigy <- 572
sigy2 <- 23530
sigx <- 43
sigx2 <- 157.42
sigxy <- 1697.8
# a
ybar <- sigy/n
xbar <- sigx/n
b1 <- (sigxy - ((1/n)*sigy*sigx))/(sigx2-(sigx^2/n))
b0 <- ybar-b1*xbar
# b
b0+b1*4.3
# c 
b0+b1*3.7
# d
46.1 - (b0+b1*3.7)
# e
sxx <- sigx2 - (sigx^2 / n)
sxy <- sigxy - (sigx * sigy / n)
sst <- sigy2 - n*ybar^2
sse <- sst - b1*sxy
ssr <- sst - sse
t0 <- b1/sqrt((sse/(n-2))/sxx)
qt(0.975,n-2)
pt(0.89,n-2)
# f
sig2hat <- sse/(n-2)
seb1 <- sqrt(sig2hat/sxx)
# g
seb0 <- sqrt(sig2hat*((1/n)+(xbar^2/sxx)))

# Problem 2
############
# a
b1-qt(0.975,n-2)*seb1
b1+qt(0.975,n-2)*seb1
# b
b0-qt(0.975,n-2)*seb0
b0+qt(0.975,n-2)*seb0
# c
muygivenx <- b0+b1*2.5
muygivenx-qt(0.975,n-2)*sqrt(sig2hat*((1/n)+((2.5-xbar)^2/sxx)))
muygivenx+qt(0.975,n-2)*sqrt(sig2hat*((1/n)+((2.5-xbar)^2/sxx)))
# d
muygivenx-qt(0.975,n-2)*sqrt(sig2hat*(1+(1/n)+((2.5-xbar)^2/sxx)))
muygivenx+qt(0.975,n-2)*sqrt(sig2hat*(1+(1/n)+((2.5-xbar)^2/sxx)))

# Problem 3
############
library(stats)
p3 <- read.xlsx('HW8_Data.xlsx',1)
# a
mod <- lm(y~x,p3)
mod$coefficients
summary(mod)$sigma^2
plot(p3$x,p3$y, xlab = 'Opponents Rush Yds', ylab = 'Games Won')
abline(mod)
# b
predict(mod,data.frame(x=1800))
# c
mod$coefficients[2]*(-100)
# d
1/mod$coefficients[2]
# e
match(1917,mod$model$x)
mod$fitted.values[12]
mod$residuals[12]
# f,g
summary(mod)
# h
t0 <- (mod$coefficients[2]-(-0.01))/coef(summary(mod))['x','Std. Error']
qt(0.995,28-2)

# Problem 4
############
# a
confint(object = mod, parm = 'x', level = 0.95)
# b
confint(object = mod, parm = '(Intercept)', level = 0.95)
# c
predict.lm(object = mod, newdata = data.frame(x=1800), interval = "confidence")
# d
predict.lm(object = mod, newdata = data.frame(x=1800), interval = "prediction")

# Problem 5
############
# a
summary(mod)$r.squared
# b
qqnorm(mod$residuals)
qqline(mod$residuals)
# c
plot(p3$x, residuals(mod), xlab = "Opp. Rush Yds (x)", ylab = "Residuals")
abline(h=0)
plot(mod$fitted.values, mod$residuals, xlab = "Predicted values", ylab = "Residuals")
abline(h=0)

# Problem 6 
############
xbar <- mean(p3$x)
ybar <- mean(p3$y)
predict.lm(mod, newdata = data.frame(x=xbar))

# Problem 8
############
# a - checking theoretical results with examples from previous problems
p7 <- data.frame(y=p3$y*3, x=p3$x*5)
mod7 <- lm(y~x, p7)
mod7$coefficients
mod$coefficients
mod$coefficients[2]/mod7$coefficients[2]
summary(mod7)$sigma^2/summary(mod)$sigma^2

# Problem 9
############
p9 <- read.xlsx('HW8_Data.xlsx',2)
# a
mod9 <- lm(y~x2+x7+x8, data = p9)
summary(mod9)
# b
summary(mod9)$sigma^2
# c
summary(mod9)$coefficients[,"Std. Error"]
# d
predict.lm(mod9,newdata=data.frame(x2=2000,x7=60,x8=1800))
# e,f
summary(mod9)
# g,h
mod9red <- lm(y~x2+x7, data = p9)
anova(mod9red, mod9)
# i
confint(mod9)
# j
predict.lm(mod9,newdata = data.frame(x2=2000,x7=60,x8=1800), se.fit = TRUE)
# k
predict.lm(mod9,newdata = data.frame(x2=2000,x7=60,x8=1800), interval = "confidence", level = 0.95)

