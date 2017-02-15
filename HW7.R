library(xlsx)
library(pwr)

# Problem 1
############
p1 <- read.xlsx('HW7_data.xlsx',sheetName='Problem 1')
# a
xbar1 <- mean(p1$Machine1)
xbar2 <- mean(p1$Machine2)
sigma1 <- 0.02
sigma2 <- 0.025
n1 <- length(p1$Machine1)
n2 <- length(p1$Machine2)

z0 <- (xbar1-xbar2)/sqrt((sigma1^2/n1)+(sigma2^2/n2))
qnorm(0.975)
# b
2 * pnorm(z0, lower.tail = FALSE)
# c
pwr.norm.test(d=(0.04/sqrt(sigma1^2+sigma2^2)),n=10, sig.level = 0.05, alternative = 'two.sided')
# d
xbar1-xbar2-qnorm(0.975)*sqrt((sigma1^2/n1)+(sigma2^2/n2))
xbar1-xbar2+qnorm(0.975)*sqrt((sigma1^2/n1)+(sigma2^2/n2))
# e
((qnorm(0.975)+qnorm(0.95))^2*(sigma1^2+sigma2^2))/0.04^2
       # or use pwr.norm.test
pwr.norm.test(d=(0.04/sqrt(sigma1^2+sigma2^2)), sig.level = 0.05, alternative = 'two.sided',power = 0.95)
       # results agree!

# Problem 2
###########
xbar1 <- 30.87
xbar2 <- 30.68
n1 <- 12
n2 <- 10
sigma1 <- 0.1
sigma2 <- 0.15
# a
xbar1-xbar2-qnorm(0.95)*sqrt((sigma1^2/n1)+(sigma2^2/n2))
xbar1-xbar2+qnorm(0.95)*sqrt((sigma1^2/n1)+(sigma2^2/n2))
# b
xbar1-xbar2-qnorm(0.975)*sqrt((sigma1^2/n1)+(sigma2^2/n2))
xbar1-xbar2+qnorm(0.975)*sqrt((sigma1^2/n1)+(sigma2^2/n2))
# c
xbar1-xbar2+qnorm(0.95)*sqrt((sigma1^2/n1)+(sigma2^2/n2))
# d
z0 <- (xbar1-xbar2)/sqrt((sigma1^2/n1)+(sigma2^2/n2))
z0
# e
2 * pnorm(z0, lower.tail = FALSE)
# f
pwr.norm.test(d=(0.2/sqrt(sigma1^2+sigma2^2)), sig.level = 0.05, alternative = 'two.sided',
              power = 0.9)

# Problem 3
############
p3 <- read.xlsx('HW7_data.xlsx',sheetName='Problem 3')
xbar1 <- mean(p3$X1)
xbar2 <- mean(p3$X2, na.rm = TRUE)
sigma1 <- 20
sigma2 <- sigma1
n1 <- 15
n2 <- 8
delta <- 10
# a
xbar1-xbar2+qnorm(0.9)*sqrt((sigma1^2/n1)+(sigma2^2/n2))
# b
z0 <- (xbar1-xbar2-delta)/sqrt((sigma1^2/n1)+(sigma2^2/n2))
qnorm(0.9)
# c
pnorm(z0)

# Problem 4
############
xbar1 <- 86
xbar2 <- 89
n1 <- 12
n2 <- 15
s1 <- 3
s2 <- 2
# a
sp2 <- (((n1-1)*s1^2)+((n2-1)*s2^2))/(n1+n2-2)
t0 <- (xbar1-xbar2)/(sqrt(sp2)*sqrt((1/n1)+(1/n2)))
-qt(0.99,25)
# b
xbar1-xbar2-qt(0.975,25)*sqrt(sp2)*sqrt((1/n1)+(1/n2))
xbar1-xbar2+qt(0.975,25)*sqrt(sp2)*sqrt((1/n1)+(1/n2))

# Problem 5
############
p5 <- read.xlsx('HW7_data.xlsx',sheetName='Problem 5')
# a
boxplot(p5$Type1,p5$Type2, names = c('Type 1','Type 2'), horizontal = TRUE, xlab='Deflection Temperature')
qqnorm(p5$Type1, main = 'Type 1');qqline(p5$Type1)
qqnorm(p5$Type2, main = 'Type 2');qqline(p5$Type2)
# b
s12 <- var(p5$Type1)
s22 <- var(p5$Type2)
s12/s22
f0 <- var.test(p5$Type1,p5$Type2)
var.test(p5$Type1,p5$Type2)
# c
shapiro.test(p5$Type1)
shapiro.test(p5$Type2)
xbar1 <- mean(p5$Type1)
xbar2 <- mean(p5$Type2)
n1 <- length(p5$Type1)
n2 <- length(p5$Type2)

sp2 <- (((n1-1)*s12)+((n2-1)*s22))/(n1+n2-2)
t0 <- (xbar1-xbar2)/(sqrt(sp2)*sqrt((1/n1)+(1/n2)))
-qt(0.95,28)
# d
pt(1.19, 28)
# e
pwr.t.test(d=0.25,sig.level = 0.05,power = 0.9,type = 'two.sample',alternative = 'less')


# Problem 6
############
xbar1 <- 290
xbar2 <- 321
s1 <- 12
s2 <- 22
n1 <- 10
n2 <- 16
nu <- ((s1^2/n1)+(s2^2/n2))^2/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))
# a
t0 <- (xbar1-xbar2)/sqrt((s1^2/n1)+(s2^2/n2))
-qt(0.95,nu)
# b
pt(t0,nu)
# c
t0 <- (xbar1-xbar2+25)/sqrt((s1^2/n1)+(s2^2/n2))
# d
f0 <- s1^2/s2^2
qf(0.975,9,15)
qf(0.025,9,15)
qf(0.05,9,15)

# Problem 7
###########
p7 <- read.xlsx('HW7_data.xlsx',sheetName='Problem 7')
xbar1 <- mean(p7$Club1)
xbar2 <- mean(p7$Club2)
s12 <- var(p7$Club1)
s22 <- var(p7$Club2)
n1 <- length(p7$Club1)
n2 <- length(p7$Club2)
# a
shapiro.test(p7$Club1)
shapiro.test(p7$Club2)
var.test(p7$Club1,p7$Club2)
# b
sp2 <- (((n1-1)*s12)+((n2-1)*s22))/(n1+n2-2)
t0 <- (xbar1-xbar2)/(sqrt(sp2)*sqrt((1/n1)+(1/n2)))
qt(0.975,22)
# c
t.test(p7$Club1,p7$Club2, var.equal = TRUE)
# d
pwr.t2n.test(n1=12,n2=12,d=0.2/(2*sqrt(sp2)),alternative='two.sided')
# e
pwr.t.test(d=0.1/(2*sqrt(sp2)),sig.level = 0.05,power = 0.8,type = 'two.sample')
# f
xbar1-xbar2-qt(0.975,22)*sqrt(sp2)*sqrt((1/n1)+(1/n2))
xbar1-xbar2+qt(0.975,22)*sqrt(sp2)*sqrt((1/n1)+(1/n2))

# Problem 8
############
p8 <- read.xlsx('HW7_data.xlsx',sheetName='Problem 8')
dj <- p8$Lang1-p8$Lang2
dbar <- mean(dj)
sd1 <- sqrt(var(dj))
n <- length(dj)
# a
dbar-qt(0.975,11)*sd1/sqrt(n)
dbar+qt(0.975,11)*sd1/sqrt(n)
# b
qqnorm(dj);qqline(dj)
shapiro.test(dj)

# Problem 9
############
p9 <- read.xlsx('HW7_data.xlsx',sheetName='Problem 9')

t.test(p9$Before,p9$After, paired = TRUE, alternative = 'greater')

# Problem 10
############
sm <- 0.98
sw <- 1.02
nm <- 25
nw <- 21
f0 <- sm^2/sw^2
qf(0.99,nm-1,nw-1)
qf(0.01,nm-1,nw-1)

# Problem 11
#############
p1 <- (300-253)/300
p2 <- (300-196)/300
p <- (600-253-196)/600
n1 <- 300
n2 <- 300
z0 <- (p1-p2)/sqrt(p*(1-p)*((1/n1)+(1/n2)))
qnorm(0.995)

p1-p2-qnorm(0.995)*sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
p1-p2+qnorm(0.995)*sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))

# Problem 12
#############
p12 <- read.xlsx('HW7_data.xlsx',sheetName='Problem 12')
# a
qqnorm(p12$Vendor1);qqline(p12$Vendor1)
qqnorm(p12$Vendor2):qqline(p12$Vendor2)
# b
var.test(p12$Vendor1,p12$Vendor2)