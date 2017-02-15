# Problem 1
############

F <- function(x){
  y <- x
  pts <- length(x)
  for (i in pts) {
    ifelse(x[i] < -2, y[i] <- 0,
           ifelse(x[i] < 2, y[i] <- 0.25*x+0.5,
                  ifelse(x[i] >= 2, y[i] <- 1
                         )))
  }
  return(y)
}

# a
F(1.8)
# b
1-F(-1.4)
# c
F(-2)
# d
F(1) - F(-1)

# Problem 2
############
# a
D(expression(1-exp(-2*x)), 'x')
# b 
D(expression(0.2*x),'x')
D(expression(0.04*x + 0.64),'x')

# Problem 3
############
f <- function(x){
  y <- 1.5*x^2
  return(y)
}

F <- function(x){
  y <- x
  pts <- length(x)
  for (i in 1:pts) {
    ifelse(x[i] < -1, y[i] <- 0,
           ifelse(x[i] < 1, y[i] <- (integrate(f, lower = -1, upper = x[i])$val),
                  y[i] <- 1
                  ))
  }
  return(y)
}

# Plot of CDF
curve(F, from = -1, to = 1, n=1001)
# Mean or Expected Value of X
integrate(function(x) x*f(x), lower = -1, upper = 1)
# Variance of X
integrate(function(x) x^2*f(x), lower = -1, upper = 1)

# Problem 4
############
a <- 1.5
b <- 2.2
f <- function(x){
  y <- x
  pts <- length(x)
  for (i in 1:pts) {
    ifelse(x[i] <1.5, y[i] <- 0,
           ifelse(x[i] <= 2.2, y[i] <- (1/(b-a)),
                  y[i] <- 0
                  ))
  }
  return(y)
}
# a
# mean
(a+b)/2
# variance
(b-a)^2/12

# b
integrate(f,lower = -Inf,upper = 2)

# Problem 5
############
# a
qnorm(0.9)
# b
qnorm(0.5)
# c
qnorm(0.1, lower.tail = FALSE)
# d
qnorm(0.9, lower.tail = FALSE)
# e


# Problem 6 
############
# a
p <- 0.9
qnorm((1+p)/2)
qnorm((1-p)/2)
# b
p <- 0.99
qnorm((1+p)/2)
qnorm((1-p)/2)
# c
p <- 0.68
qnorm((1+p)/2)
qnorm((1-p)/2)
# d
p <- 0.9973
qnorm((1+p)/2)
qnorm((1-p)/2)

# Problem 7
############
# a
pnorm(12,mean=12.4,sd=0.1)
# b
pnorm(12.1,mean = 12.4,sd = 0.1) + pnorm(12.6,mean = 12.4,sd = 0.1,lower.tail = FALSE)
# c
p <- 0.99
qnorm((1+p)/2, mean = 12.4,sd = 0.1)
qnorm((1-p)/2, mean = 12.4,sd = 0.1)

# Problem 8
############
n <- 5000
p <- 1-0.999
mu <- n*p
var1 <- n*p*(1-p)
z <- (10-mu)/sqrt(var1)

pnorm(z,lower.tail = FALSE)

# Problem 9
############
# a
lam <- 1000*0.4
# b
z <- (350-lam)/sqrt(lam)
pnorm(z,lower.tail = FALSE)

# Problem 10
############
lam <- 0.0003
# a
pexp(10000,rate = lam,lower.tail = FALSE)
# b
pexp(7000,rate = lam)

# Problem 11
############
mu <- 15
lam <- 1/mu
# a
dexp(30, rate = lam)
# b
pexp(10,rate = lam)
# c
pexp(10,rate = lam) - pexp(5, rate = lam)
# d
qexp(0.9, rate = lam)

# Problem 12
############
lam <- 30/60
r <- 5
# a
mu <- r/lam
std <- sqrt(r/lam^2)
# b
pgamma(10,shape = r,rate = lam)
# c
pgamma(5,shape = r,rate = lam)

# Problem 13
############
lam <- 20
r <- 100
# a
mu <- r/lam
# b
r <- 30
mu <- r/lam
# c
lam <- 5 # calls in 15 sec interval
ppois(3, lam, lower.tail = FALSE)

# Problem 14
############
# a
mu <- 700*gamma(1+0.5)
# b
var1 <- 700^2*gamma(1+(2/2)) - 700^2*(gamma(1+(1/2)))^2
# c
pweibull(mu, 2,scale = 700, lower.tail = FALSE)

# Problem 15
############
# a
plnorm(10, meanlog = 0.5, sdlog = 1, lower.tail = FALSE)
# b
qlnorm(0.5,meanlog = 0.5, sdlog= 1)
# c
mu <- exp(0.5+(1/2))
std <- sqrt(exp(2*0.5+1)*(exp(1)-1))
