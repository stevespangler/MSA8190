# Problem 1
###########
x <- c(1,1.5,1.5,2.5,3)
y <- c(1:5)
fxy <- c(1/4,1/8,1/4,1/4,1/8)
mux <- x*fxy
muy <- y*fxy
# k
covxy <- sum((x-mux)*(y-muy)*fxy)
covxy
# l 
varx <- sum((x-mux)^2 * fxy)
vary <- sum((y-muy)^2 * fxy)
corxy <- covxy / sqrt(varx*vary)
corxy

# Problem 2
###########
x <- c(1,1,1,2,2,2,3,3,3)
y <- c(1,2,3,1,2,3,1,2,3)
c <- 1/36
fxy <- c(2*c,3*c,4*c,3*c,4*c,5*c,4*c,5*c,6*c)
mux <- sum(x*fxy)
muy <- sum(y*fxy)
# h
varx <- sum((x-mux)^2*fxy)
varx
# i
vary <- sum((y-muy)^2*fxy)
vary
# o 
covxy <- sum((x-mux)*(y-muy)*fxy)
covxy
# p
corxy <- covxy / sqrt(varx*vary)
corxy

# Problem 3
###########  Must complete (b) before (a)
# b
x <- c(0:4)
p <- 0.993
n <- 4

fx <- c(dbinom(0,n,p),dbinom(1,n,p),dbinom(2,n,p),dbinom(3,n,p),dbinom(4,n,p))
fx

# a
y <- c(0:4)
p <- 0.997
n <- 4

fy <- c(dbinom(0,n,p),dbinom(1,n,p),dbinom(2,n,p),dbinom(3,n,p),dbinom(4,n,p))
fy

combins <- expand.grid(fx,fy)
fxy <- combins$Var1 * combins$Var2
fxy
# c
mux <- sum(x*fx)
mux
# d
fy
# e
muy <- sum(y*fy)
muy
# f
vary <- sum((y-muy)^2*fy)
vary

# Problem 4
###########
x <- c(1,1,1,1,2,2,2,2)
y <- c(1,1,2,2,1,1,2,2)
z <- c(1,2,1,2,1,2,1,2)
fxyz <- c(0.05,0.1,0.15,0.2,0.2,0.15,0.1,0.05)

# e
mux <- sum(x * fxyz)
mux
# j
muy <- sum(y * fxyz)
covxy <- sum((x-mux)*(y-muy)*fxyz)
covxy
# k
muz <- sum(z * fxyz)
covxz <- sum((x-mux)*(z-muz)*fxyz)
covxz
# l 
covyz <- sum((y-muy)*(z-muz)*fxyz)
covyz

# Problem 5
###########
# c
pbinom(2,12,0.15)

# Problem 6
############
# a
pdf1 <- function(x){integrate(function(y) (x+y), x, x+2)$value}
pdf2 <- function(x){sapply(x, pdf1)}
integrate(pdf2, 0, 3)$value
# b
pdf1 <- function(x){integrate(function(y) (1/24)*(x+y), x, 2)$value}
pdf2 <- function(x){sapply(x, pdf1)}
integrate(pdf2, 0, 1)$value
# c
pdf1 <- function(x){integrate(function(y) (1/24)*(x+y), x, x+2)$value}
pdf2 <- function(x){sapply(x, pdf1)}
integrate(pdf2, 1, 2)$value
# d
pdf1 <- function(x){integrate(function(y) (1/24)*(x+y), 0, x)$value}
pdf2 <- function(x){sapply(x, pdf1)}
1 - integrate(pdf2, 0, 1)$value
# e
pdf1 <- function(x){integrate(function(y) (1/24)*(x+y), x, 2)$value}
pdf2 <- function(x){sapply(x, pdf1)}
integrate(pdf2, 0, 2)$value
# f
pdf1 <- function(x){integrate(function(y) x*(x+y), x, x+2)$value}
pdf2 <- function(x){sapply(x, pdf1)}
integrate(pdf2, 0, 3)$value



# Problem 7
###########
# c
(1+sqrt(3))/(2*sqrt(3))

# Problem 8
###########
(2.95-3)/0.04
(7.6-7.7)/0.08

(pnorm(1.25) - pnorm(-1.25))^2


# Problem 9
###########
# a
vard <- 0.1^2+0.05^2+0.05^2
vard
stdd <- sqrt(vard)
stdd
# b
pnorm(5.9, mean = 6, sd = stdd)
