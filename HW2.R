## MSA 8190 
## Assignment 2
## Steven Spangler
##
##
# Problem 1
# a
p <- 0.3 # lines are occupied
n <- 10
dbinom(3,n,p)
# b
pbinom(9,n,p)
# c
n*p

# Problem 2
# a
n <- 24
p <- 0.01
exp <- n*p
var1 <- n*p*(1-p)
sd <- sqrt(var1)
exp + 3*sd
pbinom(1,n,p,lower.tail = FALSE)
# b
p <- 0.04
sum(dbinom(2:24,n,p))
pbinom(1,n,p,lower.tail = FALSE)
# c
newp <- pbinom(1,n,p,lower.tail = FALSE)
n <- 5
pbinom(1,n,newp,lower.tail = FALSE)

# Problem 3
# a
p <- 0.2
exp <- 1/p


# Problem 4
# a
p <- 0.1
r <- 2
pnbinom(3,r,p,lower.tail=FALSE)
# b
r/p

# Problem 5
# a
p <- 0.005
dbinom(8,8,p)
# b
1/p
# c
newp <- dbinom(8,8,p)
1/newp

# Problem 6
# a
p <- 0.001
r <- 3
r/p
# b
sqrt((r*(1-p))/p^2)

# Problem 7
# a
k <- 10
m <- 240
n <- 560
dhyper(1,m,n,k)
# b
phyper(1,m,n,k,lower.tail = FALSE)

# Problem 8
# a
k <- 10
m <- 5
n <- 70
dhyper(0,m,n,k)
# b
phyper(0,m,n,k,lower.tail = FALSE)
# c
dhyper(1,m,n,k)
# d
p <- m/(m+n)
k*p

# Problem 9
# a
lam <- 10
dpois(5,lam)
# b
ppois(3,lam)
# c
lam <- 2*10
dpois(15,lam)
# d
lam <- 10/2
dpois(5,lam)

# Problem 10
# a
lam <- 10*0.05
p <- dpois(0,lam)
p
# b
n <- 10
dbinom(0,n,p)
# c
pbinom(1,n,p)

# Problem 11
lam <- 0.01 * 100
ppois(3,lam)

# Problem 12
# b
n <- 50
p <- 1/5
n*p
# c
n*p*(1-p)
