# Problem 1
############
setwd("C:/Users/Steven Spangler/OneDrive - Georgia State University/MSA 8190/ExampleData")
library(xlsx)
d1 <- read.xlsx('HW5_data.xlsx', sheetName = 'Problem 1')$Cotton_Percentage

# a
mean(d1)
var(d1)
median(d1)

# b
stem(d1)

# c
range(d1)
table(cut(d1, c(32:38), right = FALSE))
hist(d1, xlab='Cotton Percentage', main = 'Histogram of Cotton Percentage')

# d
boxplot(d1, main = 'Percent of Cotton in Material', xlab = 'Cotton Percentage', horizontal = TRUE)

# Problem 2
############
d2 <- read.xlsx('HW5_data.xlsx', sheetName = 'Problem 2')$visco

# a 
stem(d2)
plot.ts(d2, main = 'Viscosity Over Time', xlab='Hour', ylab='Viscosity')

# Problem 8
############
n <- 20
sigy <- 12.7
sigy2 <- 8.8
sigx <- 1487
sigx2 <- 143215
sigxy <- 1083

# a
b1 <- (sigxy - (sigy * sigx / n)) / (sigx2 - (sigx^2 / n))
b0 <- (sigy / n) - (b1 * (sigx / n))
b0
b1

line1 <- function(x){
  y <- b1*x + b0
  return(y)
}
curve(line1, from = 0, to = 500, ylab = 'y')

# b
b0 + b1*85

# c 
b0 + b1*90

# Problem 9
############
d3 <- read.xlsx('HW5_data.xlsx', sheetName = 'Problem 9')

# a
plot(d3$Age..weeks., d3$Strength..psi., xlab='Age of Propellant (weeks)', 
     ylab='Shear Strength of Bond (PSI)', pch=15)

# b
mod <- lm(d3$Strength..psi.~d3$Age..weeks.)
mod$coefficients

# estimate of sigma squared
sum(mod$residuals^2) / mod$df.residual

# c 
mod$coefficients[1] + mod$coefficients[2] * 20
