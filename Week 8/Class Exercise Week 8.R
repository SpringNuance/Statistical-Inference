# 1
setwd("~")
# a)
D <- read.table('data_children.txt', sep = '\t', header = T)

# b)
plot(D,
     pch = 16, yaxt = 'n',
     xlim = c(18,30), 
     ylim = c(76,84));axis(2, las = 2)
title(main = 'Scatter plot height ~ age')

# c)
fit1 <- lm(height ~ age, data = D)

# d)
abline(fit1, lty = 3, col="red")

# bonus: add least-squares vertical lines,
# i.e., model residuals
arrows(x0 = D$age,
       y0 = fit1$fitted.values,
       x1 = D$age,
       y1 = D$height,
       length = 0)

# bonus: manually computing regression coefficients
x <- D[, 1]
y <- D[, 2]
rho <- cor(x, y)
sy <- sd(y)
sx <- sd(x)
mx <- mean(x)
my <- mean(y)
fitY <- my + rho * (sy/sx) * (x - mx)

# or
par(new = T)
plot(fitY, pch = 16, yaxt = 'n', col = "blue")

l2norm <- function(x) { return(sqrt(sum(x^2))) }
l2norm(fitY - D$fitted.values)


# e)
summary(fit1)
fit1$coef
#"One unit increase in the x-axis is 
# b1 in the y-axis"
# That is, "In one month, babies grow 0.63 centimeters, on average"


# Note that, in univariate linear regression
# the coefficient of determination, i.e., R squared
# is the Pearson correlation coefficient squared:
summary(fit1)$r.squared
cor(D$age, D$height)^2

# The R^2 ~ 0.99 indicates an excellent fit! Note that,
# R^2 takes values between 0 and 1!

# bonus: add step function
# make a step function:
sf <- stepfun(c(19:31),
              y = fit1$fitted.values[1] + c(0,cumsum(rep(fit1$coefficients[2],13))))

# plot:
plot.stepfun(sf, yaxt = 'n',
             xlim = c(18,30), ylim = c(76,84),
             main = '');axis(2, las = 2)

# add points:
points(D, pch = 16)
abline(fit1, lty = 3)

# 2
# a)
A <- read.table('data_tobacco.txt', sep = '\t', header = T)

# b)
plot(A$consumption,
     A$incidence,
     pch = 16, yaxt = 'n',
     xlim = c(200,1300),
     ylim = c(50,500));axis(2, las = 2)
title(main = 'Scatter plot incidence ~ consumption')

# c)
fit2 <- lm(A$incidence ~ A$consumption)

# d)
abline(fit2, lty = 3, col="red")
# the fit does not look too good

# BONUS demonstration: add least-squares vertical lines,
# i.e., model residuals
arrows(x0 = A$consumption,
       y0 = fit2$fitted.values,
       x1 = A$consumption,
       y1 = A$incidence,
       length=0)

# e)
summary(fit2)
# coefficient is about 0.23, which implies that
# for an increase of a single smoked cigarette
# in year per capita, the expected value of the incidence
# of lung cancer increases by 0.23 units.

# H0: b1 = 0
# H1: b1 != 0
# Alpha = 0.05
# p-value < alpha
# b1 is not zero
# the effect is not caused by randomness
# the finding is statistically significant

# f)
# coefficient of determination, R^2 = 0.54 =>
# model 'explains 54.9% of the
# variation in the incidence rate .

# g)
A <- A[-7,]

# bonus: is linear regression robust for outliers?
install.packages('manipulate')
library(manipulate)


# function used as expression in manipulate
# contaminates the data with an outlier
# estimates linear model and plots it.
interactive.regression <- function(x,v,u) {
  
  
  levels(x$country)[12] <- 'Outlier'
  x[12,2] <- 'Outlier'
  x[12,1] <- v
  x[12,3] <- u
  fit <- lm(x$incidence ~ x$consumption)
  
  #x_lim <- c(floor(min(x$consumption)),
  #           ceiling(max(x$consumption)))
  
  #y_lim <- c(floor(min(x$incidence)),
  #           ceiling(max(x$incidence)))
  
  plot(x$incidence ~ x$consumption,
       xlim = c(200,1400), 
       ylim = c(40,10000),
       xlab = 'Consumption', 
       ylab = 'Incidence',
       main = '',
       pch = c(rep(1,11),16))
  
  text(x[12,1],
       x[12,3], 
       x[12,2], 
       pos = 2)
  abline(fit, lty = 3)
  labs <- paste0('b_hat = ',
                 round(fit$coef[2],3),
                 '\nr^2 = ',
                 round(summary(fit)$r.squared,3))
  title(adj = 0, main = labs)
  
}

# Use sliders:
# - See how a mid-point outlier affects when incidence is increased ~ 600
# - See how a low-point outlier affects when incidence is increased ~ 200
# - See how a high-point outlier affects when incidence is increased ~ 1400
# i.e, use the "Consumption" slider first to set the consumption rate,
# then use "Incidence" slider to change the magnitude of the "Outlier"
manipulate(interactive.regression(A,v,u),
           v = slider(200,1400, initial = 200, step = 20, label = 'Consumption'),
           u = slider(40,10000, initial = 40, step = 50, label = 'Incidence'))

# Linear model is not robust against outliers!