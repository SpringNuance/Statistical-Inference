# Solutions to Exercise 12

#Do this just once
#install.packages("np")

library(np)
library(KernSmooth)
data(cps71)
x <- cps71$age
y <- cps71$logwage

##########################################################################
# Problem 1a

par(mfrow=c(2,5))
for (h in seq(1,50,by=5)) {
  K <- ksmooth(x, y, kernel="normal", bandwidth=h)
  plot(x, y)
  lines(K, col="red")
}


# h=1 seems quite too wiggly
# h=100 seems to be global averaging
# Perhaps anything between 5 and 20 seems plausible

##########################################################################
# Problem 1b

nh <- 60
n <- 205
h_vals <- seq(0.5, 30.0, length=nh)
mse_vals <- rep(0, nh)
for (i in 1:nh){
  h <- h_vals[i]
  sse <- 0
  for (jtest in 1:n){
    ytestpred <- ksmooth(x[-jtest], y[-jtest], kernel="normal", bandwidth=h, x.points=c(x[jtest]))$y
    sse <- sse + (y[jtest] - ytestpred)^2 
  }
  mse_vals[i] <- sse/n
}


# Showing how bandwidth affects MSE on test set.
plot(h_vals, mse_vals)

# Select bandwidth that minimizes MSE
imin = which.min(mse_vals)
h <- h_vals[imin]
h
# Should be 5

# Look at the smoothing with this bandwidth, using all data
K <- ksmooth(x, y, kernel="normal", bandwidth=h)
plot(x, y)
lines(K, col="red")


##########################################################################
# Problem 2

xrange <- c(15,70)
plot(x, y, xlim=xrange, ylim=c(8,16))
h <- 5 / 2.7

K0 <- locpoly(x, y, degree=0, kernel="normal", bandwidth=h, range.x=xrange)
lines(K0, col="red")

K1 <- locpoly(x, y, degree=1, kernel="normal", bandwidth=h, range.x=xrange)
lines(K1, col="blue")

K2 <- locpoly(x, y, degree=2, kernel="normal", bandwidth=h, range.x=xrange)
lines(K2, col="green")

# Answer:
# (i)   Well within the range the fits seem very similar.  The 2nd degree polynomial
#       seems more wiggly than the others.
#
# (ii)  Near the ends, the 0-degree (Nadaraya-Watson) seems to be "undershooting".
#       This is an edge effect: the local averaging is seeing points on one side only.
#       Linear and quadratic fits seem to be following the tails of the data better.
#
# (iii) Outside the data (extrapolating) all three fits seem to shoot to different
#       directions.  All of them seem rather unreliable, at least when going further
#       away than the current bandwidth.  Perhaps using a bigger bandwidth would help?
#       In any case we would be assuming (without any supporting data) that the wages
#       behave the same way outside and inside the observed range.


##########################################################################
# Problem 3

plot(x, y)
h <- 5 / 2.7

for (i in 1:20){
  I = sample(1:n, n, replace=TRUE)
  K1 <- locpoly(x[I], y[I], degree=0, kernel="normal", bandwidth=h)
  lines(K1, col="blue")
}

# Answer: The 20 lines seem to be very consistent towards the left end,
# so it seems we know the average wage pretty well here.
# Towards the right end they seem to diverge, indicating that we are
# more uncertain of the average wage.


##########################################################################
# Problem 4

xrange <- c(21, 65)
h <- 500 / 2.7
B <- 1000
gridsize <- 401

# Create a B*gridsize matrix to hold the values
yboot <- matrix(0, nrow=B, ncol=gridsize)
K <- locpoly(x, y, degree=0, kernel="normal", bandwidth=h, range.x=xrange, gridsize=gridsize)

for (b in 1:B){
  I = sample(1:n, n, replace=TRUE)
  Kboot <- locpoly(x[I], y[I], degree=0, kernel="normal", bandwidth=h, range.x=xrange, gridsize=gridsize)
  yboot[b,] <- Kboot$y
}

lower <- rep(0, gridsize)
med   <- rep(0, gridsize)
upper <- rep(0, gridsize)
for (i in 1:gridsize){
  lower[i] <- quantile(yboot[,i], 0.025)
  med[i]   <- quantile(yboot[,i], 0.500)
  upper[i] <- quantile(yboot[,i], 0.975)
}
plot(x, y, xlim=xrange)
lines(K1$x, lower, col="blue")
lines(K1$x, med, col="black")
lines(K1$x, upper, col="red")

# Answer:
#
# The bands are getting wider in the right half of the data.
# There could be two causes:
#  (1) less data points there, so more uncertainty
#  (2) y values having wider variance there, so more uncertainty
#
# Also the bands are getting wider near both endpoints.
# This is an edge effect.  Near the edges there are less data
# points so the local averaging is depending on fewer points,
# and is thus more uncertain.
#
# Note that the 95% confidence band is for the estimated AVERAGE WAGE
# for each age.  It is not trying to contain 95% of the individual
# wages!  You can see this very clearly if you set h=500, which is
# so wide that the "local" averaging is in fact global averaging,
# and we are in fact trying to find a confidence interval for
# the global average wage.  Because there is so much data (n=205),
# we have a fairly precise idea of the global average.

