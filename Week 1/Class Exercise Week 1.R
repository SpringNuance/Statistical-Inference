# Exercise 2
####
# a)
####
x <- rnorm(n = 100)

####
# b)
####
mean(x)
sd(x)
# Why not 0 and 1?
# At larger sample size, mean and sd would be closer to 0 and 1, and converges to 0 and 1 at infinity. Why? See part e!
# However, this is a random sample, generated from the standard normal distribution.
# In other words, this is NOT the normal distribution.
# More on this in part e!

# Optional visualisation:
hist(x, freq = F, density = 10, ylim = c(0,0.5), yaxt = 'n');axis(2, las = 2)
par(new = T)
plot(seq(-4,4,length.out = 1000), dnorm(seq(-4,4,length.out = 1000)), type = 'l', axes = F, yaxt = 'n', xaxt = 'n', ylab = '', xlab = '', lwd = 2);box()
abline(v = 0, lty = 1, lwd = 2)
legend('topright', legend = c('Random sample distribution', 'the normal distribution'), pch = c(15,150))

####
# c)
####
var(x)
# See that:
sd(x)^2
# or 
myVar <- function(x) { sum((x - mean(x))^2)/(length(x) - 1) }


####
# d)
####
x_10 <- rnorm(10,1,3)
x_100 <- rnorm(100,1,3)
x_1000 <- rnorm(1000,1,3)
x_100000 <- rnorm(100000,1,3)

# Optional visualisation:
library(DescTools)
set.seed(123)
hist(x_1000, freq = F, ylim = c(0,0.2), xlim = c(-10,10), xlab = 'x', yaxt = 'n');axis(2, las = 2)
hist(x_100, freq = F, add=T)
hist(x_10, freq = F, add=T)
par(new = T)
plot(seq(-10,10,length.out = 1000), dnorm(seq(-10,10,length.out = 1000), 1, 3), type = 'l', axes = F, yaxt = 'n', xaxt = 'n', ylab = '', xlab = '', lwd = 2);box()
abline(v = 1, lty = 1, lwd = 2)
# With n = 100000 empirical and theoretical should be closer:
hist(x_100000, freq = F, add=T)

####
# e)
####
# R tricks-n-tips hat on: apply() -function.
apply(data.frame(x_10, x_100, x_1000, x_100000),2,mean)
apply(data.frame(x_10, x_100, x_1000, x_100000),2,sd)
# He is a madman! Anon. function inside apply()
apply(data.frame(x_10, x_100, x_1000, x_100000),2, function(x) cbind(mean(x), sd(x)))

# Back-with-the-math-hat!
# It seems that at larger sample sizes, the empirical distribution statistics are closer to theoretical, why?
# Because of the Law of large numbers (LLN), of course! (https://en.wikipedia.org/wiki/Law_of_large_numbers)

# Optional Monte Carlo demo (which is one "application" for LLN):
# R tricks-n-tips hat on: replicate() -function.
mean(replicate(10, mean(rnorm(100))))
mean(replicate(100, mean(rnorm(100))))
mean(replicate(1000, mean(rnorm(100))))
# w000t?
# "Take N random samples from standard normal distribution size n and calculate the expected value of expected values"
# More repetitions -> closer to theoretial values.

# Optional visualization:
df <- replicate(10000, mean(rnorm(100)))
m <- rep(NA, 10000)
for(i in 1:10000) {
  m[i] <- mean(df[1:i])
}
plot(m, pch = 16, ylim = c(-0.1,0.1), yaxt = 'n', xlab = 'N', ylab = 'Expected value', col = "blue");axis(2, las = 2)
abline(h = 0, lwd = 2, lty = 1)
legend('topright', legend = c('Observed expected value', 'Expected value'), pch = c(16,150))
title('Monte Carlo error')

# Exercise 3
####
# a)
####
heights <- rnorm(15, 170, 15)

####
# b)
####
hist(heights) #fill the parameters yourself!

####
# c)
####
hist(rnorm(20000, 170, 15), breaks=100)
par(new=T)
plot(seq(120,220,length.out = 20000), dnorm(seq(120,220,length.out = 20000), 170, 15), type = 'l', axes = F, yaxt = 'n', xaxt = 'n', ylab = '', xlab = '', col = "blue", lwd = 2);box()

# Exercise 4
####
# a)
####
eyecolors <- sample(1:3, 15, replace = TRUE)
eyecolors_2 <- factor(eyecolors, labels = c("Brown", "Green", "Blue"))

####
# b)
####
barplot(table(eyecolors))
barplot(table(eyecolors_2))

####
# c)
####
e2 <- sample(1:50, 2000, replace=TRUE)
e3 <- factor(e2, labels = c("Brown", "Green", "Blue"))
barplot(table(e3))