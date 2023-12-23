####
# 1:
####
# a)
hist(rivers, density = 10, xlab = 'Length (miles)', ylim = c(0,100), yaxt = 'n');axis(2, las = 2)

# b)
# histogram with probability densities (freq = F):
hist(rivers, density = 10, xlab = 'Length (miles)', ylab = '', yaxt = 'n', freq = F);axis(2, las = 2)
# exp. dist pdf with lambda = 0.0015
lines(seq(0,4000,length.out = 1000), dexp(seq(0,4000,length.out = 1000), rate = 0.0015), lwd = 2, col = "blue")

# or
boxplot(rivers)
boxplot(rivers, outline = F)

# c)
a <- cut(rivers, breaks = c(min(rivers),250,500,750,1000,1250,max(rivers)),
         include.lowest = T, right = T, dig.lab = 4)
a
# words of warning: include.lowest is set to FALSE by default, therefore
# it would 'drop' the minimum value when the breaks are defined like this.
# dig.lab can be use to adjust the number of digits if
# some lables are given in scientific notation, for example

# d)
barplot(table(a))
# y-axis tick labels, x-axis label size, and bar colors:
barplot(table(a), yaxt = 'n', cex.names = 1.25, col = "blue");axis(2, las = 2)
title(xlab = 'River length (miles)', ylab = 'Frequency')

# or
pie(table(a))
# With sprinkles on top
pie(table(a), col = colorRampPalette(c("blue","red"))(6))
# colorRampPalette(x)(y) can be used to generate color gradients
# where x is a vector of color names or hexcode
# and y is an integer number on how many colors should be outputted
colorRampPalette(c('red','blue'))(10)




####
# 2:
####
?islands
islands

# a)
hist(islands, density = 10, xlab = 'Area (1000 x miles^2)', ylim = c(0,50), xlim = c(0,20000),
     yaxt = 'n');axis(2, las = 2)
# some empty classes / intervals can be seen; try different breaks:
hist(islands, density = 10, xlab = 'Area (1000 x miles^2)', ylim = c(0,50), xlim = c(0,20000),
     yaxt = 'n', breaks = 5);axis(2, las = 2)
# exponential distribution, most likely.

# or

dotchart(sort(islands), pch = 16, lcol = "green")
# note that, dotchart() function can be frustrating sometimes with long label names.

# or
boxplot(islands)
boxplot(log(islands))
boxplot(islands, outline = F)

# c)
# compute measures of location
mean(islands)
median(islands)
mean(islands) / median(islands)

# measures of scatter
sd(islands)
mad(islands)

# visualize:
dotchart(sort(islands), pch = 16, lcol = "blue")
abline(v = c(mean(islands), median(islands)), col="red")

# in practically same way one can visualize and compare measures of scatter

# d)
# lets try to trust R to compute outliers for us
out <- boxplot(islands, plot=F)$out
islands %in% out
# extract non outliers
isl <- islands[!islands %in% out]

# is it correct?  

# once again
out2 <- out[-order(out, decreasing = T)[8:8]]
# alternatively
t <- out[out != 840]

islands %in% out2
isl <- islands[!islands %in% out2]

# or 

sort(islands, decreasing = T)
# remove the continents:
b <- sort(islands, decreasing = T)[-c(1:7)]


# compute stats:
mean(b)
median(b)
mean(b) / median(b)
# looks like they are in the same ballpark
# visualize:
dotchart(sort(b), pch = 16, lcol = "blue")
abline(v = c(mean(b), median(b)), col="red")

####
# 3:
####
?Nile
Nile

# a)
# 
hist(Nile, density = 10, xlab = 'Water Flow volume (10^8 m^3)', ylim = c(0,30),
     yaxt = 'n');axis(2, las = 2)
# looks like the century water flow of the river Nile is normally distributed

# ts plot
plot(Nile)
# adjust a bit:
plot(Nile, type = 'b', yaxt = 'n', ylab = 'Water Flow volume (10^8 m^3)', pch = 16);axis(2, las = 2)

# b)
# - perhaps a more dry season in the middle of the follow-up time.
# - perhaps there is seasonal u-shaped variation spanning ~ 100 years

# c)
# we could achieve this by calculating these values one-by-one, but
# there are easier ways to do this:
summary(Nile)

# next, package called 'psych' has a function called
# 'describe' which would be the most simple to use here

#install.packages('psych')
library(psych)
describe(Nile)

# kurtosis can be defined in (at least) three ways and
# different packages may implement different methods.
# this has caused some mix-ups and frustration in the previous years so let's
# look into this.
#install.packages('e1071')
#install.packages('moments')
library(e1071)
library(moments)
moments::kurtosis(Nile) # This is kurtosis
e1071::kurtosis(Nile) # This is so called excess kurtosis
# excess kurtosis is defined as kurtosis - 3
moments::kurtosis(Nile) - 3
e1071::kurtosis(Nile)
# they are still different!!!

# there are also some minor differences in the way the kurtosis
# is calculated. In psych package kurtosi() function
# you can look and define different ways to calculate kurtosis:
kurtosi(Nile, type = 1) # How moments package implements:
moments::kurtosis(Nile) - 3

kurtosi(Nile, type = 2) # how someone can implement
kurtosi(Nile, type = 3) # how package e1071 implements
e1071::kurtosis(Nile)

# perhaps the way how e1071 defines the excess kurtosis is the most used.

# computing mode:
table(Nile)
# More conveniently
sort(table(Nile), decreasing = T)
# the data has four modes (the first four observations in the ranked observations)
sort(table(Nile), decreasing = T)[1:4]

# d)
plot(Nile, type = 'b', yaxt = 'n', ylab = '', pch = 16);axis(2, las = 2)
# Mean = the average level around which the data fluctuates
# SD & Var = the average size of these fluctuations
# Min = the lowest point of the curve
# Max = the lowest point of the curve
# Median = the average level around which the data fluctuates (robust)
# MAD = the average size of these fluctuations (robust)
# (Mode = difficult to see...)
# (Skewness = difficult to see...)
# (Kurtosis = difficult to see...)

# Visualize the stats:
mtext(c('Mean','Median','Sd-','Sd+','Max','Min','MAD-','MAD+'),
      at = c(mean(Nile), median(Nile), mean(Nile) - sd(Nile), mean(Nile) + sd(Nile), max(Nile), min(Nile), median(Nile) - mad(Nile), median(Nile) + mad(Nile)),
      side = 2, las = 2, col="blue")

abline(h = c(mean(Nile), median(Nile), mean(Nile) - sd(Nile), mean(Nile) + sd(Nile), max(Nile), min(Nile), median(Nile) - mad(Nile), 
             median(Nile) + mad(Nile)), lty = 3, col="red")