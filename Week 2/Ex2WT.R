# Graphics preset:
pcol <- c("#FFFFFFD9", '#1f2828', '#e85836', '#6c8d8d', '#ccb100')
op.web <- par(family = 'serif', bg = '#ffcd00',
              cex.axis = 1.25, cex.lab = 1.5, cex.main = 1.5,
              col.axis = pcol[2], col.lab = pcol[2], col.main = pcol[2], col.sub = pcol[2],
              font.lab = 3, font.axis = 3, fg = pcol[2], tck = 0.015)

####
# 1:
####
# a)
rivers
hist(rivers, density = 10, xlab = 'Length (miles)', ylim = c(0,100))
boxplot(rivers)
# Pro tip: axis tick labels horizontally
hist(rivers, density = 10, xlab = 'Length (miles)', ylim = c(0,100),
     yaxt = 'n')
axis(2, las = 2)

# b)
# This can be explained, or a exponential distribution PDF can be shown, or
# Show it on R:
# Extra demonstration (code does not need to be explained deeply):
# Drawing some exponential distribution probability density function
# Histogram with probability densities (freq = F):
hist(rivers, density = 10, xlab = 'Length (miles)', ylab = '',
     yaxt = 'n', freq = F);axis(2, las = 2)
# Exponential distribution PDF with rate parameter lambda = 0.0015
lines(seq(0,4000,length.out = 1000),
     dexp(seq(0,4000,length.out = 1000), rate = 0.0015), lwd = 2, col = pcol[1])

# c)
a <- cut(rivers,
         breaks = c(min(rivers),250,500,750,1000,1250,max(rivers)),
         include.lowest = TRUE, right = TRUE, dig.lab = 4)
a
# Words of warning: include.lowest is set to FALSE by default, therefore
# it would 'drop' the minimum value when the breaks are defined like this.
# Moreover, dig.lab can be use to adjust the number of digits if
# some lables are given in scientific notation, for example

# d)
barplot(table(a), xlab = 'Hello world', names.arg = c('min','mid','med','semilarge','large','extra large'))
# We can leave the figure like that, or with some inishing touches.
# Y-axis tick labels, x-axis label size, and bar colors:
barplot(table(a), yaxt = 'n',
        cex.names = 0.75, col = pcol[2]);axis(2, las = 2)
title(xlab = 'River length (miles)', ylab = 'Frequency')
# OR
pie(table(a))
# With sprinkles on top
pie(table(a), col = colorRampPalette(c(pcol[2],pcol[1]))(6))
# colorRampPalette(x)(y) can be used to generate color gradients
# where x is a vector of color names or hexcode
# and y is an integer number on how many colors should be outputted
colorRampPalette(c('red','blue'))(10)

# e)
# - Histogram begins from 0 which is below the minimum, i.e., empty class
# - Barplot and piechart classes begins from the actual miminum value thus is more 'defined'
# - Barplot is here, perhaps, the most informative: 1. does not show empty classes,
# 2. the distribution (theoretical) can still be guessed (in piechart not too well)
# 3. displays the 'mode' well.

####
# 2:
####
# Find out the source of the data:
?islands
# Look at the data:
islands

# a)
hist(islands, density = 10,
     xlab = 'Area (1000 x miles^2)',
     ylim = c(0,50), xlim = c(0,20000),
     yaxt = 'n');axis(2, las = 2)
# certainly there are more small islands than large islands.
# Moreover, some empty classes / intervals can be seen; try different breaks:
hist(islands, density = 10,
     xlab = 'Area (1000 x miles^2)',
     ylim = c(0,50), xlim = c(0,20000),
     yaxt = 'n', breaks = 5);axis(2, las = 2)
# Nevertheless, exponential distribution, most likely.

# Since we have so few data points (observations in terms of statistics)
# this can be visualized point-by-point:
plot(islands)
# We can do better that this!
dotchart(sort(islands), pch = 16, lcol = 1)
# Note that, dotchart() function can be frustrating sometimes with long label names.

# b)
# From the dotchart, we can immediately see that the seven largest are the continents
# whereas the rest are, well, large islands.
# If we would have more information on, for example, the latitudes / longnitudes
# we could further analyze how the island landmass is distributed by the lat/long.

# c)
mean(islands)
median(islands)
mean(islands) / median(islands)
# Hold up, 30x difference! Shouldn't these measures be more or less the same or at least much closer?
# Well, since our data is NOT from a symmetric distribution, therefore 
# Mean and median are NOT the same, and the more the data is 'skewed' the more
# they deviate, typically.
sd(islands)
mad(islands)
# Visualize:
dotchart(sort(islands), pch = 16, lcol = 1)
abline(v = c(mean(islands), median(islands)))
# Essentially, mean is NOT a robust location estimator whereas median is.
# This means, that it takes only one observation taken into the infinity
# and the mean explodes, whereas median is not.

# d)
# Perhaps the justification we can come up with is to remove the continents
# from the data. How to remove data points in practice in R? There are many ways
# and here is one:
# - To find the continents, sort from largest to smallest
sort(islands, decreasing = T)
# - Remove the continents:
b <- sort(islands, decreasing = T)[-c(1:7)]
# Make the calculations:
mean(b)
median(b)
mean(b) / median(b)
# Now these estimates are at least in the same ballpark!
# And visualize:
dotchart(sort(b), pch = 16, lcol = 1)
abline(v = c(mean(b), median(b)))
# Note that, by delting 'outliers' by visual 'evidence' can quite easily
# lead into you delting all the points, except those you like!
# Therefore, you should have an excellent justification to remove
# outliers! Typically, the justifications should stem from the context
# not from the numbers!

# There are of course numerical ways to determine outliers, one is Tukey's
# boxplot:
boxplot(islands)
# The outliers (points outside the box) are defined:
# - top outlier > 3rd quartile + 1.5 * IQR (points above the top whisker)
# - bot outlier < 1st quartile - 1.5 * IQR (not in this data)
# - Interquartile range (IQR) is Q3 - Q1
# See
IQR(islands)
quantile(islands)
# Q3 - Q1 = IQR:
diff(quantile(islands)[c(2,4)])
# To use this to remove the ourliers for the islands data:
b2 <- islands[islands <= quantile(islands)[4] + 1.5 * IQR(islands)]
# And the stats and vizs:
mean(b2)
median(b2)
mean(b2) / median(b2)
dotchart(sort(b2), pch = 16, lcol = pcol[2])
abline(v = c(mean(b2), median(b2)))

####
# 3:
####
# Look at the data source:
?Nile
# Aah, time-series!
Nile

# a)
# Histogram:
hist(Nile, density = 10,
     xlab = 'Water Flow volume (10^8 m^3)', ylim = c(0,30),
     yaxt = 'n');axis(2, las = 2)
# Looks like the century water flow of the river Nile is normally distributed

# Line plot
plot(Nile)
# Some adjustments:
plot(Nile, type = 'b',
     yaxt = 'n', ylab = 'Water Flow volume (10^8 m^3)',
     pch = 16);axis(2, las = 2)

# b)
# - Perhaps a more dry season in the middle of the follow-up time.
# - Perhaps there is seasonal u-shaped variation spanning ~ 100 years
# - There is also a clear change-point in ~ 1898

# c)
# We could achieve this by calculating these values one-by-one, BUT
# there are easier ways to do this:
summary(Nile)
# The base package summary() function is quite useful
# and can understand (at least tries to) multiple
# input types, i.e., where the object is from
# (linear regression model, time-series, etc.)
# A package called 'psych' has a function called
# 'describe' which would be the most simple to use here

#install.packages('psych')
library(psych)
describe(Nile)
var(Nile)
sort(table(Nile), decreasing = T)
# Kurtosis can be defined in (at least) three ways and
# different packages may implement different methods.
# This has caused some mix-ups and frustration in the previous years so let's
# look into this.
#install.packages('e1071')
#install.packages('moments')
library(e1071)
library(moments)
moments::kurtosis(Nile) # This is kurtosis
e1071::kurtosis(Nile) # This is so called excess kurtosis
# Excess kurtosis is defined as kurtosis - 3
moments::kurtosis(Nile) - 3
e1071::kurtosis(Nile)
# w00000t but it isn't!!

# There are also some minor differences in the way the kurtosis
# is calculated. In psych package kurtosi() function
# you can look and define different ways to calculate kurtosis:
kurtosi(Nile, type = 1) # How moments package implements:
moments::kurtosis(Nile) - 3

kurtosi(Nile, type = 2) # How someone can implement
kurtosi(Nile, type = 3) # how package e1071 implements
e1071::kurtosis(Nile)

# Perhaps the way how e1071 defines the excess kurtosis is the most used.

# Finally, mode, i.e., the most frequent single (or multiple if ties) observation:
table(Nile)
# More conveniently
sort(table(Nile), decreasing = T)
# The data has four modes (the first four observations in the ranked observations)

# d)
# Discuss and visualize:
plot(Nile, type = 'b', yaxt = 'n',
     ylab = '', pch = 16);axis(2, las = 2)
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
      at = c(mean(Nile), median(Nile), mean(Nile) - sd(Nile),
             mean(Nile) + sd(Nile), max(Nile), min(Nile),
             median(Nile) - mad(Nile), median(Nile) + mad(Nile)),
      side = 2, las = 2)

abline(h = c(mean(Nile), median(Nile), mean(Nile) - sd(Nile),
             mean(Nile) + sd(Nile), max(Nile), min(Nile),
             median(Nile) - mad(Nile), median(Nile) + mad(Nile)), lty = 3)



