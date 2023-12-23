####
# 1:
####
# Context:
?sleep
# Data strucutre:
str(sleep)
# Data:
head(sleep)

# a)
A <- sleep[sleep$group == 1,]

# b)
boxplot(A$extra)
#
plot(A$extra,
     cex = 2, pch = 16)
abline(h = 0)
# Or sort
plot(sort(A$extra),
     cex = 2, pch = 16)
abline(h = 0)

# c)
# H0: Median difference between placebo and drug 1 is equal
# H1: Median difference between placebo and drug 1 is not equal
binom.test(sum(A$extra > 0), 10)
# What does this test?
# "Test statistic W ~ b(n, p = 0.5)"

# Sample distribution:
barplot(table(A$extra > 0) / 10, yaxt = 'n');axis(2, las = 2)
# p = 0.5 exactly!

# d)
# No statistically significant difference compared to placebo

####
# 2:
####
salary <- data.frame(women = c(42600, 43600, 49300, 42300, 46200, 45900, 47500, 41300),
                     men = c(46200, 44700, 48400, 41700, 48600, 49300, 48300, 44300))
# a)
boxplot(salary$women, salary$men, names = c('Women','Men'))
#
plot(salary$women ~ salary$men,
     cex = 2, pch = 16)
abline(a = 0, b = 1)
# b.
# The data is paired (and the pairs are not independent), making paired sign test and paired signed rank test appropriate choices.
# Note that using a two-sample rank test is not justified as it assumes the independence of the two samples.

# c)
# H0: Median difference between women and men is equal
# H1: Median difference between women and men is not equal
# Significance level alpha 0.1
# Signed rank test
wilcox.test(salary$women, salary$men, paired = T)
# These are equal tests:
wilcox.test(salary$women - salary$men, mu = 0)

# Paired sign test:
binom.test(x = sum((salary$women - salary$men) > 0),
           n = 8)

# What does this test?
# "Test statistic W ~ b(n, p = 0.5)"
barplot(table(c(0,1)) / 2, yaxt = 'n');axis(2, las = 2)
# Sample distribution:
barplot(table((salary$women - salary$men) > 0) / 8, yaxt = 'n', ylim = c(0,0.8));axis(2, las = 2)
# Note that, in both tests, the x_i - y_i = 0 are excluded.

# d)
# In signed rank test We observe p-value less than alpha, 0.054 < 0.1
# and reject the H0; therefore, we have statistical
# evidence that the salary between women and men are not equal.

# In sign test We observe p-value larger than alpha, 0.289 > 0.1
# and retain the H0; therefore, we do not have statistical
# evidence that the salary between women and men would not be equal.

# Which one we choose?

# e)
# Assumptions:
# Sign test:
# - i.i.d.

# Singned rank test:
# - i.i.d.
# - Under the H0, the Difference y_i - x_i is from symmetric distribution

# Well, our sample size is quite small, n = 8 so it is difficult to
# evaluate the distribution:
boxplot(salary$women - salary$men)
# Moreover, if you are sure that your sample A and B comes from the 
# same distribution (continuous), then the pairwise difference
# is from symmetric distribution
# Demonstration:
# Exponential distribution, not symmetric:
hist(rexp(1000), density = 10)
# Difference of two exponential distributions, symmetric:
hist(rexp(1000) - rexp(1000), density = 10)

####
# 3:
####
line <- c("F", "F", "M", "M", "F", "M", "F", "F", "M", "F", "M", "F", "M", "M", "M", "F", "M", "M", "M", "M")

# a)
# - i.i.d observations (the test requires this)
# - H0: the distributions are equal (the test defines this)
# - H1: the distributions are not equal (the test defines this)
# - Alpha = 0.05 (we choose this)

# b)
# - i.i.d yes, the students were randomly selected from Aalto
# - we note that, male height distribution might have less kurtosis compared to female hd.
# (i.e., male height is from "wider" distribution compared to female height)
# c)
female <- (1:length(line))[line == 'F']
male <- (1:length(line))[line == 'M']

# d)
wilcox.test(female, male)

# Or manually:
# Calculate U_1 and U_2, and choose the smaller one:
U1 <- sum(male) - (length(male)*(length(male)+1) / 2)
U2 <- sum(female) - (length(female)*(length(female)+1) / 2)
min(U1, U2)
# Note that, U_1 + U_2 = n_1*n_2:
U1+U2 == length(male) * length(female)
# P-value (two-sided):
2 * pwilcox(min(U1, U2), 8, 12)

# Bonus:
# See the distribution of the U statistic for n_1 = 8 and n_2 = 12:
plot(x = seq(0,100), dwilcox(seq(0,100),8,12),
     type = 'b', lwd = 1)

