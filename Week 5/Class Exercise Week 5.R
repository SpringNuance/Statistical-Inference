# 1
?precip
str(precip)
precip

# a)
# Histogram for distribtuion:
hist(precip, density = 10)

# Individual data points:
dotchart(sort(precip), pch = 16)
abline(v = 20)

# b)
a <- as.numeric(precip < 20)

# Distribution wet / dry:
barplot(table(a), ylim = c(0,60), names.arg = c('Wet','Dry'), yaxt = 'n');axis(2, las = 2)

# c)
set.seed(123)
binom.test(sum(a), length(a), conf.level = 0.95)
# Note that, this gives the confidence intervals using Clopper-Pearson method!

# Here is CIs using normal approximation, as in the lecture slides:
p_hat <- mean(a)
n <- length(a)
ci <- c(p_hat - 1.96 * sqrt(p_hat*(1 - p_hat))/sqrt(n),
        p_hat + 1.96 * sqrt(p_hat*(1 - p_hat))/sqrt(n))
ci

# Bonus: for normal approximation CIs you can use DescTools package BinomCI
library(DescTools)
BinomCI(sum(a), length(a), method = 'wald')
# Minor discrepancy due to differences in normal distribution quantile:
qnorm(1 - 0.05/2) != 1.96

# d)
# For every 100 random samples of US cities of size 70, in roughly 95 of them the confidence interval
# computed as in part c contains the true proportion of dry cities in the US. We hope that the single
# interval we have is one of these 95.

# 2
p_0 <- 0.098  # Proportion of vowel / total number of people living in finland (Population)

# a)
n <- 35       # Number of participants
x <- 5        # Last name begins with a vowel
p_hat <- x/n  # Estimated proportion of vowel / number of participants in this sample (Sample)

# b.
# Assumptions:
# The sample is iid from Bernoulli with parameter value p where p is the proportion of people living in
# Finland with last name starting with a vowel. 
# (that is, everyone has their last name beginning with a vowel with equal proability and independently
# of each other)
#
# Hypotheses:
# H0: p == 0.098
# H1: p != 0.098

# c)
# Exact test if n*phat <= 10 or n*(1 - phat) <= 10
binom.test(x, n, p = p_0)

# Else, assymptotic test:
prop.test(x, n, p = p_0, correct = F)

# d)
# The conclusions most likely cannot be used to draw inference on the
# proportion of people in the whole Finland as the session participants make a poor *random* sample of this population.
# At best, the participants could be considered a random sample of all Aalto students in
# particular programmes.

# 3) 
# http://www.sthda.com/english/wiki/two-proportions-z-test-in-r
# https://www.graphpad.com/guides/prism/latest/statistics/one-tail_vs__two-tail_p_values.htm

# a)
x_1 <- 537
n_1 <- 963

x_2 <- 438
n_2 <- 901

phat <- c(x_1/n_1, x_2/n_2)
barplot(phat, ylim = c(0, 1),
        yaxt = 'n');axis(2, las = 2)

# b.
# H0: p_1 = p_2
# H1: p_1 > p_2
# where p_1, p_2 are the success probabilities (probabilities to support the candidate) in the two samples
# which are assumed to be independent of each other and iid from two Bernoulli distributions.
prop.test(c(x_1, x_2), c(n_1, n_2), alternative = "greater", correct = FALSE)

# c.
# The one-sided p-value ~= 0.001 < 0.05 -> we reject the null hypothesis in favor of the alternative.
# That is, the support has decreased.

# d.
# (The assumptions are stated above in the answer to b.) To ensure that the samples are independent and iid
# and representative of the nationwide support level, the pollmaker should draw the samples perfectly randomly
# from amongst all eligible voters.
