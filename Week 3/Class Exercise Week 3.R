####
# 1:
####
# Useful function to look at the structure of an R object:
str(iris)

# a)
x <- (iris$Sepal.Length / iris$Sepal.Width)[1:50]

# or 

iris_temp <- iris[iris$Species == "setosa", ]
iris_ratio <- iris_temp[, 1]/iris_temp[, 2]

# b)
hist(x, density = 10, ylim = c(0,30), yaxt = 'n');axis(2, las = 2)
# or
boxplot(x, yaxt = 'n');axis(2, las = 2)

# c)
# The bootstap algorithm:

# - Generate B bootstrap samples of size n with replacement, i.e.,
# any one observation can be in the B_i bootstrap sample (BS)
# multiple times or not at all.
# (with replacement, i.e., "put the marble back into the bag each time")
# - Calculate an estimate of expected value of each B_i BS
# - Combine the sample mean with the BS the expected values
# - Rank / sort the BS estimates for the expected value
# - The BS confidence intervals can be found at:
# B * (alpha/2) and B * (1 - (alpha/2)) quantiles of
# the ranked values. For example, a 95% percentile bootstrap CI
# with 1,000 bootstrap samples is the interval between the 
# 25th quantile value and the 975th quantile value of the 
# 1,000 bootstrap parameter estimates.

# Number of bootstrap repetitions:
B <- 1000
# Size of the bootstrap sample:
n <- length(x)
# Expected value of the sample:
E <- mean(x)
# Functions sample() and replicate() are useful here
# (for loop works as well bu ugly).
E_BS <- replicate(B, mean(sample(x,n,replace = T)) )

# Find the 95% CIs:
BS_ci <- quantile(E_BS, probs = c(0.025,0.975))

# d)
hist(x, density = 10, ylim = c(0,30), yaxt = 'n');axis(2, las = 2)
abline(v = BS_ci, lwd = 3, lty = 3, col = "red")


####
# 2:
####
salary <- data.frame(women = c(42600, 43600, 49300, 42300, 46200, 45900, 47500, 41300),
                     men = c(46200, 44700, 48400, 41700, 48600, 49300, 48300, 44300))
# a)
boxplot(salary$women, salary$men, names = c('Women','Men'))

# b)

# c)
# H0: The salary between women and men is equal, mu_i == mu_j, (mu_i - mu_j == 0) i != j
# H1: The salary between women and men is not equal, mu_i != mu_j, (mu_i - mu_j != 0) i != j
# Significance level alpha 0.1
t.test(salary$women, salary$men, paired = T, conf.level = 0.90)
# These are equal tests:
t.test(salary$women - salary$men, mu = 0, conf.level = 0.90)
# What are the confidence intervals in the output?
# It is the CI for the mean difference between the salaries.
# [The formula can be found at the lecture slides ( x +- Z*(sd/sqrt(n)) )]
mean(salary$women - salary$men) + qt(0.05,7) * (sd(salary$women - salary$men) / sqrt(8))
mean(salary$women - salary$men) - qt(0.05,7) * (sd(salary$women - salary$men) / sqrt(8))

# d)
# We observe p-value less than alpha, 0.037 < 0.1
# and reject the H0; therefore, we have statistical
# evidence that the salary between women and men are not equal.

# e)
# Assumptions:
# - i.i.d.
# - normality
# Is the data normal?
# --> could be, however, the sample size is extremely small
# thus we cannot make conclusions about the distribution.
# Better to use non-parametric test! Next week.
plot(salary$women - salary$men, rep(1, 8))
abline(v = 0, lty = 3)

# or

hist(salary$women - salary$men)

####
# 3:
####
head(iris)
#  we want the 3rd column and rows 51 to 150
# - 3rd column because it contains the petal lengths
# - Rows 51 to 150 because those contain the virginica
# and versicolor species.

# Make new data frame:
D <- iris[-c(1:50),c(3,5)]

# or 

D1 <- iris[iris$Species != "setosa", c(3, 5)]
D1 <- data.frame(versicolor = D1[D1[, 2] == "versicolor", 1], virginica = D1[D1[, 2] == "virginica", 1])

# Factor level "setosa" shold be dropped so that it
# does not infere with our analysis by accident:
D <- droplevels(D)

# a)
# Boxplot:
boxplot(D$Petal.Length ~ D$Species)

####
# Formula method:
boxplot(D$Petal.Length ~ D$Species)
# Default method:
boxplot(D$Petal.Length[1:50], D$Petal.Length[51:100])
####

# Histogram:
hist(D$Petal.Length[1:50], xlim = c(3,7), density = 10)
hist(D$Petal.Length[51:100], add = T, density = 30)
# Scatter plot:
plot(D$Petal.Length[1:50], D$Petal.Length[51:100], pch = 16)

# b, d)
# Two sample t-test (Note that, this time the observations are not paired)
# H0: mu_i == mu_j, i != j
# H1: mu_i != mu_j, i != j
# Alpha = 0.05
t.test(D$Petal.Length ~ D$Species, conf.level = 0.95)
t.test(D1$versicolor, D1$virginica, conf.level = 0.95)
# p-value << alpha
# We have quite solid statistical evidence that the petal lengths
# are different between the species vir. and ver.


# c, d)
# H0: var_i == var_j, i != j
# H1: var_i != var_j, i != j
# Alpha = 0.05
var.test(D$Petal.Length ~ D$Species)
tapply(D$Petal.Length,D$Species, var)
# p-value > alpha
# We have quite solid statistical evidence that the
# variances are equal.

# e) Both tests assume:
# - i.i.d.
# - normality of the sample
# Both assumptions are more or less justified, based on visual evidence and
# that each sample contains only certain type of flower, i.e., ver. and vir.