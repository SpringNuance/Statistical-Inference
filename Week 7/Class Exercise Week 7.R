# Set your working directory to the folder you downloaded the data:
setwd("C:/Users/ACNXGD3ED010/Desktop/AALTO/Statistical Inference/teaching/Week7")

list.files()
#  1
# a)
D <- read.table('data_dependency.txt', header = T, sep = '\t')
D
# b)
pairs(D, pch = 16, lower.panel = NULL)
# Too much information...should plot one-by-one:
par(mfrow = c(2, 4))
plot(D$x1, D$y1)
plot(D$x2, D$y2)
plot(D$x3, D$y3)
plot(D$x4, D$y4)
plot(D$x5, D$y5)
plot(D$x6, D$y6)
plot(D$x7, D$y7)

demo.plot <- function(x) {
  tmp.x <- as.list(D[,c(T,F)])
  tmp.y <- as.list(D[,c(F,T)])
  tmp.x.y.l <- Map(cbind,tmp.x,tmp.y)
  
  tmp.fun <- function(x) {
    plot(x, pch = 16)
    cor.dum.p <- cor(x[,1],  x[,2], method = 'p')
    cor.dum.s <- cor(x[,1],  x[,2], method = 's')
    title(adj = 0, paste0('Pearson correlation : ',round(cor.dum.p,4),'\n',
                          'Spearman correlation: ',round(cor.dum.s,4)))
    lm.dum <- lm(x[,2] ~  x[,1])
    
    abline(lm.dum, lty = 3)
    readline('Press ENTER for next plot')
  }
  
  lapply(tmp.x.y.l, tmp.fun)
  
}
demo.plot(D)

# c)
# Pearson:
# Basic solution, one-by-one:
my_correlation <- function(i){
  pearson <- cor(D[, (2*i - 1)], D[, 2*i])
  spearman <- cor(D[, (2*i - 1)], D[, 2*i], method = "spearman")
  print(paste0("x", i, ";y", i, ", Pearson: ", round(pearson, 3), ", Spearman: ", round(spearman, 3)))
}

for(i in 1:7){
  my_correlation(i)
}

# Advanced solution, correlation matrix:
cor(D, method = 'p') 

# More advanced solution, correlation matrix and indexing:
diag(cor(D, method = 'p')[c(T,F),c(F,T)])

# In the model solutions, there is one more different
# way of doing this!

# Spearman:
cor(D, method = 's')
# Again, too much information:
diag(cor(D, method = 's')[c(T,F),c(F,T)])

# d)
# Pearson correlation "equation"...
cov(D$x1, D$y1) / ( sd(D$x1) * sd(D$y1))
# ...so essentially a normalized covariance
# Therefore, as the variance increases, the correlation decreases

# Increasing linear/monotone relationship which gets more and more
# difficult to see because of the increasing y-variance. That is, 
# increasing the y-variance hides the linear relationship under 
# the added "noise", decreasing the correlations. 
# This is the same phenomenon as in homework problem 1b.

#  2
# a)
A <- read.table('data_tobacco.txt', header = T, sep = '\t')

# b)
# Scatter plot:
plot(A$consumption, A$incidence,
     pch = 16, ylim = c(0,500), xlim = c(200,1400),
     yaxt = 'n');axis(2, las = 2)

text(A$consumption, A$incidence, A$country, pos = 4)

# c)
# Both, Pearson and Spearman correlation coefficients, positive.

# d)
cor(A$consumption, A$incidence, method = 'p')
cor(A$consumption, A$incidence, method = 's')

# Note that, Spearman cor.coef is Pearson cor.coef
# for ranked observations, i.e, measures monotonic relationship:
cor(rank(A$consumption), rank(A$incidence))

# e)
# Number of premutation replications:
B <- 2000
pcor <- replicate(B, cor(A$consumption, sample(A$incidence,nrow(A),F), method = "pearson"))
hist(pcor, freq = F, density = 10)
mean(abs(pcor) >= cor(A$consumption, A$incidence, method = "pearson"))

scor <- replicate(B, cor(A$consumption, sample(A$incidence,nrow(A),F), method = "spearman"))
hist(scor, freq = F, density = 10)
mean(abs(scor) >= cor(A$consumption, A$incidence, method = "spearman"))

# Both smaller than 0.05 -> both correlations differ significantly from 0

# e)
# Remove USA from the data and do the analysis again:
A <- A[-7,]

# Running the previous code to remove USA and then redoing the steps yields
# Pearson correlation: 0.941, p-value: 0
# Spearman correlaion: 0.927, p-value: 0.001
# The correlations are higher and more significant without the "outlier" which masked the "true" relationship.

# Note: in practice, the labeling of USA as an outlier and the consecutive removal of it should be somehow 
# justified.