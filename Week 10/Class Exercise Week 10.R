#install.packages('carData')
library(carData)

# 1
head(Chirot)    
str(Chirot)     
summary(Chirot) 

# a)

pairs(Chirot)


boxplot(Chirot)

# Correlation analysis:
# install.packages('gplots')
library(gplots)
par(pty = 's')    # Forces square plot
b <- heatmap.2(cor(Chirot), # Correlation matrix
               Rowv = NA, dendrogram = 'none', # No reordering of features
               col = colorRampPalette(c('#1f2828','white','#e85836'))(50), # Color scale
               symm = T, # Symmetric input
               trace = 'none', # No trace lines
               cellnote = round(cor(Chirot),2), notecol = 'black', # cell lables
               key = F, # Legend key off
               margins = c(8,8)) # Control row and col label margins

# only the upper triangle:
tmp <- cor(Chirot)
tmp[upper.tri(tmp)] <- NA

b <- heatmap.2(tmp, # Correlation matrix
               Rowv = NA, dendrogram = 'none', # No reordering of features
               col = colorRampPalette(c('#1f2828','white','#e85836'))(50), # Color scale
               symm = T, # Symmetric input
               trace = 'none', # No trace lines
               cellnote = round(tmp,2), notecol = 'black', # cell lables
               key = F, # Legend key off
               margins = c(8,8)) # Control row and col label margins

# b)
fit1 <- lm(intensity ~., data = Chirot)
summary(fit1)
# Note that, this is same as:
lm(intensity ~ midpeasant + tradition + commerce + inequality, data = Chirot)

# c)
# Variance inflation factor:
#install.packages('car')
library(car)
vif(fit1)

# no significant multicollinearity; no need
# to drop any features

par(mfrow = c(2,2))
plot(fit1)
# 1: residuals are equally distributed around the fit
# 2: residuals are normally distributed, remember E[e] = 0 in lin.reg.
# 3: no heteroschedasticity, i.e., not trend in residuals
# 4: no significant outliers, i.e., no points that would
# significantly affect the linear model

# simulate heteroscedastic data, and lin. model:
x <- rep(1:100,2)
sigma2 <- x^1.5
y <- x + rnorm(x,mean=0,sd=sqrt(sigma2))
par(mfrow = c(1,1))
plot(x,y,pch=16)

hf <- lm(y~x)
abline(hf, lty = 3)
arrows(x0=x, x1=x,
       y0=hf$fitted.values,
       y=y,
       length = 0)
par(mfrow=c(2,2))
plot(hf)
par(mfrow=c(1,1))

# d)

# e)
summary(fit1)


### 2
?longley
longley

# a)
# univariate distributions, excluding time:
par(mfrow = c(2,3))
apply(longley[,-6], 2, hist)

# Boxplots:
par(mfrow = c(1,1))
boxplot(longley[,-6])
# pairwise scatterplot:
pairs(longley, upper.panel = NULL, pch = 16)

plot(ts(longley))

# correlation
b2 <- heatmap.2(cor(longley), # Correlation matrix
                Rowv = NA, dendrogram = 'none', # No reordering of features
                col = colorRampPalette(c('#1f2828','white','#e85836'))(50), # Color scale
                symm = T, # Symmetric input
                trace = 'none', # No trace lines
                cellnote = round(cor(longley),2), notecol = 'black', # cell lables
                key = F, # Legend key off
                margins = c(8,8)) # Control row and col label margins

# correlation analysis suggest high multicollinearity
# b)
fit3 <- lm(Employed ~., data = longley)
summary(fit3)
# Note the extremely high R^2

# c)
par(mfrow = c(2,2))
plot(fit3)

# Variance Inflation Factors:
vif(fit3)

# Let's drop featrues one-by-one starting from the one with highest VIF
# -GNP
fit4 <- lm(Employed ~ GNP.deflator + Unemployed + Armed.Forces + Population + Year, data = longley)
vif(fit4)
# Then -year
fit5 <- lm(Employed ~ GNP.deflator + Unemployed + Armed.Forces + Population, data = longley)
vif(fit5)
# Then -GNP.deflator:
fit6 <- lm(Employed ~ Unemployed + Armed.Forces + Population, data = longley)
vif(fit6)
summary(fit6)
# We are good with this model by 'rule of thumb': VIF < 10 is 'good'
# Let's look at the diagnostic plots once more:
par(mfrow = c(2,2))
plot(fit6)

# Oh, now we have heteroscedastisity; investigate:
par(mfrow = c(1,3))
plot(Employed ~ Unemployed, data = longley, pch = 16);abline(lm(Employed ~ Unemployed, data = longley), lty = 3)
plot(Employed ~ Armed.Forces, data = longley, pch = 16);abline(lm(Employed ~ Armed.Forces, data = longley), lty = 3)
plot(Employed ~ Population, data = longley, pch = 16);abline(lm(Employed ~ Population, data = longley), lty = 3)
# Well, Unemployed and Armed.forces might actually be
# heteroscedastic.
# Try dropping that:
fit7 <- lm(Employed ~  Population + Armed.Forces, data = longley)
vif(fit7)
par(mfrow = c(2,2))
plot(fit7)

# It is likely that the best linear model we can come up with here
# is one with only population as a predictor:
fit8 <- lm(Employed ~ Population, data = longley)
plot(fit8)
summary(fit8)

# - Still an excellent R^2
# - Are some of the assumptions violated, likely
# - Does the result make sense?
# "one unit increase in the population, increases employment by 0.48"

