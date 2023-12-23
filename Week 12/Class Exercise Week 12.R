#http://reliawiki.org/index.php/Multiple_Linear_Regression_Analysis

#1. 

#a. 
df <- data.frame(sw = iris[, 2], species = iris[, 5])
boxplot(sw ~ species, data=df, yaxt='n', ylab = "sepal width");axis(2, las=2)

# or
#install.packages("gglot2")
library(ggplot2)
ggplot(df, aes(x = sw, fill = species)) + 
geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE) +
coord_flip()

# using histogram
ggplot(df, aes(x = sw, fill = species)) +
  geom_histogram(bins = 10, col = "black") +
  facet_wrap(. ~ species) +
  labs(title = "Species histograms",
     x = NULL,
     y = "N. of species")
  
#b. ANOVA
  
iris_anova <- aov(sw ~ species, data=df)
summary(iris_anova)

bartlett.test(sw ~ species, data=df)

#c. ? 

#d. 
pairwise.t.test(df$sw, df$species, p.adjust.method = "bonferroni")

#2. 

?mtcars
str(mtcars)
head(mtcars)

#a.
df_mt <- data.frame(mpg = mtcars$mpg, hp = mtcars$hp, am = mtcars$am)

plot(mpg ~ hp, data = df_mt[df_mt$am == "0", ])
plot(mpg ~ hp, data = df_mt[df_mt$am == "1", ])

ggplot(df_mt, aes(x = hp, y = mpg)) +
  geom_point() +
  facet_wrap(. ~ am)

# b.
# complete linear model: X_ij = b0 + b1*hp + b2*am + b3*hp*am
# am == 0 => X_ij = b0 + b1*hp
# am == 1 => X_ij = b0 + b1*hp + b2 + b3*hp = (b0 + b2) + (b1 + b3) * hp
# after interpretation: => X_ij = b0 + b1*hp + b2 + b3*hp = b0 + b1 * hp
mtc_lm <- lm(mpg ~ hp + am + hp*am, data = df_mt)
summary(mtc_lm)