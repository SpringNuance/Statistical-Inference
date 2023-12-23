# Read data:
lipid_data <- read.csv("data_lipid.csv", header = T)

# a) Describe the data:
summary(lipid_data)

# See data type:
str(lipid_data)

# b) separate normally distributed from the data using
# shapiro.test() with really conservative confidence level alpha 0.001 and
# use only the PLACEBO arm.
# Three steps:
# 1. Calculate the p-values and save the into a new vector
# 2. Find which of the p-values are smaller than alpha
# 3. Create two new data.frames: normally distributed and not-normal distributed variables
shapiro_p_values <- apply(lipid_data[1:100,-1],2, function(x) shapiro.test(x)$p.value)

non_normal_indices <- which(shapiro_p_values < 0.001)
normal_indices <- which(!shapiro_p_values < 0.001)

lipid_data_normal <- lipid_data[,c(1, 1 + normal_indices)]
lipid_data_non_normal <- lipid_data[,c(1, 1 + non_normal_indices)]

# c) conduct a t.test for normally distributed variables to find statistically signifcant
# difference between the placebo and the treatment arm using confidence level alpha = 0.05. Which variables are significantly differing?
ttest_p_values <- apply(lipid_data_normal[,-1],2, function(x) t.test(x ~ lipid_data_normal$studyarm)$p.value)
which(ttest_p_values < 0.05)

# d) conduct a Wilcoxon Rank Sum test for non-normally distributed variables to find statistically signifcant
# difference between the placebo and the treatment arm using confidence level alpha = 0.05. Which variables are significantly differing?
wtest_p_values <- apply(lipid_data_non_normal[,-1],2, function(x) wilcox.test(x ~ lipid_data_normal$studyarm)$p.value)
which(wtest_p_values < 0.05)

# e) To control false discovery rate, adjust the p-values using Benjamini-Hochberg p-value adjustement. Which variables are significantly
# differing now?
which(p.adjust(ttest_p_values, method = 'BH') < 0.05)
which(p.adjust(wtest_p_values, method = 'BH') < 0.05)


# Optional: random forest classification
# Conduct a random forest classification and study correspondence between
# random forest variable importance and statistically signifcant variables found in part e)
library(randomForest)
fit1 <- randomForest(x = lipid_data[,-1], y = factor(lipid_data[,1]), proximity = T)
fit1
varImpPlot(fit1)
