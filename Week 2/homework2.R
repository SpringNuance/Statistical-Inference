library(ggplot2)
library(rgl)

data("presidents")
typeof(presidents)
plot.ts(presidents,col="red",main = "US President Quarterly Approval Rating",xlab="Year",ylab="Approval rating")

# run Ctrl + Shift + Enter to run the entire Script
df <- data.frame(approval=as.matrix(presidents), quarter=time(presidents))
approvalRatings <- df$approval
approvalRatings
typeof(approvalRatings)
std <- 

# mean
mean(approvalRatings, na.rm = TRUE)

# median
median(approvalRatings, na.rm = TRUE)

# standard deviation
sd(approvalRatings, na.rm = TRUE)

# quantile 
quantile(approvalRatings, 0.25, na.rm = TRUE)
quantile(approvalRatings, 0.75, na.rm = TRUE)

# range
range(approvalRatings, na.rm = TRUE)
