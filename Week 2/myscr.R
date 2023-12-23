#l <- l[!is.na(l)]

#2
boxplot(islands)
boxplot(log(islands))
boxplot(islands, outline = F)
out <- boxplot(islands, plot=F)$out

#d. Remove some of the outliers (and think of a possible reason for justifying this!) from 
#the data and compute the same measures as in part c. 
islands %in% out
isl <- islands[!islands %in% out]
out2 <- out[-order(out, decreasing = T)[8:8]] 
t <- out[out != 840]

#3
sort(table(Nile), decreasing = T)[1:4]