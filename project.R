##### NYSE Final Project
#### Joey Cody & Morgan Geffroy


## Get first i symbols of our NASDAQ stocks
symbols <- function(i) { 
  vector <- as.vector(CompanyNasd.filtered$Symbol[1:i]) 
  return(getSymbols(vector, src="yahoo"))
}
SymbolList <- symbols(30)

# Return yearly return of specifid input stocks, adjust to remain `xtc` 
# class for handling of data with quantmod
periodreturn <- function(i, time) {
  
  # paste time period with return ex. 'yearly' + 'Return'
  duration <- paste(time, "Return", sep="")
  # get specified time series using SymbolList
  timeseries <- adjustOHLC(get(SymbolList[i]), adjust = "split", use.Adjusted=TRUE)
  
  # call duration of return requested with computed time series
  do.call(duration, list(timeseries))
}


# call yearly function and merge results
mydata <- do.call(merge, lapply(1:length(SymbolList), periodreturn, list("yearly")))
mydata <- do.call(merge, lapply(1:length(SymbolList), periodreturn, list("monthly")))
mydata <- do.call(merge, lapply(1:length(SymbolList), periodreturn, list("quarterly")))

# transform and rename rownames
mydata <- t(mydata)
rownames(mydata) <- SymbolList
mydata <- na.omit(mydata)



### agglomerative clustering using ward's/single/complete clusts
#scale
medians <- apply(mydata, 2, median)
mads <- apply(mydata, 2, mad)
s.mydata <- scale(mydata, center=medians, scale=mads)

# fit
d.mat <- dist(s.mydata)
h.ward <- hclust(d.mat, method = "ward")
h.single <- hclust(d.mat, method= "single")
h.complete <- hclust(d.mat, method= "complete")

# plot
library(cluster)
plot(h.ward, cex = 0.5, main = "Ward’s Method", xlab = "Cells", col = "red")
plot(h.single, cex = 0.5, main = "Single-link", xlab = "Cells", col = "blue")
plot(h.complete, cex = 0.5, main = "Complete-link", xlab = "Cells", col = "green")



### k-means clustering

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
# sum of squares
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}

# plot 
plot(1:15, wss, type="b", col="red", xlab="Number of Clusters", ylab="Within groups sum of squares")

# 7 cluster solution
fit <- kmeans(mydata, 7) 
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata.kclust <- fit$cluster
