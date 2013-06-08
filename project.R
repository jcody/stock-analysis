###########################################################################################
#######                                  Homework 2                                 #######
###################                                                      ##################
###########                     Joey CODY & Morgan GEFFROY                      ###########
###################                                                      ##################
#######                                 06/07/2013                                  #######
###########################################################################################


#########################  (Activation of the required package)  ##########################	
library(quantmod)
library(ggplot2)
library(cluster)
library(e1071)
library(fpc)
library(xts)



######################  (Importation of the name of the companies)  #######################

# Url for downloading company name of the nasdaq
## File companylist.csv
http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nasdaq&render=download	

# Url for downloading company name of the NYSE
## File companylistNYSE.csv
http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nyse&render=download

CompanyNasd <- read.csv(file="FinancialDataAnalysis/companylist.csv",head=TRUE,sep=",")		##Data.frame of the name of the company in the nasdaq
CompanyNyse <- read.csv(file="FinancialDataAnalysis/companylistNYSE.csv",head=TRUE,sep=",") ##Data.frame of the name of the company in the Nyse

drops <- c("ADR.TSO","IPOyear","Summary.Quote","X")					     							  #Names of the columns I want to get rid of 
CompanyNasd.filtered <- CompanyNasd[CompanyNasd$MarketCap>1000000000 & CompanyNasd$Sector != "n/a",!(names(CompanyNasd) %in% drops)]   #Dropping of the columns useless and the company without corresponding sector
CompanyNyse.filtered <- CompanyNyse[CompanyNyse$MarketCap>1000000000 & CompanyNyse$Sector != "n/a",!(names(CompanyNyse) %in% drops)]   #Dropping of the columns useless and the company without corresponding sector


########################  (Getting the data of a given company)  ##########################

# Example for Goldman Sachs
getSymbols("GS")				# Getting the data
periodicity(GS)
to.monthly(GS)					# Monthly Xts file
GS.filtered <- to.monthly(GS)	
# Monthly return (Not the last week to have the same date as the period available for the risk-free asset)
Ret.GS.month <- last(monthlyReturn(GS), '-1 weeks')	
Ret.GS.monthF <- as.vector(Ret.GS.month$monthly.returns) 		# As a vector. 



###########################################################################################
####################################  (Preprocessing)  ####################################
###########################################################################################

## Get first i symbols of our NASDAQ stocks
symbols <- function(i) { 
  vector <- as.vector(sample(CompanyNasd.filtered$Symbol, i, replace = FALSE, prob = NULL))

  return(vector)
}

# Grab specified quantity of j NYSE stock symbols
SymbolList <- symbols(100)

## Grab the timeseries data of j specified stocks
for (j in 1:length(SymbolList)) {
  SymbolList[j] <- getSymbols(SymbolList[j])			
}

# Return yearly return of specific input stocks, adjust to remain `xtc` 
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
mydata1 <- do.call(merge, lapply(1:length(SymbolList), periodreturn, list("yearly")))
mydata2 <- do.call(merge, lapply(1:length(SymbolList), periodreturn, list("monthly")))
mydata3 <- do.call(merge, lapply(1:length(SymbolList), periodreturn, list("quarterly")))
mydata4 <- do.call(merge, lapply(1:length(SymbolList), periodreturn, list("daily")))
mydata5 <- do.call(merge, lapply(1:length(SymbolList), periodreturn, list("weekly")))

# transform and rename rownames
mydata1 <- t(mydata1)
mydata2 <- t(mydata2)
mydata3 <- t(mydata3)
mydata4 <- t(mydata4)
mydata5 <- t(mydata5)

# inherit rownames from stock symbols from SymbolList data frame
rownames(mydata1) <- SymbolList
rownames(mydata2) <- SymbolList
rownames(mydata3) <- SymbolList
rownames(mydata4) <- SymbolList
rownames(mydata5) <- SymbolList

# omit any stocks that don't have historical data from '07 - present
mydata1 <- na.omit(mydata1)
mydata2 <- na.omit(mydata2)
mydata3 <- na.omit(mydata3)
mydata4 <- na.omit(mydata4)
mydata5 <- na.omit(mydata5)

# list of remaining stock symbols in dataset after omitting missing
symbolfinal1 <- rownames(mydata1)
symbolfinal2 <- rownames(mydata2)
symbolfinal3 <- rownames(mydata3)
symbolfinal4 <- rownames(mydata4)
symbolfinal5 <- rownames(mydata5)


# list of stock's sectors 
mydata1.sector <- CompanyNasd.filtered[match(symbolfinal1, CompanyNasd.filtered$Symbol),]$Sector
mydata2.sector <- CompanyNasd.filtered[match(symbolfinal2, CompanyNasd.filtered$Symbol),]$Sector
mydata3.sector <- CompanyNasd.filtered[match(symbolfinal3, CompanyNasd.filtered$Symbol),]$Sector
mydata4.sector <- CompanyNasd.filtered[match(symbolfinal4, CompanyNasd.filtered$Symbol),]$Sector
mydata5.sector <- CompanyNasd.filtered[match(symbolfinal5, CompanyNasd.filtered$Symbol),]$Sector

# length of result vectors
level1 <- length(levels(mydata1.sector))
level2 <- length(levels(mydata2.sector))
level3 <- length(levels(mydata3.sector))
level4 <- length(levels(mydata4.sector))
level5 <- length(levels(mydata5.sector))



###########################################################################################
###########################################################################################
#################################  (ANALYSIS OF THE DATA)  ################################
###########################################################################################
###########################################################################################


### agglomerative clustering using ward's/single/complete clusts
# median
medians1 <- apply(mydata1, 2, median)
medians2 <- apply(mydata2, 2, median)
medians3 <- apply(mydata3, 2, median)
medians4 <- apply(mydata4, 2, median)
medians5 <- apply(mydata5, 2, median)

# median average deviation
mads1 <- apply(mydata1, 2, mad)
mads2 <- apply(mydata2, 2, mad)
mads3 <- apply(mydata3, 2, mad)
mads4 <- apply(mydata4, 2, mad)
mads5 <- apply(mydata5, 2, mad)

# scale
s.mydata1 <- scale(mydata1, center=medians1, scale=mads1)
s.mydata2 <- scale(mydata2, center=medians2, scale=mads2)
s.mydata3 <- scale(mydata3, center=medians3, scale=mads3)
s.mydata4 <- scale(mydata4, center=medians4, scale=mads4)
s.mydata5 <- scale(mydata5, center=medians5, scale=mads5)

# fit distance matrix
d.mat1 <- dist(s.mydata1)
d.mat2 <- dist(s.mydata2)
d.mat3 <- dist(s.mydata3)
d.mat4 <- dist(s.mydata4)
d.mat5 <- dist(s.mydata5)

# hclust ward
h.ward1 <- hclust(d.mat1, method = "ward")
h.ward2 <- hclust(d.mat2, method = "ward")
h.ward3 <- hclust(d.mat3, method = "ward")
h.ward4 <- hclust(d.mat4, method = "ward")
h.ward5 <- hclust(d.mat5, method = "ward")

# hclust single
h.single1 <- hclust(d.mat1, method= "single")
h.single2 <- hclust(d.mat2, method= "single")
h.single3 <- hclust(d.mat3, method= "single")
h.single4 <- hclust(d.mat4, method= "single")
h.single5 <- hclust(d.mat5, method= "single")

# hclust complete-link
h.complete1 <- hclust(d.mat1, method= "complete")
h.complete2 <- hclust(d.mat2, method= "complete")
h.complete3 <- hclust(d.mat3, method= "complete")
h.complete4 <- hclust(d.mat4, method= "complete")
h.complete5 <- hclust(d.mat5, method= "complete")

# Allow 6 plots
par( mfrow = c( 2, 3 ) )
# plot ward's cluster
plot(h.ward1, cex = 0.5, main="yearly"    ,sub = "Ward’s Method", xlab = "Cells", col = "red")
plot(h.ward2, cex = 0.5, main="monthly"   ,sub = "Ward’s Method", xlab = "Cells", col = "red")
plot(h.ward3, cex = 0.5, main="quarterly" ,sub = "Ward’s Method", xlab = "Cells", col = "red")
plot(h.ward4, cex = 0.5, main="daily"     ,sub = "Ward’s Method", xlab = "Cells", col = "red")
plot(h.ward5, cex = 0.5, main="Weekly"    ,sub = "Ward’s Method", xlab = "Cells", col = "red")

# final ward's dendrogram plot
plot(h.ward3, cex=0.5, main="Quarterly Returns (Ward's)", col="#487AA1", col.main="#2B4C66", col.lab="#7C8071", col.axis="#F38630")

# plot single link cluster
plot(h.single1, cex = 0.5, main="yearly"   ,sub = "Single-link", xlab = "Cells", col = "blue")
plot(h.single2, cex = 0.5, main="monthly"  ,sub = "Single-link", xlab = "Cells", col = "blue")
plot(h.single3, cex = 0.5, main="quarterly",sub = "Single-link", xlab = "Cells", col = "blue")
plot(h.single4, cex = 0.5, main="daily"    ,sub = "Single-link", xlab = "Cells", col = "blue")
plot(h.single5, cex = 0.5, main="weekly"   ,sub = "Single-link", xlab = "Cells", col = "blue")

# plot complete-link
plot(h.complete1, cex = 0.5, main="yearly"   ,sub = "Complete-link", xlab = "Cells", col = "green")
plot(h.complete2, cex = 0.5, main="monthly"  ,sub = "Complete-link", xlab = "Cells", col = "green")
plot(h.complete3, cex = 0.5, main="Quarterly Returns (Complete-Link)",sub = "Complete-link", xlab = "Cells", col="#487AA1", col.main="#2B4C66", col.lab="#7C8071", col.axis="#F38630")
plot(h.complete4, cex = 0.5, main="daily"    ,sub = "Complete-link", xlab = "Cells", col = "green")
plot(h.complete5, cex = 0.5, main="weekly"   ,sub = "Complete-link", xlab = "Cells", col = "green")




###### k-means clustering
###########################################################################################

# Determine number of clusters
wss1 <- (nrow(mydata1)-1)*sum(apply(mydata1,2,var))
wss2 <- (nrow(mydata2)-1)*sum(apply(mydata2,2,var))
wss3 <- (nrow(mydata3)-1)*sum(apply(mydata3,2,var))
wss4 <- (nrow(mydata4)-1)*sum(apply(mydata4,2,var))
wss5 <- (nrow(mydata5)-1)*sum(apply(mydata5,2,var))

# sum of squares
for (i in 1:100) {
  wss1[i] <- sum(kmeans(mydata1, centers=i)$withinss)
  wss2[i] <- sum(kmeans(mydata2, centers=i)$withinss)
  wss3[i] <- sum(kmeans(mydata3, centers=i)$withinss)
  wss4[i] <- sum(kmeans(mydata4, centers=i)$withinss)
  wss5[i] <- sum(kmeans(mydata5, centers=i)$withinss)
}

# plot 
plot(1:30, wss1, type="b", col="red", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Yearly")
plot(1:30, wss2, type="b", col="red", xlab="Number of Clusters", ylab="Within groups sum of squares", main="monthly")
plot(1:30, wss3, type="b", col="red", xlab="Number of Clusters", ylab="Within groups sum of squares", main="quaterly")
plot(1:30, wss4, type="b", col="red", xlab="Number of Clusters", ylab="Within groups sum of squares", main="daily")
plot(1:30, wss5, type="b", col="red", xlab="Number of Clusters", ylab="Within groups sum of squares", main="weekly")


# Solutions with 9 cluster = the number of sector in the dataset. 
fit1 <- kmeans(mydata1, level1)
fit2 <- kmeans(mydata2, level2)
fit3 <- kmeans(mydata3, level3)
fit4 <- kmeans(mydata4, level4)
fit5 <- kmeans(mydata5, level5)


fit1 <- kmeans(mydata1, 9)
fit2 <- kmeans(mydata2, 9)
fit3 <- kmeans(mydata3, 9)
fit4 <- kmeans(mydata4, 9)
fit5 <- kmeans(mydata5, 9)


# get cluster means 
aggregate(mydata1,by=list(fit1$cluster),FUN=mean)
aggregate(mydata2,by=list(fit2$cluster),FUN=mean)
aggregate(mydata3,by=list(fit3$cluster),FUN=mean)
aggregate(mydata4,by=list(fit4$cluster),FUN=mean)
aggregate(mydata5,by=list(fit5$cluster),FUN=mean)


# append cluster assignment
mydata.kclust1 <- fit1$cluster
mydata.kclust2 <- fit2$cluster
mydata.kclust3 <- fit3$cluster
mydata.kclust4 <- fit4$cluster
mydata.kclust5 <- fit5$cluster


# Analyze the cluster to which each cell  is allocated
(SymbolList.kmeans1 <- data.frame(type = mydata1.sector, # True value of classes
                                   clusterA = mydata.kclust1))
(SymbolList.kmeans2 <- data.frame(type = mydata2.sector, # True value of classes
                                   clusterA = mydata.kclust2))
(SymbolList.kmeans3 <- data.frame(type = mydata3.sector, # True value of classes
                                   clusterA = mydata.kclust3))
(SymbolList.kmeans4 <- data.frame(type = mydata4.sector, # True value of classes
                                   clusterA = mydata.kclust4))
(SymbolList.kmeans5 <- data.frame(type = mydata5.sector, # True value of classes
                                   clusterA = mydata.kclust5))




# Lumping Error - Splitting error  - total error - total error rate
count.clustering.errors <- function(true.classes,inferred.clusters) {
	# Should double-check that the two input vectors have the same length!
	n = length(true.classes)

	# Always a good idea to explicitly declare counters
	lumping.error = 0
	splitting.error = 0

	for (i in 1:(n-1)) {
		for (j in (i+1):n) {
			if ((true.classes[i] != true.classes[j]) # Different classes
			& (inferred.clusters[i] == inferred.clusters[j])) { # Same cluster
				lumping.error = lumping.error + 1
			}
			if ((true.classes[i] == true.classes[j]) # Same classes
			& (inferred.clusters[i] != inferred.clusters[j])) { #Different Cluster
				splitting.error = splitting.error +1
			}
		} # Close for j
	} # Close for i
	
	total.errors = lumping.error + splitting.error
	n.distinct.pairs = n*(n-1)/2
	total.error.rate = total.errors/n.distinct.pairs
	
	return(list(lumping.error = lumping.error,
				splitting.error = splitting.error,
				total.errors = total.errors,
				total.error.rate = total.error.rate))
}

# count clusters
count.clustering.errors(SymbolList.kmeans1$type, SymbolList.kmeans1$clusterA)
count.clustering.errors(SymbolList.kmeans2$type, SymbolList.kmeans2$clusterA)
count.clustering.errors(SymbolList.kmeans3$type, SymbolList.kmeans3$clusterA)
count.clustering.errors(SymbolList.kmeans4$type, SymbolList.kmeans4$clusterA)
count.clustering.errors(SymbolList.kmeans5$type, SymbolList.kmeans5$clusterA)

# error rates
error.k1 <- (count.clustering.errors(true.classes = SymbolList.kmeans1$type, inferred.clusters = SymbolList.kmeans1$clusterA))
error.k2 <- (count.clustering.errors(true.classes = SymbolList.kmeans2$type, inferred.clusters = SymbolList.kmeans2$clusterA))
error.k3 <- (count.clustering.errors(true.classes = SymbolList.kmeans3$type, inferred.clusters = SymbolList.kmeans3$clusterA))
error.k4 <- (count.clustering.errors(true.classes = SymbolList.kmeans4$type, inferred.clusters = SymbolList.kmeans4$clusterA))
error.k5 <- (count.clustering.errors(true.classes = SymbolList.kmeans5$type, inferred.clusters = SymbolList.kmeans5$clusterA))


a <- rbind(round(unlist(error.k1),2), round(unlist(error.k2),2), round(unlist(error.k3),2), round(unlist(error.k4),2), round(unlist(error.k5),2))
 
ERROR.kmeans <- data.frame(Periodicity = c("Yearly", "monthly", "quarterly", "daily", "weekly") , a)

View(ERROR.kmeans)

# show error results
table(SymbolList.kmeans1$type,SymbolList.kmeans1$clusterA)


# Function to compute the entropy.
entropy.from.counts <- function(x) {
	# Normalize to get a distribution
	p = x/sum(x)
	# Discard the zero-probability entries
	p = p[p>0]
	entropy=-sum(p*log(p,base=2))
	return(entropy)
}

# Gives a table with the entropy for each sector
round(apply(table(SymbolList.kmeans1$type,SymbolList.kmeans1$clusterA),1,entropy.from.counts),2)



###### k-medoids clustering
###########################################################################################

# K-medoids
Fit1a <- pamk(mydata1,metric="manhattan")
Fit1b <- pamk(mydata1,metric="euclidean")
Fit2a <- pamk(mydata2,metric="manhattan")
Fit2b <- pamk(mydata2,metric="euclidean")
Fit3a <- pamk(mydata3,metric="manhattan")
Fit3b <- pamk(mydata3,metric="euclidean")
Fit4a <- pamk(mydata4,metric="manhattan")
Fit4b <- pamk(mydata4,metric="euclidean")
Fit5a <- pamk(mydata5,metric="manhattan")
Fit5b <- pamk(mydata5,metric="euclidean")

# plot K-medoids
plot (Fit1a$pamobject)
plot (Fit1b$pamobject)
plot (Fit2a$pamobject)
plot (Fit2b$pamobject)
plot (Fit3a$pamobject)
plot (Fit3b$pamobject)
plot (Fit4a$pamobject)
plot (Fit4b$pamobject)
plot (Fit5a$pamobject)
plot (Fit5b$pamobject)


###### DBSCAN clustering
dbscan(mydata1, 9, showplot=TRUE)
dbscan(mydata2, 9, showplot=TRUE)
dbscan(mydata3, 9, showplot=TRUE)
dbscan(mydata4, 9, showplot=TRUE)
dbscan(mydata5, 9, showplot=TRUE)




###############################  (Computation of the beta)  ###############################
##################################  (For a given stock)  ##################################

date <- as.Date(SPretm$Date)
riskprem<- vector("numeric", 76)			#vector of the risk premium returns. 

for (i in 1:76){
	riskprem[i] <- SPretm.filtered$x[i] - Rfree[i]	
}

riskprem  <- t(riskprem)
riskprem <- t(riskprem)								# Risk-Premium


############# BETA
beta.lm = lm(Ret.GS.monthF ~ riskprem, data=faithful)
coef(beta.lm)["riskprem"]


############# BETA function one stock.
BETAs <- function (symbol) {
		a <- length(symbol)										#number of month needed and the dimension of the vector created.
		riskprem <- riskprem

		Ret.month <- last(monthlyReturn(symbol), '-2 months')	#Monthly return (Not the last two week to have the same date as the period available for the risk-free asset)
		Ret.monthF <- as.vector(Ret.month$monthly.returns) 		#As a vector. 

		a <- lm((Ret.monthF) ~ riskprem)						#linear regression	
		return(coef(a)["riskprem"])								#Return the coefficient of this linear regression. 
}


#############################  (Function for several Stocks)  #############################

# Beta function
BETAm <- function (symbol) {
	a <- length(symbol)
	b <- vector("numeric", 76)
	riskprem <- riskprem

	for (i in 1:a) {		
		c <- BETAs(get(symbol[i]))
		b[i] <- c
	}
	return(b)
}


# Funtion to take the market capitalistion of a given stock. 
Market.Cap <- function(symbol) {
	a <- length(symbol)
	for (i in 1:a) {
			# ATTENTION: We have to give the symbol with quote ("")
			m[i] <- CompanyNasd.filtered[(CompanyNasd.filtered$Symbol == symbol[i]),"MarketCap"]
	}
	return(m)
}


# Fucntion to gather market capitalisation and beta of a set of stocks. 
MC.B <- function (symbols) {
	a <- length(symbols)
	A <- matrix (0,a,2)
	A[,1] <- Market.Cap(symbols)
	for (i in 1:a) {
		A[i,2] <- 12*BETAs(get(symbols[i]))
		}
	return(A)
}


## Get rid of the outlier. 
# I have to get ride of stock with a too big market capitalisation. 
Mydata.Beta <- MC.B(symbolfinal2) 
Mydata.Beta.filtered <- Mydata.Beta[(Mydata.Beta[,1] < 20000000000),]
BETA.symbol <- rownames(Mydata.Beta.filtered)
Mydata.Beta.sector <- CompanyNasd.filtered[match(Mydata.Beta.filtered, CompanyNasd.filtered$Symbol),]$Sector


# DBSCAN algo to compute filtered stocks
dbscan(Mydata.Beta.filtered, 2,  col.lab="#7C8071", col.axis="#F38630", showplot=TRUE )
DBSCAN1 <- dbscan(Mydata.Beta.filtered, 300000000, showplot=TRUE)


# Dendrograms
Medians <- apply(Mydata.Beta.filtered, 2, median)
Mads <- apply(Mydata.Beta.filtered, 2, mad)
S.Mydata <- scale(Mydata.Beta.filtered, center=Medians, scale=Mads)
D.Mat <- dist(S.Mydata)


# hierarchal clustering against filtered stocks
H.Ward <- hclust(D.Mat, method = "ward")
H.Single <- hclust(D.Mat, method= "single")
H.Complete <- hclust(D.Mat, method= "complete")


# Plots of Dendodgrammes
plot(H.Ward, cex = 0.5, main="Market capitalisation and beta" ,sub = "Ward’s Method", xlab = "Cells", col = "red")
plot(H.Single, cex = 0.5, main="Market capitalisation and beta" ,sub = "Single-link", xlab = "Cells", col = "blue")
plot(H.Complete, cex = 0.5, main="Market Cap with beta (Complete-Link)"  ,sub = "Complete-link", col="#487AA1", col.main="#2B4C66", col.lab="#7C8071", col.axis="#F38630")


# K-means
WSS <- (nrow(Mydata.Beta.filtered)-1)*sum(apply(Mydata.Beta.filtered,2,var))

for (i in 2:30) {
  WSS[i] <- sum(kmeans(Mydata.Beta.filtered,, centers=i)$withinss)
}

plot(1:30, WSS, type="b", col="red", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Market capitalisation and beta")

points(1:30,WSS[1:30], cex = 0.9)
abline(v = c(4,5), col = "blue") # add vertical and horizontal lines


FIT <- kmeans(Mydata.Beta.filtered, 4)
aggregate(Mydata.Beta.filtered,by=list(FIT$cluster),FUN=mean)

Mydata.KClust <- FIT$cluster

(SymbolList.KMEANS <- data.frame(type = Mydata.Beta.sector, # True value of classes
                                   clusterA = Mydata.KClust))

# Plot of the K-means
plotcluster(Mydata.Beta.filtered, FIT$cluster)
clusplot(Mydata.Beta.filtered, FIT$cluster, color=TRUE, shade=TRUE, labels=2, lines=1)



#### K-medoids
###########################################################################################
Kmedoids <- pam(Mydata.Beta.filtered, 9)

Kmedoids1 <- pam(Mydata.Beta.filtered,4)


# Plot the Kmedoids
plotcluster(Mydata.Beta.filtered, Kmedoids$cluster)
plotcluster(Mydata.Beta.filtered, Kmedoids1$cluster)

clusplot(Mydata.Beta.filtered, Kmedoids$cluster, color=TRUE, shade=TRUE, labels=2, lines=1)
clusplot(Mydata.Beta.filtered, Kmedoids1$cluster, color=TRUE, shade=TRUE, labels=2, lines=1)



# C-means
###########################################################################################

cl <- cmeans(Mydata.Beta.filtered, 3, iter.max = 100, verbose = FALSE,
									  dist = "euclidean", method = "cmeans", m = 2,
									  rate.par = NULL, weights = 1, control = list())

plotcluster(Mydata.Beta.filtered, cl$cluster, main="C-Means", col="#487AA1", col.main="#2B4C66", col.lab="#7C8071", col.axis="#F38630")
clusplot(Mydata.Beta.filtered, cl$cluster, color=TRUE, shade=TRUE, labels=2, lines=1)




#################################################################
#### Match SymbolList to CompanyNasd.filtered and get sector ####
#################################################################

mydata1.sector <- CompanyNasd.filtered[match(symbolfinal1, CompanyNasd.filtered$Symbol),]$Sector


# Obtain parallel coordinate plot
### Sorry - didn't include this one on the final
### because it wasn't interesting enough!
#################################################################
#################################################################

plot.zoo(zoo(t(mydata1), order.by=1:ncol(mydata1)), screens = 1, type='o', col = rainbow(ncol(t(mydata1))), ylab="Data")

lines(mydata1.sector, col=1)

legend('topright', names(mydata1.sector), mydata1.sector, col=1:ncol(mydata1), lty=1, cex=.65)