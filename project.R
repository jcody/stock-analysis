###########################################################################################
#######                                  Homework 2                                 #######
###################                                                      ##################
###########                     Joey CODY & Morgan GEFFROY                      ###########
###################                                                      ##################
#######                                 06/07/2013                                  #######
###########################################################################################

# I start by setting my workspace.
setwd("/Users/morgangeffroy/Google Drive/Cours/UCSB/Spring 2013/Data Mining/Project")    # Set your own work space between the " ".
rm(list=ls())                                                                            # Erase all object in the memory.



###########################################################################################
#####################################  (import data)  #####################################
########################################################################################### 


#########################  (Activation of the required package)  ##########################
		
											#Installation of the package quantmod
install.packages('quantmod')
											#Activation of the package quantmod
library("quantmod")	


											#Installation of the package RQuantLib
install.packages("RQuantLib")
											#Activation of the package RQuantLib
library("RQuantLib")	

											#Installation of the package quantmod add-on
install.packages("qmao")
											#Activation of the package quantmod	add-on
library("qmao")


######################  (Importation of the name of the companies)  #######################


http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nasdaq&render=download	#Url for downloading company name of the nasdaq
				##File companylist.csv

http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nyse&render=download   #Url for downloading company name of the NYSE
				##File companylistNYSE.csv



CompanyNasd <- read.csv(file="FinancialDataAnalysis/companylist.csv",head=TRUE,sep=",")		##Data.frame of the name of the company in the nasdaq
CompanyNyse <- read.csv(file="FinancialDataAnalysis/companylistNYSE.csv",head=TRUE,sep=",") ##Data.frame of the name of the company in the Nyse


drops <- c("ADR.TSO","IPOyear","Summary.Quote","X")					     							  #Names of the columns I want to get rid of 
CompanyNasd.filtered <- CompanyNasd[CompanyNasd$MarketCap>1000000000 & CompanyNasd$Sector != "n/a",!(names(CompanyNasd) %in% drops)]   #Dropping of the columns useless and the company without corresponding sector

CompanyNyse.filtered <- CompanyNyse[CompanyNyse$MarketCap>1000000000 & CompanyNyse$Sector != "n/a",!(names(CompanyNyse) %in% drops)]   #Dropping of the columns useless and the company without corresponding sector






########################  (Getting the data of a given company)  ##########################

#example for Goldman Sachs
getSymbols("GS")				# Getting the data

periodicity(GS)

to.monthly(GS)					# Monthly Xts file

GS.filtered <- to.monthly(GS)	

Ret.GS.month <- last(monthlyReturn(GS), '-1 weeks')	#Monthly return (Not the last week to have the same date as the period available for the risk-free asset)

Ret.GS.monthF <- as.vector(Ret.GS.month$monthly.returns) 		#As a vector. 




###########################################################################################
####################################  (Preprocessing)  ####################################
###########################################################################################


## Get first i symbols of our NASDAQ stocks
symbols <- function(i) { 
  vector <- as.vector(sample(CompanyNasd.filtered$Symbol, i, replace = FALSE, prob = NULL))
  
  return(vector)
}

SymbolList <- symbols(100)


 for (j in 1:100) {
    getSymbols(SymbolList[j])		##It works.		
  }

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


rownames(mydata1) <- SymbolList
rownames(mydata2) <- SymbolList
rownames(mydata3) <- SymbolList
rownames(mydata4) <- SymbolList
rownames(mydata5) <- SymbolList


mydata1 <- na.omit(mydata1)
mydata2 <- na.omit(mydata2)
mydata3 <- na.omit(mydata3)
mydata4 <- na.omit(mydata4)
mydata5 <- na.omit(mydata5)


symbolfinal1 <- rownames(mydata1) #return the final list of the stock after having getting ride of the missing value
symbolfinal2 <- rownames(mydata2)
symbolfinal3 <- rownames(mydata3)
symbolfinal4 <- rownames(mydata4)
symbolfinal5 <- rownames(mydata5)

###########################################################################################
###########################################################################################
#################################  (ANALYSIS OF THE DATA)  ################################
###########################################################################################
###########################################################################################


### agglomerative clustering using ward's/single/complete clusts
#scale
medians1 <- apply(mydata1, 2, median)
medians2 <- apply(mydata2, 2, median)
medians3 <- apply(mydata3, 2, median)
medians4 <- apply(mydata4, 2, median)
medians5 <- apply(mydata5, 2, median)

mads1 <- apply(mydata1, 2, mad)
mads2 <- apply(mydata2, 2, mad)
mads3 <- apply(mydata3, 2, mad)
mads4 <- apply(mydata4, 2, mad)
mads5 <- apply(mydata5, 2, mad)

s.mydata1 <- scale(mydata1, center=medians1, scale=mads1)
s.mydata2 <- scale(mydata2, center=medians2, scale=mads2)
s.mydata3 <- scale(mydata3, center=medians3, scale=mads3)
s.mydata4 <- scale(mydata4, center=medians4, scale=mads4)
s.mydata5 <- scale(mydata5, center=medians5, scale=mads5)

# fit
d.mat1 <- dist(s.mydata1)
d.mat2 <- dist(s.mydata2)
d.mat3 <- dist(s.mydata3)
d.mat4 <- dist(s.mydata4)
d.mat5 <- dist(s.mydata5)

h.ward1 <- hclust(d.mat1, method = "ward")
h.ward2 <- hclust(d.mat2, method = "ward")
h.ward3 <- hclust(d.mat3, method = "ward")
h.ward4 <- hclust(d.mat4, method = "ward")
h.ward5 <- hclust(d.mat5, method = "ward")

h.single1 <- hclust(d.mat1, method= "single")
h.single2 <- hclust(d.mat2, method= "single")
h.single3 <- hclust(d.mat3, method= "single")
h.single4 <- hclust(d.mat4, method= "single")
h.single5 <- hclust(d.mat5, method= "single")

h.complete1 <- hclust(d.mat1, method= "complete")
h.complete2 <- hclust(d.mat2, method= "complete")
h.complete3 <- hclust(d.mat3, method= "complete")
h.complete4 <- hclust(d.mat4, method= "complete")
h.complete5 <- hclust(d.mat5, method= "complete")

# plot
library(cluster)

par( mfrow = c( 2, 3 ) )  #I split the screen by 4.

plot(h.ward1, cex = 0.5, main="yearly"    ,sub = "Ward’s Method", xlab = "Cells", col = "red")
plot(h.ward2, cex = 0.5, main="monthly"   ,sub = "Ward’s Method", xlab = "Cells", col = "red")
plot(h.ward3, cex = 0.5, main="quarterly" ,sub = "Ward’s Method", xlab = "Cells", col = "red")
plot(h.ward4, cex = 0.5, main="daily"     ,sub = "Ward’s Method", xlab = "Cells", col = "red")
plot(h.ward5, cex = 0.5, main="Weekly"     ,sub = "Ward’s Method", xlab = "Cells", col = "red")


plot(h.single1, cex = 0.5, main="yearly"   ,sub = "Single-link", xlab = "Cells", col = "blue")
plot(h.single2, cex = 0.5, main="monthly"  ,sub = "Single-link", xlab = "Cells", col = "blue")
plot(h.single3, cex = 0.5, main="quarterly",sub = "Single-link", xlab = "Cells", col = "blue")
plot(h.single4, cex = 0.5, main="daily"    ,sub = "Single-link", xlab = "Cells", col = "blue")


plot(h.complete1, cex = 0.5, main="yearly"   ,sub = "Complete-link", xlab = "Cells", col = "green")
plot(h.complete2, cex = 0.5, main="monthly"  ,sub = "Complete-link", xlab = "Cells", col = "green")
plot(h.complete3, cex = 0.5, main="quarterly",sub = "Complete-link", xlab = "Cells", col = "green")
plot(h.complete4, cex = 0.5, main="daily"    ,sub = "Complete-link", xlab = "Cells", col = "green")








###### k-means clustering

# Determine number of clusters
wss1 <- (nrow(mydata1)-1)*sum(apply(mydata1,2,var))
wss2 <- (nrow(mydata2)-1)*sum(apply(mydata2,2,var))
wss3 <- (nrow(mydata3)-1)*sum(apply(mydata3,2,var))
wss4 <- (nrow(mydata4)-1)*sum(apply(mydata4,2,var))
wss5 <- (nrow(mydata5)-1)*sum(apply(mydata5,2,var))

# sum of squares
for (i in 1:60) {
  wss1[i] <- sum(kmeans(mydata1, centers=i)$withinss)
  wss2[i] <- sum(kmeans(mydata2, centers=i)$withinss)
  wss3[i] <- sum(kmeans(mydata3, centers=i)$withinss)
  wss4[i] <- sum(kmeans(mydata4, centers=i)$withinss)
  wss5[i] <- sum(kmeans(mydata5, centers=i)$withinss)
}

# plot 
plot(1:25, wss1, type="b", col="red", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Yearly")
plot(1:24, wss2, type="b", col="red", xlab="Number of Clusters", ylab="Within groups sum of squares", main="monthly")
plot(1:24, wss3, type="b", col="red", xlab="Number of Clusters", ylab="Within groups sum of squares", main="quaterly")
plot(1:24, wss4, type="b", col="red", xlab="Number of Clusters", ylab="Within groups sum of squares", main="daily")

# 13,5,6,12 cluster solutions
fit1 <- kmeans(mydata1, 13)
fit2 <- kmeans(mydata2, 5)
fit3 <- kmeans(mydata3, 6)
fit4 <- kmeans(mydata4, 12)

# get cluster means 
aggregate(mydata1,by=list(fit1$cluster),FUN=mean)
aggregate(mydata2,by=list(fit2$cluster),FUN=mean)
aggregate(mydata3,by=list(fit3$cluster),FUN=mean)
aggregate(mydata4,by=list(fit4$cluster),FUN=mean)

# append cluster assignment
mydata.kclust1 <- fit1$cluster
mydata.kclust2 <- fit2$cluster
mydata.kclust3 <- fit3$cluster
mydata.kclust4 <- fit4$cluster

#Analyze the cluster to which each cell  is allocated
(SymbolList.kmeans1 <- data.frame(type = rownames(symbolList), # True value of classes
clusterA = fit1$cluster))




#Lumping Error - Splitting error  - total error - total error rate

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







###### k-medoids clustering
library(fpc)

#K-medoids
Fit1a <- pamk(mydata1,metric="manhattan")
Fit1b <- pamk(mydata1,metric="euclidean")
Fit2a <- pamk(mydata2,metric="manhattan")
Fit2b <- pamk(mydata2,metric="euclidean")
Fit3a <- pamk(mydata3,metric="manhattan")
Fit3b <- pamk(mydata3,metric="euclidean")
Fit4a <- pamk(mydata4,metric="manhattan")
Fit4b <- pamk(mydata4,metric="euclidean")

# plot K-medoids
plot (Fit1a$pamobject)
plot (Fit1b$pamobject)
plot (Fit2a$pamobject)
plot (Fit2b$pamobject)
plot (Fit3a$pamobject)
plot (Fit3b$pamobject)
plot (Fit4a$pamobject)
plot (Fit4b$pamobject)







###### DBSCAN clustering

dbscan(mydata1, 10, showplot=TRUE)









###############################  (Computation of the beta)  ###############################


##################################  (For a given stock)  ##################################

#Data of the Treasury Bond rate for the last 20 year. 
#http://research.stlouisfed.org/fred2/series/GS20/)	

TreasuryBond <- read.csv(file="FinancialDataAnalysis/GS20-monthlyF.csv",head=TRUE,sep=",")	#importation of the csv file downloaded on http://research.stlouisfed.org/fred2/series/GS20/)
					#monthly rate

Rfree <- as.vector(TreasuryBond$VALUE)

SPret <- read.csv(file="FinancialDataAnalysis/S&Preturn.csv",head=TRUE,sep=",")	#importation of the csv file downloaded on http://research.stlouisfed.org/fred2/series/GS20/)
					#daily rate


SPretm <- read.csv(file="FinancialDataAnalysis/S&Preturnm.csv",head=TRUE,sep=",")	#importation of the csv file downloaded on http://research.stlouisfed.org/fred2/series/GS20/)
					#monthly rate

SPretm.filtered <- as.vector(SPretm)			#Create a vector with only the value of the S&P500 at closure, market return.  




Ret.GS.month <- monthlyReturn(GS)													#Monthly return


date <- as.Date(TreasuryBond$DATE)				#Only the date important in the data set. 
date.filtered <- date[date > "2006-12-31"]		#

###### OR

date <- as.Date(SPretm$Date)


RP <- vector("numeric", 76)			#vector of the risk premium returns. 

for(i in 1:76){
		RP[i] <- SPretm.filtered[i] - Rfree[i]	
}
RP <- t(RP)
RP <- t(RP)								# Risk-Premium


############# BETA

beta.lm = lm(Ret.GS.monthF ~ RP, data=faithful)


coef(beta.lm)["RP"]

############# BETA function one stock.

BETAs <- function (symbol) {

				a <- length(symbol)

				riskprem<- RP

				Ret.month <- last(monthlyReturn(symbol), '-2 weeks')	#Monthly return (Not the last two week to have the same date as the period available for the risk-free asset)

				Ret.monthF <- as.vector(Ret.month$monthly.returns) 		#As a vector. 

				a <- lm(Ret.monthF ~ riskprem)	
				return(coef(a)["riskprem"])
		}



#############################  (Function for several Stocks)  #############################


##Beta function

BETAm <- function (symbol) {

				a <- length(symbol)

				riskprem<- RP

				for (i in 1:a) {

					Ret.month <- last(monthlyReturn(symbol[i]), '-2 weeks')	#Monthly return (Not the last two week to have the same date as the period available for the risk-free asset)

					Ret.monthF <- as.vector(Ret.month$monthly.returns) 		#As a vector. 

					a <- lm(Ret.monthF ~ riskprem)
					
					b[i] <- as.numeric(coef(a)["riskprem"])

					}


				return(b)
		}



Market.Cap <- function(symbol) {

			a <- length(symbol)

			for (i in 1:a) {
  
  				# ATTENTION: We have to give the symbol with quote ("")
 				m[i] <- CompanyNasd.filtered[(CompanyNasd.filtered$Symbol == symbol[i]),"MarketCap"]

 			}

  			return(m)
}


MC.B <- function (symbols) {

			a <- length(symbols)

			A <- matrix (0,a,2)

			A[,1] <- Market.Cap(symbols)

			for (i in 1:a) {

				A[i,2] <- BETAs(as.integer(symbols[i]))

				}

			return(A)

			}




