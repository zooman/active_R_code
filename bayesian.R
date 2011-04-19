###############################################################################



# BAYESIAN NETWORKS
#--------------------------------------------------------------------------------

# Call the libraries
#--------------------------------------------------------------------------------
library(deal)
#library(MASTINO)

# Read the data set (Hotel data)into a data frame
#--------------------------------------------------------------------------------
data = read.csv(file="hi.csv",header=TRUE,sep=",")

# Check the attributes of the data 
#--------------------------------------------------------------------------------
str(data)

# Choose a sample of the data for the network building
#--------------------------------------------------------------------------------
data.sample = subset(data, select = c(RMS_AVAIL_QTY, LOC_DESC, Occupancy, AvgDailyRate, Compet_Occupancy, PercentBusiness, hotelcount5mile ,web_nts_totsty))

# Initialize the network
#--------------------------------------------------------------------------------
hotel.fit = network(data.sample)

# Obtain the network information
#-------------------------------------------------------------------------------
hotel.fit
str(hotel.fit)

# Create a simple plot
#-------------------------------------------------------------------------------
plot(hotel.fit)

# Initialize the joint probability distribution
#-------------------------------------------------------------------------------
hotel.prior = jointprior(hotel.fit)

# Learn the network
#-------------------------------------------------------------------------------
hotel.fit=getnetwork(learn(hotel.fit,data.sample,hotel.prior))

# Specifying node information i.e nodes 1,2 and 3 depend on each other
#-------------------------------------------------------------------------------
#hotel.fit = getnetwork(insert(hotel.fit,2,3,data.sample,hotel.prior))
#hotel.fit = getnetwork(insert(hotel.fit,3,1,data.sample,hotel.prior))
#hotel.fit = getnetwork(insert(hotel.fit,2,1,data.sample,hotel.prior))

#Autosearch the network (returns all tried networks from the initial network)
#-------------------------------------------------------------------------------
hisc <- autosearch(hotel.fit,data.sample,hotel.prior,trace=FALSE)
plot(getnetwork(hisc))

# Use the inbuilt heuristic to fit the network
#--------------------------------------------------------------------------------
hisc2 <- heuristic(hotel.fit,data.sample,hotel.prior,restart=10,trace=FALSE)
plot(getnetwork(hisc2))

par(mfrow=c(1,2))
plot(getnetwork(hisc))
plot(getnetwork(hisc2))


# Plot all the network structures for the trained network
#--------------------------------------------------------------------------------
plot(makenw(gettable(hisc2),hotel.fit))

# Trylist enables to check if the node has been learned earlier with the same parent configuration
# Takes a long time to run-------------------------------------------------------------------------------
#maketrylist(hotel.fit,data.sample,hotel.prior,timetrace=FALSE)

# Network family
#--------------------------------------------------------------------------------
allhotels <- getnetwork(networkfamily(hotel.fit))
plot(allhotels)


###############################################################################
# Version : 1.0
# Project Title : Bayesian Networks (Manor dataset)
# Author: Bharat.Upadrasta
###############################################################################

rm(list = ls())

#-----------------------------------------------
# Library Calls
#-----------------------------------------------
library(deal)
library(bnlearn)
library(randomForest)
library(car)
library(rpart)
library(caret)

#------------------------------------------------
# Function Calls
#------------------------------------------------

#------------------------------------------------
# Function to normalize data
#------------------------------------------------
normal = function(object)
{
	val <- (object - mean(object))/sd(object)
	return(val)
}	

#---------------------------------------------
# Function to normalize a data frame
#---------------------------------------------
getData = function(data.frame)
{	
	for(j in 1:ncol(data.frame))
	{
		a <- colnames(data.frame)
		if(class(data.frame[,j]) == "factor")
		{
			data.frame[,j] = data.frame[,j]
		}	
		else if(class(data.frame[,j]) == "str")
		{
			data.frame[,j] = data.frame[,j]
		}
		else if(a[j] == "obs")
		{
			data.frame[,j] = data.frame[,j]
		}
		else
		{
			data.frame[,j] = normal(data.frame[,j])
		}
	}
	return(data.frame)
}	

#--------------------------------------------------
# Data Call
#--------------------------------------------------
homeData <- read.csv(file="Tooldemo.csv",head=TRUE,sep=",")
summary(homeData)
str(homeData)

#------------------------------------
# Training and Test Data
#------------------------------------
set.seed(100)
train.pct <- 0.8
index.train <- sort(sample(1:nrow(homeData), train.pct*nrow(homeData))) 
index.test <- sort((1:nrow(homeData))[!(1:nrow(homeData) %in% index.train)]) 

home.train <- homeData[index.train,] 
home.test <- homeData[index.test,]

#------------------------------------
# Variable Importance
#------------------------------------

# Standardizing the training and test sets
std.homeTrain <- getData(home.train)
std.homeTest <- getData(home.test)

timeVar <- which(colnames(std.homeTrain) == 'week_start_date')
catVar <- which(colnames(std.homeTrain) == 'market')

std.homeTrain <- std.homeTrain[,-c(timeVar, catVar)]

# Random Forest Check
rfFit <- randomForest(sales_per_store ~ .,data=std.homeTrain, ntree=50, norm.votes=FALSE, importance=TRUE, do.trace=10)
plot(rfFit)
round(importance(rfFit), 2)
varImpPlot(rfFit,sort=TRUE,n.var=15)

# Selecting the best variables (non standardized form)
new.homeTrain <- subset(home.train,select=c("sales_per_store","coupon_per_store","direct_mail_per_store","tv_trps_per_store","google_impressions_per_store","unemp_rate","income","avg_temp","precipitation"))

#------------------------------------------------------
# Bayesian Network
#------------------------------------------------------

#Plot an initial fit of the network
fit <- network(new.homeTrain)
plot(fit)

#Plot the network using the grow-shrink (Markov Blanket)
#algorithm
fit1 <- gs(new.homeTrain,optimized = TRUE, debug = TRUE)
plot(fit1)

#Plot using greedy search (hill climbing algorithm)
fit2 <- hc(new.homeTrain,optimized = TRUE,debug = TRUE)
plot(fit2)

# Plot using tabu search
fit3 <- tabu(new.homeTrain,optimized = TRUE,debug = TRUE)
plot(fit3)

# The network fits are different!
compare(fit1,fit2,debug=TRUE)
compare(fit2, fit3,debug=TRUE)

# Test the strength of the arcs between nodes
arc.strength(fit2, new.homeTrain,criterion="bootstrap")
arc.strength(fit3, new.homeTrain,criterion="bootstrap")

# Bootstrapping Check
bn.boot(data = new.homeTrain, R = 2, m = 500, algorithm = "gs",
		statistic = arcs)

# Cross Validation of the network
bn.cv(new.homeTrain,'hc')

# Fit the different parameters of the network
paramFit <- bn.fit(fit2, new.homeTrain)

# Plot the fitted paramter network
#----------------------------------------------
# QQ plot
bn.fit.qqplot(paramFit, xlab = "Theoretical Quantiles",
		ylab = "Sample Quantiles", main = "Normal Q-Q Plot")

# Histogram
bn.fit.histogram(paramFit, density = TRUE, xlab = "Residuals",
		ylab = "density",
		main = "Histogram of the residuals")

# XY Plot (Scatter Plot)

bn.fit.xyplot(paramFit, xlab = "Fitted values",
		ylab = "Residuals", main = "Residuals vs Fitted")

# Measure the variability of the network structure
v <- bn.moments(new.homeTrain, algorithm = "gs", R = 30)
bn.var(v, method = "tvar")
bn.var.test(v, method = "nvar")

# Get the moral graph for the bayesian network
moral(fit1)
moral(fit2)
moral(fit3)

# Finally, get the score of the network
score(fit2,new.homeTrain)
score(fit3,new.homeTrain)

# It is seen that fit3 or the Tabu search gives the
# best network structure
plot(fit3)

