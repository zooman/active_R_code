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
