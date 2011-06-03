# TODO: Add comment
# 
# Author: zubin
###############################################################################

library(ellipse)

# Pirate Hat
x <- seq(-1.5,1.5,.01)
mix <- 0.5
y <- (1-mix)*c(dchisq(x[151:300],2),0.2373671,rev(dchisq(x[151:300],2))) + mix*exp(-pi*x^2)

plot(seq(-3,3,.01),exp(-pi*seq(-3,3,.01)^2),type="n",ylim=c(-2,1), xlab = NA, ylab = NA) #, axes = FALSE)
lines(ellipse(matrix(c(1/15,0,0,1/20),ncol=2),centre=c(0,-.05)),type="l")
polygon(c(-1.5,x,1.5),c(.18,y,.18),col="black")
text(x = 0, y = 0.35, labels = expression(pi), cex = 2.5, col = "white")

library(rgl)
example(surface3d)
example(plot3d)

demo(persp)

#generate torus data
torus1<-function(R = 5, r = 2, xc = 0, yc = 0, zc = 0, n = 5000) {
# R is the radius from the center of the torus to the center of the ring
# r is the radius of the ring
# xc, yc, and zc ar th x, y, and z center of the torus
	u<-seq(0,2*pi,len=n)
	v<-runif(n,min=0,max=2*pi)
	x<-(R+r*cos(v))*cos(u)+xc
	y<-(R+r*cos(v))*sin(u)+yc
	z<-r*sin(v)+zc
	data<-data.frame(x=x,y=y,z=z)
}

zdata3 <- torus1()

#plot data
plot3d(zdata3)

library(reshape)
#standardize data
#range: scale to [0, 1] 
#rank: convert values to ranks 
#robust: robust version of sd, substract median and divide by median absolute deviation 
#sd: subtract mean and divide by standard deviation 
scaledz <- rescaler.data.frame(zdata3,type="sd")
plot3d(scaledz)



circle <- function(center.x=0,center.y=0,r=1){
	t <- seq(0,2*pi,pi/32)
	x<-center.x + r * cos(t)
	y<-center.y + r * sin(t)
	cbind(x,y)
}

par(mar=c(0, 0, 0, 0),bg="black")
plot(0,0,asp=1,xlim=c(-2,2),ylim=c(-2,2), type="n", xlab = NA, ylab = NA, ann=FALSE, axes=FALSE)

# Bones
polygon(circle(1.5,1.125,3/16),col="white",border=NA)
polygon(circle(1.125,1.5,3/16),col="white",border=NA)



#----------------qgraph-----------------------
raw <- read.csv("hi.csv")

raw$STRUC_DESC <- NULL
raw$LOC_DESC <- NULL
raw$MGMT_TYP_DESC <- NULL

raw$PercentTransientNights <- NULL
raw$PercentBusiness <- NULL
raw$inn_nts_totsty <- NULL
raw$FAC_ID <- NULL

library(qgraph)
y <- cor(raw)
ynames <- dimnames(y)[1]
toolnames <- unlist(ynames)

qgraph(y)

qgraph(y,minimum=.3,directed=FALSE,details=TRUE,edge.labels=TRUE,edge.label.cex=.6)
qgraph(y,minimum=.3,directed=TRUE,details=TRUE,edge.labels=TRUE,edge.label.cex=.6)
#title("EDA: Correlation Network",line=-1,cex.main=1)

qgraph(y,minimum=.2,mode='strength',cut=.5,directed=TRUE,layout='spring',details=TRUE,edge.labels=TRUE,edge.label.cex=.6, asize=.1,graph='association')
qgraph(y,minimum=.1,mode="strength",cut=.2,directed=TRUE,layout='spring',details=TRUE,edge.labels=TRUE,edge.label.cex=.6, asize=.1,graph='concentration')
qgraph(y,minimum=.1,directed=TRUE,layout='spring',details=TRUE,edge.labels=FALSE,edge.label.cex=.6, asize=.1,graph='factorial')


qgraph.efa(raw,5,rotation='varimax',details=TRUE, minimum=.03,layout="tree")
#par(mfrow=c(2,1))
qgraph.efa(raw,5,rotation='varimax',details=TRUE, minimum=.03,factorCors=TRUE,scores="regression",crossloadings=FALSE)
#qgraph.efa(raw,5,rotation='varimax',details=TRUE, minimum=.03,factorCors=TRUE,scores="regression",crossloadings=TRUE)

#facty <- factanal(raw,5,rotation='varimax')
#qgraph.loadings(facty$loadings,model='formative',minimum=.2,details=TRUE,crossloadings=FALSE,factorCors=FALSE)

qgraph(y,filetype='tex',tooltips=toolnames)
qgraph(y,filetype='tex',tooltips=toolnames,cut=.5,minimum=.2,directed=TRUE,layout='spring',details=TRUE,edge.labels=TRUE,edge.label.cex=.7, asize=.1,graph='association')
qgraph(y,filetype='tex',tooltips=toolnames,cut=.2,minimum=.1,directed=TRUE,layout='spring',details=TRUE,edge.labels=TRUE,edge.label.cex=.7, asize=.1,graph='concentration')

#----------------------customer data

raw <-read.csv(file="sample_customer.csv")
raw$SUCCESS_B <- NULL
raw$IDS_BOOK_PCT <- NULL
raw <- raw[,-1]
str(raw)

y <- cor(raw)
qgraph(y, directed=TRUE,details=TRUE)
qgraph(y,directed=TRUE,details=TRUE,graph='concentration')
qgraph(y,directged=TRUE,details=TRUE,graph='factorial')


#mmx data-----------------

raw <-read.csv(file="manormmx.csv")

raw$Date <- NULL
cgroups <- raw$market
raw$market <- NULL
raw$Google_Impper_store <- NULL
raw$MEDIAN_HH_SIZE <- NULL
raw$percent_NON_HISP_ASIAN_PACIFIC <- NULL
raw$percent_POP_55__YEARS_of_AGE <- NULL
raw$HHs_per_SQ_MILE  <- NULL
raw$TOT_HOUSING_UNITS_2008per_store <- NULL
raw$percent_NON_HISP_BLACK_PERSON_H <- NULL
raw$percent_NON_HISP_WHITE_PERSON_H <- NULL
raw$percent_POP_0_20_YEARS_of_AGE <- NULL
raw$SQ_MILES_of_MARKETper_store <- NULL
raw$TOTAL_HHsper_store <- NULL
raw$percent_POP_21_34_YEARS_of_AGE <- NULL
raw$percent_POP_35_44_YEARS_of_AGE <- NULL
raw$percent_POP_45_49_YEARS_of_AGE <- NULL
raw$percent_POP_50_54_YEARS_of_AGE <- NULL

y <- cor(raw)
#qgraph(cor(raw), groups=cgroups)
qgraph(y)
qgraph(y,directed=TRUE)
qgraph(y,minimum=.3,mode='strength',directed=TRUE,layout='spring',details=TRUE,edge.labels=FALSE,edge.label.cex=.6, asize=.1,graph='association')
qgraph(y,minimum=.1,mode='strength',directed=TRUE,layout='spring',details=TRUE,edge.labels=FALSE,edge.label.cex=.6, asize=.1,graph='concentration')
#qgraph(y,minimum=.2,mode='strength',directed=TRUE,layout='spring',details=TRUE,edge.labels=FALSE,edge.label.cex=.6, asize=.1,graph='factorial')

finsingular <- lm(Salesper_store ~ .,data=raw)	

fact <- factanal(raw,15,rotation="varimax",nstart=20,lower=.01)
qgraph.efa(fact,details=TRUE, minimum=.03,factorCors=TRUE,scores="regression",crossloadings=FALSE)

