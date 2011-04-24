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



#qgraph


raw <- read.csv("hi.csv")

raw$RMS_AVAIL_QTY <- NULL
raw$STRUCT_DESC <- NULL
raw$LOC_DESC <- NULL

raw$PercentTransientNights <- NULL
raw$Business <- NULL
raw$inn_nts_totsty <- NULL


library(qgraph)





