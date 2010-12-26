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