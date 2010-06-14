# Author: zubin
###############################################################################

#discuss IDE, CRAN, Libraries, ls 
#use R data sets 
data(iris)
# show libraries, dataframe, str, head, summary, help?
hist(iris)

#show rename and delete, new fields
a <- iris
str(a)
a$Species <- NULL
str(a)
a$zoo <- a$Sepal.Length * 10
names(a)[names(a)=="Sepal.Width"] <- "Sepal.Zoo"
#subset
b <- subset(a,Sepal.Width > 3)

summary(iris)
library(Hmisc)
describe(iris)

cor(iris)

library(corrgram)
corrgram(iris[,1:4],cex.labels=2,order=TRUE,lower.panel=panel.pie,upper.panel=panel.shade, text.panel=panel.txt)

pairs(iris,panel=panel.smooth)
pairs(iris[1:4],pch=21,panel=panel.smooth,bg=c("red","green","blue")[unclass(iris$Species)])

library(lattice)
xyplot(Sepal.Width ~ Sepal.Length | Species, data=iris, cex=.5)
xyplot(Sepal.Width ~ Sepal.Length | Species, data=iris,   panel = function(x, y) {
			panel.grid(v=2)
			panel.xyplot(x, y)
			panel.loess(x, y, span = 1.0, degree = 1, family="symmetric")
			panel.abline(lm(y~x)) 
		})
bwplot(Sepal.Width ~ Sepal.Length | Species, data=iris)
densityplot(Sepal.Width ~ Sepal.Length | Species, data=iris)
dotplot(Sepal.Width ~ Sepal.Length | Species, data=iris)
histogram(Sepal.Width ~ Sepal.Length | Species, data=iris)
parallel(~iris[1:4] | Species, data=iris,layout=c(1,3))

library(hexbin)
bin<-hexbin(iris[,1],iris[,2],xbins=15)
plot(bin, main="Hexagonal Binning")

data(volcano)
wireframe(volcano, shade = TRUE,aspect = c(61/87, 0.4), light.source = c(10,0,10))

#survey data ######################
# load via SQL

# load via csv 


gsts_raw <-read.csv(file="gsts_avgs.txt")
head(gsts_raw)
str(gsts_raw)

boxplot(gsts_raw[-1],main="Quality Scores", col="blue", cex.axis=.3)

cor(gsts_raw)
library(corrgram)
corrgram(gsts_raw[,-1],cex.labels=2,order=TRUE,lower.panel=panel.pie,upper.panel=panel.shade, text.panel=panel.txt)

flhs <-factanal((gsts_raw[,2:5]), factors=1, data=gsts_raw,lower=.020,scores='regression')
frhs <-factanal((gsts_raw[,6:38]), factors=3, data=gsts_raw,lower=.025,scores='regression')

gsts <- data.frame(flhs$scores,frhs$scores)
str(gsts)
names(gsts)<- c('sat','physical','fb','staff')

#explore new data set
head(gsts)
str(gsts)
cor(gsts)
pairs(gsts,panel=panel.smooth)
boxplot(gsts)
summary(gsts)
par(mfrow=c(4,1))
for(i in 1:4)hist(gsts[,i],main=paste("Histogram",i),col="RED")

#create a class variable from SAT and append to GSTS3
library(gtools)
qsat = quantcut(gsts$sat)
gstsCut <- data.frame(gsts,qsat)

# explore with conditioning plots
library(lattice)
xyplot(sat ~ physical | qsat, data=gstsCut,type=c('p', 'smooth'), col="red")

#create a new variable - random uniform
gsts <- transform(gsts,random=runif(nrow(gsts)))

#create training and validation data set
gstsTrain <- subset(gsts,random<=.8)
gstsValid <- subset(gsts,random>.8)


#fit OLS,GAM,NNET, RANDOMFOREST, MARS and CART model to training set

lm1 <- lm(sat ~ physical + fb + staff, data=gstsTrain)
plot(lm1)
plot(predict(lm1),gstsTrain$sat, col="blue")

#fit robust regression
library(MASS)
rlm1 <- rlm(sat ~ physical + fb + staff, data=gstsTrain)
summary(rlm1)

# run a stepwise and show what variables are added in order
slm1 <- step(lm1)
summary(slm1)
slm1$anova

library(mgcv)
gam1 <- gam(sat ~ s(physical) + s(fb) + s(staff), data=gstsTrain)
summary(gam1)
plot(gam1)
vis.gam(gam1)
gam.check(gam1)

library(randomForest)
rf1 <- randomForest(sat ~ physical + fb + staff, data=gstsTrain, ntree=50)
round(importance(rf1), 2)
plot(rf1)

library(nnet)
nnet1 <- nnet(sat ~ physical + fb + staff, data=gstsTrain, size=4,linout=T)

library(rpart)
tree1 <- rpart(sat ~ physical + fb + staff, data=gstsTrain,method='anova')
printcp(tree1)
plot(tree1, uniform=FALSE)
text(tree1,digits=3, cex=.7)

library(party)
ptree <- ctree(sat ~ physical + fb + staff, controls=ctree_control(minbucket=50), data=gstsTrain)
plot(ptree, main="Survey Results", type="simple")

#predict over validation set and append variables to dataframe
pols <- predict(lm1,newdata=gstsValid,interval='prediction')
gstsValid <- data.frame(gstsValid,pols)

pgam <- predict(gam1,newdata=gstsValid,interval='prediction')
gstsValid <- data.frame(gstsValid,pgam)

prf <- predict(rf1, newdata=gstsValid)
gstsValid <- data.frame(gstsValid,prf)

pnn <- predict(nnet1,newdata=gstsValid)
gstsValid <- data.frame(gstsValid,pnn)

ptree <- predict(tree1,newdata=gstsValid)
gstsValid <- data.frame(gstsValid,ptree)



#calculate MAD and plot fits of validation set for OLS
par(mfrow=c(3,2))
with (gstsValid,plot(fit,sat))
with (gstsValid,lines(lowess(fit,sat),col='red'))
with (gstsValid,lines(fit,lwr, lty=1, lwd=1, col='grey'))
with (gstsValid,lines(fit,upr, lty=1, lwd=1, col='grey'))
mae <- data.frame("ols" = mean(abs(gstsValid$sat-gstsValid$fit)))
#row.names(mae)[1] <- "ols"

#calculate MAD and plot fits of validation set for GAM
with (gstsValid,plot(pgam,sat))
with (gstsValid,lines(lowess(pgam,sat),col='red'))
mae <- cbind(mae,"gam" = mean(abs(gstsValid$sat-gstsValid$pgam)))

#calculate MAD and plot fits of validation set for RF
with (gstsValid,plot(prf,sat))
with (gstsValid,lines(lowess(prf,sat),col='red'))
mae <- cbind(mae,"RF" = mean(abs(gstsValid$sat-gstsValid$prf)))

#calculate MAD and plot fits of validation set for NNET
with (gstsValid,plot(pnn,sat))
with (gstsValid,lines(lowess(pnn,sat),col='red'))
mae <- cbind(mae,"NNET" = mean(abs(gstsValid$sat-gstsValid$pnn)))

#calculate MAD and plot fits of validation set for Tree
with (gstsValid,plot(ptree,sat))
with (gstsValid,lines(lowess(ptree,sat),col='red'))
mae <- cbind(mae,"TREE" = mean(abs(gstsValid$sat-gstsValid$ptree)))

barplot(as.matrix(mae),col="blue")
title("Summary of Fits on Validation Sample", outer=TRUE) 
