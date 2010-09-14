# 
# Author: zubin
###############################################################################

options(prompt="R> ", digits=4)
options(scipen=3)

# regression techniques
#library(Hmisc)
#hist(hotelv4)

#linear regression
hotel_raw <-read.csv(file="hi.csv")
hotelv1 <- hotel_raw[,-1]

lm_model <- lm (Occupancy ~ ., data=hotelv1)
plot(lm_model)

hotelv2 <- hotelv1[-829,]
lm_model <- lm (Occupancy ~ ., data=hotelv2)
plot(lm_model)

hotelv3 <- hotelv2[-357,]
lm_model <- lm (Occupancy ~ ., data=hotelv3)
plot(lm_model)

#final data set
hotelv4 <- subset(hotelv3,select = -c(web_nts_totsty,PercentLeisure,PercentTransientNights))

#plot by mgmt type
library(lattice)
xyplot(Occupancy ~ Compet_Occupancy |MGMT_TYP_DESC, data=hotelv4,type=c('p', 'smooth'), col="red")

#create a new variable - random uniform
hotelv4 <- transform(hotelv4,random=runif(nrow(hotelv4)))

#create training and validation data set
hotelv4Train <- subset(hotelv4,random<=.8)
hotelv4Valid <- subset(hotelv4,random>.8)

#fit linear regression
lm_model <- lm (Occupancy ~ ., data=hotelv4Train)
plot(lm_model)

#fit robust regression
library(MASS)
rlm1 <- rlm(Occupancy ~., data=hotelv4Train)
summary(rlm1)

#fit quantile regression
library(quantreg)
qm1 <- rq(Occupancy ~.,tau=.5, data=hotelv4Train)

#fit PLS
library(pls)
plsm1 <- mvr(Occupancy ~., 20,data=hotelv4Train, validation="CV")
coef(plsm1)
loadings(plsm1)
plot(RMSEP(plsm1), legendpos = "topright")
plot(plsm1, "loadings", comps = 1:3,legendpos = "topleft")
summary(plsm1)

#random forest
library(randomForest)
rf1 <- randomForest(Occupancy ~ ., data=hotelv4Train, ntree=50, importance=TRUE)
plot(rf1)
varImpPlot(rf1)

#run regression tree
library(party)
ptree1 <- ctree(Occupancy ~ ., controls=ctree_control(minbucket=30), data=hotelv4Train)
plot(ptree1, main="Hotel Tree", type="simple")

#run tree
library(rpart)
tree1 <- rpart(Occupancy ~ ., data=hotelv4Train,method='anova',cp=.005)
printcp(tree1)
plot(tree1, uniform=FALSE)
text(tree1,digits=3, cex=.7)

#neural net
library(nnet)
nnet1 <- nnet(Occupancy ~ ., data=hotelv4Train, rang=.1,size=10,maxit=1000,linout=T)

#run GAM
library(mgcv)
gam1 <- gam(Occupancy ~ s(Compet_Occupancy) + s(PercentGroupNights)+ s(Compet_AvgDailyRate) + LOC_DESC, data=hotelv4Train)
summary(gam1)
plot(gam1)
vis.gam(gam1)
gam.check(gam1)

#predict validation data set
#predict over validation set and append variables to dataframe
pols <- data.frame(predict(lm_model,newdata=hotelv4Valid,interval='prediction'))
hValid <- data.frame(pols)

pgam <- predict(gam1,newdata=hotelv4Valid,interval='prediction')
hValid <- data.frame(hValid,pgam)

prf <- predict(rf1, newdata=hotelv4Valid)
hValid <- data.frame(hValid,prf)

pnn <- predict(nnet1,newdata=hotelv4Valid, type="raw")
hValid <- data.frame(hValid,pnn)

ptree <- predict(tree1,newdata=hotelv4Valid)
hValid <- data.frame(hValid,ptree)

ptree2 <- data.frame(predict(ptree1,newdata=hotelv4Valid))
names(ptree2) <- ("ptree2")
hValid <- data.frame(hValid,ptree2)

ppls <- data.frame(predict(plsm1,newdata=hotelv4Valid,ncomp=12))
names(ppls) <- "ppls"
hValid <- data.frame(hValid,ppls)

qreg <- predict(qm1,newdata=hotelv4Valid)
hValid <- data.frame(hValid,qreg)

prlm <- predict(rlm1,newdata=hotelv4Valid)
hValid <- data.frame(hValid,prlm)


#calculate MAD and plot fits of validation set for OLS
par(mfrow=c(3,3))

with (hotelv4Valid,plot(hValid$fit,Occupancy))
with (hotelv4Valid,lines(lowess(hValid$fit,Occupancy),col='red'))
mad <- data.frame("ols" = mean(abs(hotelv4Valid$Occupancy-hValid$fit)))

#calculate MAD and plot fits of validation set for GAM
with (hotelv4Valid,plot(hValid$pgam,Occupancy))
with (hotelv4Valid,lines(lowess(hValid$pgam,Occupancy),col='red'))
mad <- cbind(mad,"gam" = mean(abs(hotelv4Valid$Occupancy-hValid$pgam)))

#calculate MAD and plot fits of validation set for RF
with (hotelv4Valid,plot(hValid$prf,Occupancy))
with (hotelv4Valid,lines(lowess(hValid$prf,Occupancy),col='red'))
mad <- cbind(mad,"RF" = mean(abs(hotelv4Valid$Occupancy-hValid$prf)))

#calculate MAD and plot fits of validation set for NNET
with (hotelv4Valid,plot(hValid$pnn,Occupancy))
with (hotelv4Valid,lines(lowess(hValid$pnn,Occupancy),col='red'))
mad <- cbind(mad,"NNET" = mean(abs(hotelv4Valid$Occupancy-hValid$pnn)))

#calculate MAD and plot fits of validation set for Tree
with (hotelv4Valid,plot(hValid$ptree,Occupancy))
with (hotelv4Valid,lines(lowess(hValid$ptree,Occupancy),col='red'))
mad <- cbind(mad,"TREE" = mean(abs(hotelv4Valid$Occupancy-hValid$ptree)))

#calculate MAD and plot fits of validation set for Tree2
with (hotelv4Valid,plot(hValid$ptree2,Occupancy))
with (hotelv4Valid,lines(lowess(hValid$ptree2,Occupancy),col='red'))
mad <- cbind(mad,"TREE2" = mean(abs(hotelv4Valid$Occupancy-hValid$ptree2)))

#calculate MAD and plot fits of validation set for PLS
with (hotelv4Valid,plot(hValid$ppls,Occupancy))
with (hotelv4Valid,lines(lowess(hValid$ppls,Occupancy),col='red'))
mad <- cbind(mad,"PLS" = mean(abs(hotelv4Valid$Occupancy-hValid$ppls)))

#calculate MAD and plot fits of validation set for RREG
with (hotelv4Valid,plot(hValid$prlm,Occupancy))
with (hotelv4Valid,lines(lowess(hValid$prlm,Occupancy),col='red'))
mad <- cbind(mad,"RREG" = mean(abs(hotelv4Valid$Occupancy-hValid$prlm)))

#calculate MAD and plot fits of validation set for QREG
with (hotelv4Valid,plot(hValid$qreg,Occupancy))
with (hotelv4Valid,lines(lowess(hValid$qreg,Occupancy),col='red'))
mad <- cbind(mad,"QREG" = mean(abs(hotelv4Valid$Occupancy-hValid$qreg)))

#plot MAPE
order <- order(colMeans(mad),decreasing = FALSE)
sorted <- mad[1,order]
barplot((as.matrix(sorted)),col="blue")
title("Summary of MAD")
title("Summary of Fits on Validation Sample", outer=TRUE, line=-1) 



