# Author: zubin
###############################################################################

options(prompt="R> ", digits=4)
options(scipen=3)

# regression techniques
#library(Hmisc)
#hist(hotelv4)

#linear regression
#SAS PROC REG
hotel_raw <-read.csv(file="hi.csv")

hotelv1 <- hotel_raw[,-1]

lm_model <- lm (Occupancy ~ ., data=hotelv1)
#plot(lm_model)

hotelv2 <- hotelv1[-829,]
lm_model <- lm (Occupancy ~ ., data=hotelv2)
#plot(lm_model)

hotelv3 <- hotelv2[-357,]
lm_model <- lm (Occupancy ~ ., data=hotelv3)
#plot(lm_model)

#final data set
hotelv4 <- subset(hotelv3,select = -c(web_nts_totsty,PercentLeisure,PercentTransientNights))

#plot by mgmt type
library(lattice)
#xyplot(Occupancy ~ Compet_Occupancy |MGMT_TYP_DESC, data=hotelv4,type=c('p', 'smooth'), col="red")

#create a new variable - random uniform
hotelv4 <- transform(hotelv4,random=runif(nrow(hotelv4)))

#create training and validation data set
hotelv4Train <- subset(hotelv4,random<=.8)
hotelv4Valid <- subset(hotelv4,random>.8)

#####################################################################################
#####################################################################################
#start regression methods

#fit linear regression
#SAS PROC REG
lm_model <- lm (Occupancy ~ ., data=hotelv4Train)
#plot(lm_model)

#fit robust regression
#SAS RobustReg, useful for outliers
library(MASS)
rlm1 <- rlm(Occupancy ~., maxit=100,data=hotelv4Train)
summary(rlm1)

#fit quantile regression
#SAS QUANTREG, useful for outliers
library(quantreg)
qm1 <- rq(Occupancy ~.,tau=.5, data=hotelv4Train)

#fit PLS
# SAS PROC PLS, useful for collinearity
hotelv4Train <- subset(hotelv4Train,select = -c(random ))
library(pls)
plsm1 <- mvr(Occupancy ~., 20,data=hotelv4Train, validation="CV")
coef(plsm1)
#loadings(plsm1)
#plot(RMSEP(plsm1), legendpos = "topright")
#plot(plsm1, "loadings", comps = 1:3,legendpos = "topleft")
summary(plsm1)

#random forest, useful for prediction and variable importance
library(randomForest)
rf1 <- randomForest(Occupancy ~ ., data=hotelv4Train, ntree=50, importance=TRUE)
#plot(rf1)
#varImpPlot(rf1)

#run regression tree, prediction and importance
library(party)
ptree1 <- ctree(Occupancy ~ ., controls=ctree_control(minbucket=30), data=hotelv4Train)
#plot(ptree1, main="Hotel Tree", type="simple")

#run tree CART algorithm
library(rpart)
tree1 <- rpart(Occupancy ~ ., data=hotelv4Train,method='anova',cp=.005)
#printcp(tree1)
#plot(tree1, uniform=FALSE)
#text(tree1,digits=3, cex=.7)


#neural net, prediction
library(nnet)
nnet1 <- nnet(Occupancy ~ ., data=hotelv4Train,size=7,maxit=2000,decay = .001,linout=F)

#support vector machine, prediction
library(e1071)
svm1 <- svm(Occupancy ~., data=hotelv4Train, type='eps')

#run GAM
#SAS PROC GAM, functional form / non-linearities
library(mgcv)
gam1 <- gam(Occupancy ~ s(Compet_Occupancy) + s(AvgDailyRate,Compet_AvgDailyRate) + s(PercentGovtNights) + slf_nts_totsty + RMS_AVAIL_QTY + LOC_DESC + PercentGroupNights, data=hotelv4Train)
summary(gam1)
#plot(gam1)
#vis.gam(gam1)
#gam.check(gam1)




#variable importance and classification
#SAS PROC GLMSELECT
#library(caret)
#ctrl <- rfeControl(functions = rfFuncs, method = "cv",workers=2,verbose = FALSE,returnResamp = "final")
#y <- hotelv4Train$Occupancy
#x <- subset(hotelv4Train,select = -c(Occupancy))
#subsets <- c(1:19)
#lmProfile <- rfe(x, y,sizes = subsets,rfeControl = ctrl)
##lmProfile
##plot(lmProfile, metric = "Rsquared",type="b")
#predictors(lmProfile)


#R squared decomposition
#run variance decomposition
lm_model <- lm (Occupancy ~ Compet_Occupancy + PercentGroupNights + LOC_DESC + AvgDailyRate + PercentBusiness + slf_nts_totsty, data=hotelv4Train)
library(relaimpo)
a <- calc.relimp(lm_model,type = c("lmg","last", "first"), rela = TRUE)
#a <- calc.relimp(ols,type = c("lmg", "pmvd", "last", "first", "betasq", "pratt"), rela = TRUE)
#plot(a)



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

psvm1 <- predict(svm1,newdata=hotelv4Valid)
hValid <- data.frame(hValid,psvm1)


#calculate MAD and plot fits of validation set for OLS
par(mfrow=c(2,5))

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

#calculate MAD and plot fits of validation set for SVM
with (hotelv4Valid,plot(hValid$psvm1,Occupancy))
with (hotelv4Valid,lines(lowess(hValid$psvm1,Occupancy),col='red'))
mad <- cbind(mad,"SVM" = mean(abs(hotelv4Valid$Occupancy-hValid$psvm1)))

#plot MAPE
order <- order(colMeans(mad),decreasing = FALSE)
sorted <- mad[1,order]
#barplot((as.matrix(sorted)),col="blue")
#title("Summary of MAD")
#title("Summary of Fits on Validation Sample", outer=TRUE, line=-1) 


#LME Mixed Models for Panel Data
#PROC MIXED





