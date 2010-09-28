# Author: zubin
###############################################################################

options(prompt="R> ", digits=4)
options(scipen=3)

# regression techniques
library(Hmisc)
#hist(hotelv4)

#####Input some data from .csv
raw <-read.csv(file="export.txt")
raw$SUCCESS_B <- NULL
raw$IDS_BOOK_PCT <- NULL
raw <- raw[,-1]
raw$REGISTRATION_B <- as.factor(raw$REGISTRATION_B)
hist(raw)

#create a new variable - random uniform
hotelv4 <- transform(raw,random=runif(nrow(raw)))

#create training and validation data set
hotelv4Train <- subset(hotelv4,random<=.8)
hotelv4Valid <- subset(hotelv4,random>.8)

hotelv4Train <- subset(hotelv4Train, select = -c(random))
hotelv4Valid <- subset(hotelv4Valid, select = -c(random))

################################################
#start Classification methods

#Logistic Regression
#SAS PROC LOGISTIC
library(Design)
llogit <- lrm(REGISTRATION_B ~ ., data=raw)

#random forest
library(randomForest)
rf1 <- randomForest(factor(REGISTRATION_B) ~ ., data=hotelv4Train, ntree=50, importance=TRUE)
plot(rf1)
varImpPlot(rf1)

#run regression tree
library(party)
ptree1 <- ctree(REGISTRATION_B ~ ., controls=ctree_control(minbucket=100), data=hotelv4Train)
plot(ptree1, main="Loyalty Tree", type="simple")

#run tree CART algorithm
library(rpart)
tree1 <- rpart(REGISTRATION_B ~ ., data=hotelv4Train,method='class',cp=.005)
printcp(tree1)
plot(tree1, uniform=FALSE)
text(tree1,digits=3, cex=.7)

#neural net
library(nnet)
nnet1 <- nnet(REGISTRATION_B ~ ., data=hotelv4Train,size=15,maxit=500,linout=F)

#support vector machine
library(e1071)
svm1 <- svm(REGISTRATION_B ~., data=hotelv4Train, type='C')

#run GAM
#SAS PROC GAM
library(mgcv)
gam1 <- gam(REGISTRATION_B ~ s(STAY_C) + s(WEB_BOOK_PCT) + (CRO_BOOK_PCT)+ s(LEADTIME) + BUS_PERCENT, family=binomial,data=hotelv4Train)
summary(gam1)
plot(gam1)


##KNN
y <- hotelv4Train$REGISTRATION_B
#x <- subset(hotelv4Train,select = -c(REGISTRATION_B))
#knn.pred2 <- knn.cv(hotelv4Train,y, k=10)

#lvq
lvq.init <- lvqinit(hotelv4Train,y)
lvqtest(lvq.init,hotelv4Train)
lvq.pred2 <- olvq1(hotelv4Train,y,lvq.init)



#variable importance and classification
#SAS PROC GLMSELECT
library(caret)
ctrl <- rfeControl(functions = rfFuncs, method = "cv",workers=2,verbose = FALSE,returnResamp = "final")
y <- hotelv4Train$REGISTRATION_B
x <- subset(hotelv4Train,select = -c(REGISTRATION_B))
subsets <- c(1:10)
lmProfile <- rfe(x, y,sizes = subsets,rfeControl = ctrl)
lmProfile
plot(lmProfile, metric = "Rsquared",type="b")
predictors(lmProfile)


#predict validation data set
#predict over validation set and append variables to dataframe
rf.pred2 <- data.frame(predict(rf1,newdata=hotelv4Valid,type="prob")[,2])
names(rf.pred2) <- 'rf.pred2'

logit.pred2 <- data.frame(predict(llogit,newdata=hotelv4Valid, type="fitted"))
names(logit.pred2) <- 'logit.pred2'

nnet.pred2 <- data.frame(predict(nnet1,newdata=hotelv4Valid,type="raw"))
names(nnet.pred2) <- 'nnet.pred2'

gam.pred2 <- data.frame(predict(gam1,newdata=hotelv4Valid,type="response"))
names(gam.pred2) <- 'gam.pred2'

ptree.pred2 <- data.frame(predict(ptree1,newdata=hotelv4Valid))
names(ptree.pred2) <- 'ptree.pred2'

svm.pred2 <- data.frame(predict(svm1,newdata=hotelv4Valid))
names(svm.pred2) <- 'svm.pred2'

lvq.pred2 <- lvqtest(lvq.pred2,hotelv4Valid)

library(ROCR)
plot(performance(prediction(rf.pred2,hotelv4Valid$REGISTRATION_B),"tpr","fpr"),col="red")
plot(performance(prediction(logit.pred2,hotelv4Valid$REGISTRATION_B),"tpr","fpr"),col="blue",add=TRUE)
plot(performance(prediction(nnet.pred2,hotelv4Valid$REGISTRATION_B),"tpr","fpr"),col="purple",add=TRUE)
plot(performance(prediction(gam.pred2,hotelv4Valid$REGISTRATION_B),"tpr","fpr"),col="green",add=TRUE)


auc1 <- performance(prediction(rf.pred2,hotelv4Valid$REGISTRATION_B),"auc")@y.values[[1]]
auc2 <- performance(prediction(logit.pred2,hotelv4Valid$REGISTRATION_B),"auc")@y.values[[1]]
auc3 <- performance(prediction(gam.pred2,hotelv4Valid$REGISTRATION_B),"auc")@y.values[[1]]
auc4 <- performance(prediction(nnet.pred2,hotelv4Valid$REGISTRATION_B),"auc")@y.values[[1]]
legend("bottomright",legend=c(paste("Random Forest         (AUC=",formatC(auc1,digits=4,format="f"),")",sep=""),
				paste("Logistic Regression  (AUC=",formatC(auc2,digits=4,format="f"),")",sep=""),
				paste("GAM                          (AUC=",formatC(auc3,digits=4,format="f"),")",sep=""),
				paste("Neural Net                 (AUC=",formatC(auc4,digits=4,format="f"),")",sep="")),
				col=c("red","blue","green","purple"), lty=1)



library(gmodels)

svm.predict=as.numeric(as.character(svm.pred2$svm.pred2))
CrossTable(svm.predict,hotelv4Valid$REGISTRATION_B,format="SAS",prop.r="FALSE",prop.c="FALSE",prop.chisq="FALSE")

CrossTable(lvq.pred2,hotelv4Valid$REGISTRATION_B,format="SAS",prop.r="FALSE",prop.c="FALSE",prop.chisq="FALSE")

ptree.predict=as.numeric(as.character(ptree.pred2$ptree.pred2))
CrossTable(ptree.predict,hotelv4Valid$REGISTRATION_B,format="SAS",prop.r="FALSE",prop.c="FALSE",prop.chisq="FALSE")



		

