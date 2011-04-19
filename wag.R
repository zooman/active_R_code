# TODO: Add comment
# 
# Author: Zubin
###############################################################################


options(prompt="R> ", digits=4)
options(scipen=3)

raw <-read.csv(file="wag.csv")
str(raw,list.len=1000)

raw$oldweights <- NULL
raw$WEIGHTS <- NULL
raw$Sampsource <- NULL
raw$Q2_1 <- NULL
raw$unique_id <- NULL

raw$PredictedClass <- as.factor(raw$PredictedClass)

raw$state <- NULL
raw$recency_dt_yyyymm <- NULL
raw$last_name_soundx <- NULL
raw$first_name_soundx <- NULL
raw$county_fips_cd <- NULL
raw$dob.1 <- NULL
raw$HSTATE <- NULL
raw$dpv_cd <- NULL


#create a new variable - random uniform
raw <- transform(raw,random=runif(nrow(raw)))
#create training and validation data set
raw.train <- subset(raw,random <=.8, select = -c(random))
raw.valid <- subset(raw,random >.8, select = -c(random))

library(randomForest)
rf1 <- randomForest(PredictedClass ~ ., data=raw.train, ntree=50, importance=TRUE)
plot(rf1, legend=TRUE)
legend("bottomleft","x,y")
varImpPlot(rf1)
impRF <- importance(rf1,type=1)

#run tree CART algorithm
library(rpart)
tree1 <- rpart(PredictedClass ~ ., data=raw.train,method='class',cp=.005,xval=30)
printcp(tree1)
plot(tree1, uniform=FALSE)
text(tree1,digits=3, cex=.5)

printcp(tree1)
plotcp(tree1)
summary(tree1)

#pruneTree <- prune(tree1, cp=tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"])
#plot(pruneTree, uniform=FALSE, main = "Pruned")
#text(pruneTree,digits=3, cex=.5)

#par(mfrow=c(2,2))
#rsq.rpart(tree1)
#rsq.rpart(pruneTree)

#run regression tree
#library(party)
#ptree1 <- ctree(PredictedClass ~ ., controls=ctree_control(mincriterion=.90), data=raw.train)
#plot(ptree1, main="WAG Chronic Tree", type="simple")

#support vector machine
#library(e1071)
#svm1 <- svm(PredictedClass ~ ., data=raw.train, type='C')

#library(nnet)
#mlogit1 <- multinom(PredictedClass ~ ., data=raw.train,MaxNWts = 5000, maxit=500)
#nnet1 <- nnet(PredictedClass ~ ., data=raw.train,size=10,maxit=500,linout=F,MaxNWts=7000)

#library(caret)
#ctrl <- rfeControl(functions = rfFuncs, method = "cv",workers=2,verbose = FALSE,returnResamp = "final")
#y <- raw$PredictedClass
#x <- subset(raw,select = -c(PredictedClass))
#subsets <- c(1:3)
#lmProfile <- rfe(x, y,sizes = subsets,rfeControl = ctrl)
#lmProfile
#plot(lmProfile, metric = "Rsquared",type="b")
#predictors(lmProfile)


library(gmodels)
forest.predict <- as.numeric(as.character(predict(rf1,newdata=raw.valid),type="class"))
#svm.predict <- as.numeric(as.character(predict(svm1,newdata=raw.valid, type='class')))
tree.predict <- as.numeric(as.character(predict(tree1,newdata=raw.valid,type="class")))
#prunetree.predict <- as.numeric(as.character(predict(pruneTree,newdata=raw.valid,type="class")))

#CrossTable(svm.predict,raw.valid$PredictedClass,format="SPSS",prop.r="TRUE",prop.c="TRUE",prop.chisq="FALSE")
CrossTable(tree.predict,raw.valid$PredictedClass,format="SPSS",prop.r="TRUE",prop.c="TRUE",prop.chisq="FALSE")
CrossTable(forest.predict,raw.valid$PredictedClass,format="SPSS",prop.r="TRUE",prop.c="TRUE",prop.chisq="FALSE",mcnemar="TRUE")
#CrossTable(prunetree.predict,raw.valid$PredictedClass,format="SPSS",prop.r="TRUE",prop.c="TRUE",prop.chisq="FALSE")

#mlogit1 <- multinom(PredictedClass ~ TOTALCOND, data=raw,MaxNWts = 5000, maxit=500)
