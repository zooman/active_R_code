# TODO: Add comment
# 
# Author: zubin
###############################################################################
library(MASS)
library(deldir)
library(gtools)
library(seas)
library(reshape)


#process data
rawdata <- read.csv("sample_customer.csv")
registration <- rawdata$REGISTRATION_B
scaledata <- rescaler.data.frame(rawdata[,2:18],type="sd")

#see histograms
#par(ask=TRUE)
par(mfrow=c(4,5))
n <- names(rawdata[,1:18])
attach(rawdata[,1:18])
sapply(n, function(x){
			y<- get(x)
			hist(y,main=x)
		})
detach(rawdata[,1:18])

#stackdf <- stack(scaledata)
#library(lattice)
#histogram(~values|ind, data = stackdf)

#run segmentation
hvq_k <- hvq(scaledata,nclust=100,depth=1, quant.err = .5)
hvqdata_subset <- hvq_k$ztab[,1:5]
hvqdata_subset
hvqdata <- hvq_k$ztab
hvqdata <- subset(hvqdata, hvqdata$n>100)

#visualize segmentation
hvqgraph33(hvqdata, zcols = 6:22, pal = 6, asp=NA, numrec=T, bwtess=T, axes=T)

#run color
hvqgraph33(hvqdata, zcols = 6:22, pal = 6, asp=NA, numrec=T, bwtess=F, axes=T,magnif=hvqdata$n)



#mvt example
library(AlgDesign)
set.seed(1, kind = NULL); runif(1)
rm(email_layout)

email_layout <- list(
		SUBJECT=factor(c('SP1_SUBJECT_1','SP1_SUBJECT_2','SP1_SUBJECT_3')),
		SUBJTEXT= factor(c('SP1_SUBJTEXT_1','SP1_SUBJTEXT_2','SP1_SUBJTEXT_3')),
		MAINIMGCTR= factor(c('SP1_IMG_CTR_1','SP1_IMG_CTR_2','SP1_IMG_CTR_3')),
		SOME_VARIABLE= factor(c('A','B','C'))
);


##build full factorial (raw)
full_fact <- gen.factorial(sapply(email_layout, length), varNames = names(email_layout))

#apply factor level names
full_fact_n <- sapply(1:length(email_layout), function(i) { factor(full_fact[[i]], levels = sort(unique(full_fact[[i]])), labels = email_layout[[i]]) })
colnames(full_fact_n) <- colnames(full_fact)

head(full_fact_n)

#--------------------------------
#run fractional design optimal
fract_2way_ <- optFederov(~
				SUBJECT +
				SUBJTEXT +
				MAINIMGCTR +
				SOME_VARIABLE +
				SUBJECT:SUBJTEXT +
				SUBJECT:MAINIMGCTR +
				SUBJECT:SOME_VARIABLE +
				SUBJTEXT:MAINIMGCTR +
				SUBJTEXT:SOME_VARIABLE +
				MAINIMGCTR:SOME_VARIABLE
		,full_fact_n, criterion = "D",args=TRUE, center=FALSE)

## Get nTrials
fract_2way_$args.nTrials
fract_2way_$design

##obtain G-effeciency
eval.design(~
				SUBJECT +
				SUBJTEXT +
				MAINIMGCTR +
				SOME_VARIABLE +
				SUBJECT*SUBJTEXT +
				SUBJECT*MAINIMGCTR +
				SUBJECT*SOME_VARIABLE +
				SUBJTEXT*MAINIMGCTR +
				SUBJTEXT*SOME_VARIABLE +
				MAINIMGCTR*SOME_VARIABLE
		,fract_2way_$design,confounding=TRUE,variances=TRUE,center=FALSE,X=full_fact_n)




## Use Augmentation to add trials
#dat<-gen.factorial(levels=3,nVars=3,varNames=c("A","B","C"))
#desD<-optFederov(~quad(A,B,C),dat,nTrials=14,eval=TRUE)
#dat<-gen.factorial(levels=3,nVars=3,varNames=c("A","B","C"))
#desA<-optFederov(~quad(.),dat,nTrials=25,augment=TRUE,rows=desD$rows)

#write output
write.table(fract_2way_$design, file = "offersets.csv", quote= TRUE, sep=",")
#write.table(fract_2way_$design, file = "offersets.csv", append = FALSE, quote = TRUE, sep = ",",eol = "n", na = "NA", dec = ".", row.names = FALSE,col.names = TRUE, qmethod = c("escape", "double"));


#predict registration
library(randomForest)
rf1 <- randomForest( REGISTRATION_B ~ ., data=rawdata[,-1], ntree=100)
importance <- (data.frame(round(importance(rf1), 2)))

plot(rf1)
varImpPlot(rf1)

lm1 <- lm(REGISTRATION_B ~ ., data=rawdata[,-1])



