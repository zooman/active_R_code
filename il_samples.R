# Author: zdowlaty
###############################################################################


##if you want to use R to hit a database, the RJDBC package is nice

#require(RJDBC, quietly=TRUE)
#require(rJava, quietly=TRUE)
#
## makes connection to database and stores information in con variable 
#
#drv<-JDBC("org.postgresql.Driver","c:/jdbc/postgresql-8.4-701.jdbc3.jar"); 
##conn<-dbConnect(drv,"jdbc:postgresql://122.181.139.115:5432","postgres","postgres");
#conn<-dbConnect(drv,"jdbc:postgresql://localhost","postgres","coolz");
#
##submits query and returns data frame into output_data
#
#sqlstring <- paste("select * from loyalty",sep="")
#raw <-dbGetQuery(conn,sqlstring)


#####Input some data from .csv

raw <-read.csv(file="export.txt")
raw$SUCCESS_B <- NULL
raw$IDS_BOOK_PCT <- NULL
raw <- raw[,-1]

library(Hmisc)
describe(raw)
hist(raw)

cor(raw)
# corr plots
pairs(raw[,1:6],panel=panel.smooth)

plot(raw$BUS_PERCENT,raw$GDS_BOOK_PCT, main="Scatterplot Example", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(raw$BUS_PERCENT~raw$GDS_BOOK_PCT), col="red") # regression line (y~x)
lines(lowess(raw$BUS_PERCENT,raw$GDS_BOOK_PCT,f=2/3),col="blue") # lowess line (x,y)

library(hexbin)
bin<-hexbin(raw$STAY_C,raw$WEB_BOOK_PCT, xbins=50)
plot(bin, main="Hexagonal Binning")

library(corrgram)
corrgram(raw,cex.labels=.7,order=TRUE,lower.panel=panel.shade,upper.panel=panel.shade, text.panel=panel.txt)

y <- cor(raw)
ynames <- dimnames(y)[1]
toolnames <- unlist(ynames)

library(qgraph)
qgraph(cor(raw),directed=TRUE,details=TRUE,graph='concentration')
qgraph(cor(raw),filetype='tex',tooltips=toolnames,directed=TRUE,layout='spring',details=TRUE,edge.labels=TRUE,edge.label.cex=.7, asize=.1,graph='association')
qgraph.efa(raw,6,rotation='varimax',details=TRUE, layout='TREE',factorCors=TRUE,scores="regression",crossloadings=FALSE)

#library(quantreg)
#zreg <- rq(raw$BUS_PERCENT~raw$GDS_BOOK_PCT, .5)
#plot(zreg)

#run factor analyis for 5 factors
factor_data <- factanal((raw[,2:29]), factors=10, data=raw,scores='regression', rotation="varimax")
factor_loadings <- factor_data$loadings
factor_loadings

#run prediction model suite
library(randomForest)
rf1 <- randomForest(as.factor(REGISTRATION_B) ~ ., data=raw[,-1], ntree=100, importance=TRUE)
importance <- (data.frame(round(importance(rf1), 2)))
plot(rf1)
varImpPlot(rf1)

#library(rpart)
#tree1 <- rpart(as.factor(REGISTRATION_B) ~ ., data=raw[,-1],method='class', control=rpart.control(minsplit=500))
#printcp(tree1)
#plot(tree1, uniform=FALSE)
#text(tree1,digits=3, cex=.7)

library(party)
fit <- ctree(REGISTRATION_B ~ ., data=raw[,-1], controls=ctree_control(minbucket=200))
plot(fit, main="Loyalty Registration Prediction", type="simple")
plot(fit, main="Loyalty Registration Prediction", terminal_panel = node_barplot)
plot(fit, main="Loyalty Registration Prediction", inner_panel = node_barplot,terminal_panel = node_barplot)

library(Design)
llogit = lrm(as.factor(REGISTRATION_B) ~ ., data=raw[,-1], x=TRUE, y=TRUE)
validate(llogit, B=100, bw=TRUE, rule="p", sls=.1, type="individual")

library(nnet)
nnet1 = nnet(REGISTRATION_B ~ ., data=raw[,-1], size=14, maxit=200)

#run OLS
ols <- lm(REGISTRATION_B ~ BUS_PERCENT + STAY_C + LEADTIME + CRO_BOOK_PCT + POINTS_COLLECTOR + WEB_BOOK_PCT + MIDSCALE_PCT + NIGHTSPERSTAY, data=raw[,-1])

#run ROC curves
#library(ROCR)
#pred <- predict(fit)
#perf <- performance(pred,"tpr","fpr")
#plot(perf)

#run variance decomposition
library(relaimpo)
a <- calc.relimp(ols,type = c("lmg","last", "first", "betasq", "pratt"), rela = TRUE)
#a <- calc.relimp(ols,type = c("lmg", "pmvd", "last", "first", "betasq", "pratt"), rela = TRUE)
plot(a)

#run kmeans
library(reshape)
scaleraw <- rescaler.data.frame(raw[,-1],type="sd")
kout = kmeans(scaleraw, 12, iter.max=500)
#plot(raw[,-1], col = kout$cluster)
center <- data.frame(kout$centers)
tcenter <- data.frame(t(center))

#par(mar=c(5,4.1,4.1,2.1))
nclus <- barplot(kout$size,main="Cluster Size",sub="Cluster", names.arg=1:12, col="blue", ylim=c(0,max(kout$size+kout$size*.05)))
text(nclus, kout$size + .03*max(kout$size),text=kout$size,kout$size, cex=.75)
#par(oma=c(1.5,0,0,0)) # 1.5 line2 at the bottom
#GenericFigure("Plot Area", 3,2)
#shortname <- "Figure1" # or maybe a filename
#mtext(paste(shortname, " ", format(Sys.time(), "%Y-%m-%d %H:%M")),cex=0.75, line=0, side=SOUTH<-1, adj=0, outer=TRUE)

#run bar plots
par(mfrow=c(1,3))
#margin reduce bottom
par(mar=c(3,4.1,4.1,2.1))
par(ask=TRUE)
labels=rownames(tcenter)
for(i in 1:12) {
vplot <- paste("X",i,sep="")
a <- barplot(tcenter[,vplot],las=2,col=ifelse(tcenter[,vplot] >= .5, 'green', ifelse(tcenter[,vplot] <= -.5, 'red', 'grey')),horiz=TRUE, main = paste("Cluster ",i," Summary"))
mtext(text=labels, side=2, line=-3, at=a, cex=.6, col="black", las=2)
mtext(text=paste("n=",kout$size[i]), side=3, cex=.6)
lines(-.5, 35, type='h', col='red', lty=2)
lines(.5, 35, type='h', col='red', lty=2)
}

#run segmentation
hvq_k <- hvq(scaleraw,nclust=100,depth=1, quant.err = .5)
hvqdata_subset <- hvq_k$ztab[,1:5]
hvqdata_subset
hvqdata <- hvq_k$ztab
hvqdata <- subset(hvqdata, hvqdata$n>100)

#visualize segmentation
hvqgraph33(hvqdata, zcols = 6:21, pal = 6, asp=NA, numrec=T, bwtess=T, axes=T)

#run color
hvqgraph33(hvqdata, zcols = 6:21, pal = 6, asp=NA, numrec=T, bwtess=F, axes=T,magnif=hvqdata$n)