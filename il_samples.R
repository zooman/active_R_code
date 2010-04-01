# Author: zdowlaty
###############################################################################


##if you want to use R to hit a database, the RJDBC package is nice

#require(RJDBC, quietly=TRUE)
#require(rJava, quietly=TRUE)

# makes connection to database and stores information in con variable 

#drv<-JDBC("org.postgresql.Driver","c:/jdbc/postgresql-8.4-701.jdbc3.jar"); 
#conn<-dbConnect(drv,"jdbc:postgresql://122.181.139.115:5432","postgres","postgres");

#submits query and returns data frame into output_data

#sqlstring <- paste("select * from stockdata",sep="")
#output_data <-dbGetQuery(conn,sqlstring)


#####Input some data from .csv

raw <-read.csv(file="export.txt")
raw$SUCCESS_B <- NULL
raw$IDS_BOOK_PCT <- NULL

library(Hmisc)
describe(raw)
hist(raw)

cor(raw)
# corr plots
pairs(raw[,2:7],panel=panel.smooth)

plot(raw$BUS_PERCENT,raw$GDS_BOOK_PCT, main="Scatterplot Example", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(raw$BUS_PERCENT~raw$GDS_BOOK_PCT), col="red") # regression line (y~x)
lines(lowess(raw$BUS_PERCENT,raw$GDS_BOOK_PCT,f=2/3),col="blue") # lowess line (x,y) 
library(hexbin)
bin<-hexbin(raw$STAY_C,raw$WEB_BOOK_PCT, xbins=50)
plot(bin, main="Hexagonal Binning")

library(corrgram)
corrgram(raw,cex.labels=.7,order=TRUE,lower.panel=panel.pie,upper.panel=panel.shade, text.panel=panel.txt)

#library(quantreg)
#zreg <- rq(raw$BUS_PERCENT~raw$GDS_BOOK_PCT, .5)
#plot(zreg)

library(reshape)
scaleraw <- rescaler.data.frame(raw[,-1],type="sd")

#run factor analyis for 5 factors

factor_data <- factanal((raw[,2:29]), factors=10, data=raw,scores='regression', rotation="varimax")
factor_loadings <- factor_data$loadings

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
llogit = lrm(as.factor(REGISTRATION_B) ~ ., data=raw[,-1])
validate(llogit, B=100, bw=TRUE, rule="p", sls=.1, type="individual")

library(nnet)
nnet1 = nnet(REGISTRATION_B ~ ., data=raw[,-1], size=14, maxit=200)

#run OLS
ols <- lm(REGISTRATION_B ~ BUS_PERCENT + STAY_C + LEADTIME + CRO_BOOK_PCT + POINTS_COLLECTOR + WEB_BOOK_PCT + MIDSCALE_PCT + NIGHTSPERSTAY, data=raw[,-1])

#run ROC curves
library(ROCR)
pred <- predict(fit)
perf <- performance(pred,"tpr","fpr")
plot(perf)

#run variance decomposition
library(relaimpo)
a <- calc.relimp(ols,type = c("lmg", "pmvd", "last", "first", "betasq", "pratt"), rela = TRUE)
plot(a)

#run kmeans
kout = kmeans(scaleraw, 12, iter.max=500)
#plot(raw[,-1], col = kout$cluster)
center <- data.frame(kout$centers)
tcenter <- data.frame(t(center))

par(mar=c(5,4.1,4.1,2.1))
nclus <- barplot(kout$size,main="Cluster Size",sub="Cluster", names.arg=1:12, col="blue")
text(nclus, kout$size + .03*max(kout$size),kout$size, text=as.character(kout$size), cex=.75)

par(oma=c(1.5,0,0,0)) # 1.5 line2 at the bottom
GenericFigure("Plot Area", 3,2)
shortname <- "Figure1" # or maybe a filename
mtext(paste(shortname, " ", format(Sys.time(), "%Y-%m-%d %H:%M")),cex=0.75, line=0, side=SOUTH<-1, adj=0, outer=TRUE)

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


x <- 1:3
bmp <- barplot(x, ylim = c(0, 1.1*max(x)))
text(bmp, x + .03*max(x), x)

