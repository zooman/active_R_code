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

#run factor analyis for 5 factors

factor_data <- factanal((raw[,2:30]), factors=10, data=raw,scores='regression', rotation="varimax")
factor_loadings <- factor_data$loadings

#
library(randomForest)
rf1 <- randomForest(as.factor(REGISTRATION_B) ~ ., data=raw[,-1], ntree=100, importance=TRUE)
importance <- (data.frame(round(importance(rf1), 2)))

plot(rf1)
varImpPlot(rf1)


library(rpart)
tree1 <- rpart(as.factor(REGISTRATION_B) ~ ., data=raw[,-1],method='class', control=rpart.control(minsplit=500))
printcp(tree1)
plot(tree1, uniform=FALSE)
text(tree1,digits=3, cex=.7)

library(party)
fit <- ctree(REGISTRATION_B ~ ., data=raw[,-1])
plot(fit, main="Loyalty Registration Prediction")

library(ROCR)
pred <- predict(fit)
perf <- performance(pred,"tpr","fpr")
plot(perf)


library(Design)
llogit = lrm(as.factor(REGISTRATION_B) ~ ., data=raw[,-1])

ols <- lm(REGISTRATION_B ~ ., data=raw[,-1])

library(relaimpo)
a <- calc.relimp(ols,type = c("lmg", "last", "first", "betasq", "pratt"), rela = TRUE )
plot(a)







