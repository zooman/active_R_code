# TODO: Add comment
# 
# Author: zubin
###############################################################################


train <- read.csv("TrainingData.csv")

#train$Timestamp <- NULL
#timeStamp <- as.POSIXct(train$Timestamp)

train$Variable158LAST <- NULL
#train$Timestamp <- NULL

train$TargetVariable <- as.factor(train$TargetVariable)
train$Variable142OPEN <- as.numeric(as.character(train$Variable142OPEN))
train$Variable142HIGH <- as.numeric(as.character(train$Variable142HIGH))
train$Variable142LOW  <- as.numeric(as.character(train$Variable142LOW))
train$Variable142LAST <- as.numeric(as.character(train$Variable142LAST))

#create train with last variables
lastVariables <- grep("LAST",names(train),value=TRUE)
tVariables <- grep("^T",names(train),value=TRUE)
train = subset(train, select = c(lastVariables,tVariables))


library(Hmisc)
describe(train)
hist(train)


train <- read.csv("TrainingData.csv")
str(train)

# Step 1: Converting the dependent variable into factor
# Following Variable Converted --> TargetVariable
train$TargetVariable <- as.factor(train$TargetVariable)

#Step 2: Variables with all missing, just constant values throught are removed in this step
dont.want1 <- which(names(train) %in% c("Variable142OPEN","Variable142HIGH","Variable142LOW", "Variable142LAST", 
				"Variable143OPEN","Variable143HIGH","Variable143LOW", "Variable143LAST",                                    
				"Variable144OPEN","Variable144HIGH","Variable144LOW", "Variable144LAST",
				"Variable145OPEN","Variable145HIGH","Variable145LOW", "Variable145LAST",
				"Variable146OPEN","Variable146HIGH","Variable146LOW", "Variable146LAST", 
				"Variable147OPEN","Variable147HIGH","Variable147LOW", "Variable147LAST",
				"Variable148OPEN","Variable148HIGH","Variable148LOW", "Variable148LAST",
				"Variable149OPEN","Variable149HIGH","Variable149LOW", "Variable149LAST",
				"Variable150OPEN","Variable150HIGH","Variable150LOW", "Variable150LAST",
				"Variable151OPEN","Variable151HIGH","Variable151LOW", "Variable151LAST",
				"Variable158OPEN","Variable158HIGH","Variable158LOW", "Variable158LAST",
				"Variable167OPEN",	"Variable167HIGH", "Variable167LOW", 
				"Variable168OPEN", "Variable168HIGH",	"Variable168LOW", 
				"Variable169OPEN", "Variable169HIGH", "Variable169LOW", 
				"Variable170OPEN", "Variable170HIGH",	"Variable170LOW",
				"Variable171OPEN", "Variable171HIGH", "Variable171LOW", 
				"Variable172OPEN", "Variable172HIGH", "Variable172LOW", 
				"Variable173OPEN", "Variable173HIGH",	"Variable173LOW",	
				"Variable174OPEN", "Variable174HIGH", "Variable174LOW",	
				"Variable175OPEN", "Variable175HIGH",	"Variable175LOW",	
				"Variable176OPEN", "Variable176HIGH",	"Variable176LOW",	
				"Variable177OPEN", "Variable177HIGH",	"Variable177LOW",	
				"Variable178OPEN", "Variable178HIGH",	"Variable178LOW",	
				"Variable179OPEN", "Variable179HIGH",	"Variable179LOW",	
				"Variable180OPEN",	"Variable180HIGH",	"Variable180LOW"))	

#Step 3: Variables with most of the values missing - Less than 2% are non-missing
dont.want2 <- which(names(train) %in% c("Variable161OPEN","Variable161HIGH","Variable161LOW","Variable161LAST",
				"Variable162OPEN","Variable162HIGH","Variable162LOW","Variable162LAST",
				"Variable163OPEN","Variable163HIGH","Variable163LOW", "Variable163LAST"))
#Step 4: Removing the missing and constant values (the above two lists)
train <- train[,c(-dont.want1,-dont.want2)]

# Step 5: Selecting only TimeStamp, TargetVariable and variables containing the word LAST
LAST_Variables <-grep("*LAST",names(train),value=TRUE)
T_Variables <-grep("^T",names(train),value=TRUE)

train = subset(train, select = c(LAST_Variables,T_Variables))

#Step 6: Creating a variable for Timestamp which shouldn't be considered for analysis

TimeStamp <- which(names(train) %in% c("Timestamp"))
traintest <- train [, -TimeStamp]

#Step 7: Eliminating the categorical variables with lots of missing values and under representation
# for one of the categories

dont.want3 <- which(names(traintest) %in% c("Variable152LAST","Variable153LAST","Variable154LAST","Variable155LAST"))

traintest <- traintest[,c(-dont.want3)]

#Step 8: Eliminating the variables causing singularity

dont.want4 <- which(names(traintest) %in% c("Variable160LAST"))
traintest <- traintest[,c(-dont.want4)]

#Step 9: Converting variable Variable156LAST to numeric binary (0/1 instead of 5/6)

traintest$Variable156LAST <- as.numeric(traintest$Variable156LAST)
traintest$Variable156LAST <- ifelse(traintest$Variable156LAST ==5, 0, 1)

# Step 10: Running the Logistic Regression Model


logit <- glm(TargetVariable ~ ., data = traintest, family=binomial("logit")) 

train.predict <- predict(logit,newdata = traintest,type="response")          

# Step 11: ROC Curves Generation for the Training Data
library(ROCR)
plot(performance(prediction(train.predict,traintest$TargetVariable),"tpr","fpr"),col = "red")
auc1 <- performance(prediction(train.predict,traintest$TargetVariable),"auc")@y.values[[1]]
legend("bottomright",legend=c(paste("Logistic Regression (AUC=",formatC(auc1,digits=4,format="f"),")",sep="")),  
		col=c("red"), lty=1)


#Step 12: Splitting the dataset into training (approximately - 80%) and validation (Approximately - 20%)
# This being a time series data this is split sequentially

Training80Percent <- traintest[1:4700,]
Validation20Percent <- traintest[4701:5922,]  

#Step 12: Building the Logistic Regression Model for the Training Data and Obtaining the ROC Curves for the Training data
logittrain <- glm(TargetVariable ~ ., data = Training80Percent, family=binomial("logit"))
train80.predict <- predict(logittrain, newdata = Training80Percent,type="response")
library(ROCR)
plot(performance(prediction(train80.predict,Training80Percent$TargetVariable),"tpr","fpr"),col = "red")
auc1 <- performance(prediction(train80.predict,Training80Percent$TargetVariable),"auc")@y.values[[1]]
legend("bottomright",legend=c(paste("Logistic Regression 80% Training Data (AUC=",formatC(auc1,digits=4,format="f"),")",sep="")), 
		col=c("red"), lty=1)

#Step 13: Building the Logistic Regression Model for the Validation Data and Obtaining the ROC Curves for the Validation data
valid20.predict <- predict(logittrain, newdata = Validation20Percent,type="response")
library(ROCR)
plot(performance(prediction(valid20.predict,Validation20Percent$TargetVariable),"tpr","fpr"),col = "red")
auc1 <- performance(prediction(valid20.predict,Validation20Percent$TargetVariable),"auc")@y.values[1]
legend("bottomright",legend=c(paste("Logistic Regression 20% Validation Data (AUC=",formatC(auc1,digits=4,format="f"),")",sep="")), 
		col=c("red"), lty=1)
