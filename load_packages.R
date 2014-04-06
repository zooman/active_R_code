# Author: zubin
###############################################################################

# see all packages installed
#library()  

#list all packages that I commonly use
p<-c()

#programming
p<-c(p,"Rpad")
p<-c(p,"plyr")
p<-c(p,"Rcurl")
p<-c(p,"reshape")
p<-c(p,"rJava")
p<-c(p,"ROCR")
p<-c(p,"XML")
p<-c(p,"xtable")
p<-c(p,"gmodels")
p<-c(p,"leaps")
p<-c(p,"gtools"); #used for hvqgraph
p<-c(p,"seas"); #used for hvqgraph
p<-c(p,"doBy")
p<-c(p,"knitr")

#database
p<-c(p,"DBI")
p<-c(p,"RJDBC")

#plotting
p<-c(p,"ggplot2")
p<-c(p,"corrgram")
p<-c(p,"deldir")
p<-c(p,"hexbin")
p<-c(p,"rgl")

#data mining/machine learning/NLP
p<-c(p,"gbm")
p<-c(p,"bayesm")
p<-c(p,"RWeka")
p<-c(p,"lsa")
p<-c(p,"tm")
p<-c(p,"caret")
p<-c(p,"earth")
p<-c(p,"lars")
p<-c(p,"party")
p<-c(p,"randomForest")
p<-c(p,"nnet")
p<-c(p,"rpart")
p<-c(p,"e1071")
p<-c(p,"mgcv")

#graphs and networks
p<-c(p,"igraph")
p<-c(p,"qgraph")

#statistics
p<-c(p,"survival")
p<-c(p,"Hmisc")
p<-c(p,"ICSNP")
p<-c(p,"car")
p<-c(p,"AlgDesign")
p<-c(p,"deal"); #bayesian networks
p<-c(p,"Design")
p<-c(p,"leaps")
p<-c(p,"MASS")
p<-c(p,"pls")
p<-c(p,"quantreg")
p<-c(p,"relaimpo")
p<-c(p,"nlme")

#time series
p<-c(p,"zoo")
p<-c(p,"xts")
p<-c(p,"quantmod")
p<-c(p,"dyn")
p<-c(p,"dynlm")
p<-c(p,"TTR")


repositories<-c("http://cran.cnr.Berkeley.edu","http://streaming.stat.iastate.edu/CRAN/")

install_package<-function(pack,repositories)
{
	if(!(pack %in% row.names(installed.packages())))
	{
		update.packages(repos=repositories, ask=F)
		install.packages(pack, repos=repositories, dependencies=T)
	}
	require(pack,character.only=TRUE)
}

for(pack in p)
{
	install_package(pack,repositories)
	print(pack)
}




