#####     Load the needed packages     #####
require(xtable, quietly=TRUE)
require(ROCR, quietly=TRUE)
require(Hmisc, quietly=TRUE)
require(randomForest, quietly=TRUE)
require(party, quietly=TRUE)
require(Design, quietly=TRUE)
require(nnet, quietly=TRUE)


#####     Load the data set     #####
raw <-read.csv(file="export.txt")
raw$SUCCESS_B <- NULL
raw$IDS_BOOK_PCT <- NULL
old.par <- par(no.readonly = TRUE)
spec <- function(tab) tab[1,1]/(tab[1,1]+tab[2,1])
sens <- function(tab) tab[2,2]/(tab[2,2]+tab[1,2])
falsepos <- function(tab) tab[2,1]/(tab[2,1]+tab[1,1])
falseneg <- function(tab) tab[1,2]/(tab[1,2]+tab[2,2])
acc.rep <- function(x) round(c(Sensitivity=sens(x),Specificity=spec(x),"False Positive"=falsepos(x),"False Negative"=falseneg(x)),4)

opt.cut <- function(mpred){
	pred <- prediction(mpred,raw[train,"REGISTRATION_B"])
	perf.acc <- performance(pred,"acc")
	acc.rocr <- max(perf.acc@y.values[[1]])
	cutoff.list.acc <- unlist(perf.acc@x.values[[1]])
	return(cutoff.list.acc[which.max(perf.acc@y.values[[1]])])
}


#####     Initialize variables     #####

Sweave("bin_class_rep.Rnw")
tools::texi2dvi("bin_class_rep.tex",pdf=TRUE)
