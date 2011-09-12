
# read the data file
raw <- read.csv("hi.csv")

# data prep
raw$STRUC_DESC <- NULL
raw$LOC_DESC <- NULL
raw$MGMT_TYP_DESC <- NULL

raw$PercentTransientNights <- NULL
raw$PercentBusiness <- NULL
raw$inn_nts_totsty <- NULL
raw$FAC_ID <- NULL

#calc correlation matrix
y <- cor(raw)
factors <- factanal(raw,factors=5, rotation='varimax')
pairs(raw[,1:6], panel=panel.smooth)

library(corrgram)
corrgram(raw[,1:6],cex.labels=.7,order=TRUE,lower.panel=panel.pie,upper.panel=panel.pts, text.panel=panel.txt,diag.panel=panel.minmax)
corrgram(raw[,1:6],cex.labels=.7,order=TRUE,lower.panel=panel.shade,upper.panel=panel.pts, text.panel=panel.txt,diag.panel=panel.minmax)


# start qgraph
library(qgraph)

qgraph(y)

#show variables index
ynames <- data.frame(unlist(dimnames(y)[1]))
names(ynames) <- "node list"

#show only correlations minimum threshold, label edges, add a title
qgraph(y,minimum=.3,directed=FALSE,details=TRUE,edge.labels=TRUE,edge.label.cex=.6)
title("EDA: Correlation Network",line=-1,cex.main=1)

#create force directed layout
qgraph(y,minimum=.3,directed=TRUE,details=TRUE,edge.labels=TRUE,edge.label.cex=.6, layout='spring')
qgraph(y,minimum=.3,directed=TRUE,details=TRUE,edge.labels=TRUE,edge.label.cex=.6, layout='spring',asize=.1)
qgraph(y,minimum=.3,directed=TRUE,details=TRUE,edge.labels=TRUE,edge.label.cex=.6, layout='spring',asize=.1, cut=.5)

#partial correlation plots
qgraph(y,minimum=.1,directed=TRUE,details=TRUE,edge.labels=TRUE,edge.label.cex=.6, layout='spring',asize=.1, cut=.5, graph='concentration')

#factor analysis plots
qgraph(y,minimum=.1,directed=TRUE,details=TRUE, layout='spring',asize=.1,graph='factorial')
qgraph.efa(raw,5,rotation='varimax',details=TRUE, minimum=.4,edge.labels=TRUE,edge.label.cex=.6,factorCors=TRUE,layout="tree")
qgraph.efa(raw,5,rotation='varimax',details=TRUE, minimum=.4,edge.labels=TRUE,edge.label.cex=.6,factorCors=TRUE)

#for pdf tool tips
ynames <- dimnames(y)[1]
toolnames <- unlist(ynames)

qgraph(y,filetype='tex',tooltips=toolnames)
qgraph(y,filetype='tex',tooltips=toolnames,cut=.5,minimum=.2,directed=TRUE,layout='spring',details=TRUE,edge.labels=TRUE,edge.label.cex=.7, asize=.1,graph='association')
qgraph(y,filetype='tex',tooltips=toolnames,cut=.2,minimum=.1,directed=TRUE,layout='spring',details=TRUE,edge.labels=TRUE,edge.label.cex=.7, asize=.1,graph='concentration')

