# Author: zubin
###############################################################################


library(RJDBC)
library(rJava)

setwd("c:/")
ldata <- read.csv("c:\export.csv")

drv<-JDBC("org.postgresql.Driver","c:/jdbc/postgresql-8.4-701.jdbc3.jar"); 
conn<-dbConnect(drv,"jdbc:postgresql://localhost","postgres","coolz");

d<-dbWriteTable(conn,"loyalty",ldata)
dbSendQuery(conn,"DROP TABLE loyalty")
dbSendQuery(conn,"CREATE TABLE loyalty (ID varchar(30))")
dbSendQuery(conn,sqlstring)

sqlstring <- paste("CREATE TABLE loyalty (
		ID
		BUS_PERCENT
		STAY_C
		PROPLOYALTY
		PRICE_INDEX
		LEADTIME
		IDS_BOOK_PCT
		CRO_BOOK_PCT
		SUCCESS_B
		POINTS_EARNEDPERREV
		MIDSCALE_PCT
		UPSCALE_PCT
		EXT_STAY_PCT
		COPARTNER_B
		AIRPORT_PCT
		RESORT_PCT
		SUBURBAN_PCT
		SMALLTOWN_PCT
		URBAN_PCT
		ADJMS_B
		DIST1_PCT
		WEEKENDS_PCT
		US_US_PCT
		MEMBER_CLUB
		POINTS_COLLECTOR
		GDS_BOOK_PCT
		WEB_BOOK_PCT
		P_REDEEM_B
		REGISTRATION_B
		NIGHTSPERSTAY
		REVPERNIGHT
		)", sep="")


trim <- function(x) gsub("(\\n|\\t| )+"," ",x)

#use:> trim(sqlstring)

