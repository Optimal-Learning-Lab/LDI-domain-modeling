library(reshape2)
library(car)
library(zoo)
library(gplots)
#library(LKT)
library(rsvd)
library(e1071)
library(Rgraphviz)
library(Matrix)
library(SparseM)
library(LiblineaR)
library(dplyr)
library(paramtest)
library(data.table)
source("C://Users//ppavl//Dropbox//Active projects//LKT//R//LKTfunctions.R")

#==========================Data Preparation==============================


setwd("C:\\Users\\ppavl\\Dropbox\\Active Projects\\LDI-domain-modeling\\")

val<-setDT(read.table("ds126_tx_All_Data_297_2015_0802_174904.txt",sep="\t",
                      header=TRUE,na.strings="NA",quote="",comment.char = ""))
val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val$CF..ansbin.<-as.numeric(val$CF..ansbin.)
val<-val[val$CF..ansbin.!=-1,]
#val<-val[val$KC..Default.!="done",]
val<-val[val$KC..Default.!="",]



aggdata<- val[,mean(CF..ansbin.),by=list(KC..Default.,Anon.Student.Id)]
colnames(aggdata)<-c('KC..Default.','Anon.Student.Id','CF..ansbin.')
aggdata<-aggdata[with(aggdata,order(KC..Default.)),]
mydata<-dcast(aggdata, KC..Default. ~ Anon.Student.Id, value.var="CF..ansbin.") #reshape to wide data format
rownamesmydata<-mydata$KC..Default.
mydata<-mydata[,-1]

# determine the column names that contain NA values
nm <- names(mydata)[colSums(is.na(mydata)) != 0]
## replace with the mean - by 'id'
mydata[, (nm) := lapply(nm, function(x) {
  x <- get(x)
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})]




mydata<-log(mydata/(1-mydata))
mydata[mydata>2] <- 2
mydata[mydata<(-2)] <- -2
rownames(mydata)<-rownamesmydata

#==========================Feature matrix================================

mydata[, names(mydata) :=lapply(.SD, function(x) x - mean(x)), .SDcols = names(mydata)]
df <- mydata[,as.matrix(.SD) %*% t(as.matrix(.SD)),.SDcols=names(mydata)]
df<-df/nrow(df)
rownames(df)<-1:nrow(mydata)
colnames(df)<-rownames(mydata)

diag(df)<-0

x<<-data.frame()

temp<-grid_search(testKCmodel,params=list(KCthreshm=c(.1),RSVDcomp=c(2:10),posKC=c(2:10)),val=(val))
names(x)<-c("KCs","Components","R-squared","R-squared_Gain")

y1[grepl("logsucAC",rownames(y1)),]
y1[grepl("logfailAC",rownames(y1)),]

y2[grepl("logsucAC",rownames(y1)),]
y2[grepl("logfailAC",rownames(y1)),]

x$KCs <- sprintf('%i KCs', x$KCs)
bar(dv = "R-squared_Gain",factors = c(KCs,Components),dataframe = x,errbar = FALSE,
    ylim=c(0, .02))





#==========================Data Preparation==============================
setwd("C:/Users/ppavl/OneDrive - The University of Memphis/IES Data")
val<-setDT(read.table("ds1465_tx_All_Data_64_2016_0720_222352.txt",sep="\t", header=TRUE,na.strings="NA",quote="",comment.char = ""))

val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val$CF..ansbin.<-as.numeric(val$CF..ansbin.)
val<-val[val$CF..ansbin.!=-1,]
val$KC..Default.<-as.numeric(regmatches(x =val$KC..Default.,regexpr("^[^-]*[^ -]",text = val$KC..Default.)))
val$KC..Default.<-ifelse(val$KC..Default.>17,val$KC..Default.-18,val$KC..Default.)
val$KC..Default.<-paste( val$KC..Default.,val$CF..Stimulus.Version.,gsub(" ","",val$CF..Correct.Answer.),sep="-")
val<-val[val$KC..Default.!="done",]
val<-val[val$KC..Default.!="",]
aggdata<- val[,mean(CF..ansbin.),by=list(KC..Default.,Anon.Student.Id)]


colnames(aggdata)<-c('KC..Default.','Anon.Student.Id','CF..ansbin.')

aggdata<-aggdata[with(aggdata,order(KC..Default.)),]

mydata<-dcast(aggdata, KC..Default. ~ Anon.Student.Id, value.var="CF..ansbin.") #reshape to wide data format
#rm(aggdata)

rownamesmydata<-mydata$KC..Default.
mydata<-mydata[,-1]

# determine the column names that contain NA values
nm <- names(mydata)[colSums(is.na(mydata)) != 0]
## replace with the mean - by 'id'
mydata[, (nm) := lapply(nm, function(x) {
  x <- get(x)
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})]
mydata<-log(mydata/(1-mydata))
mydata[mydata>2] <- 2
mydata[mydata<(-2)] <- -2
rownames(mydata)<-rownamesmydata

#==========================Feature matrix================================

mydata[, names(mydata) :=lapply(.SD, function(x) x - mean(x)), .SDcols = names(mydata)]
df <- mydata[,as.matrix(.SD) %*% t(as.matrix(.SD)),.SDcols=names(mydata)]
df<-df/nrow(df)
rownames(df)<-1:nrow(mydata)
colnames(df)<-rownames(mydata)


diag(df)<-0

x<<-data.frame()

temp<-grid_search(testKCmodel,params=list(KCthreshm=c(.1),RSVDcomp=c(2:10),posKC=c(2:10)),val=(val))
names(x)<-c("KCs","Components","R-squared","R-squared_Gain")

y1[grepl("logsucAC",rownames(y1)),]
y1[grepl("logfailAC",rownames(y1)),]

y2[grepl("logsucAC",rownames(y1)),]
y2[grepl("logfailAC",rownames(y1)),]

x$KCs <- sprintf('%i KCs', x$KCs)
bar(dv = "R-squared_Gain",factors = c(KCs,Components),dataframe = x,errbar = FALSE,
    ylim=c(0, .02))






setwd("C:\\Users\\ppavl\\Dropbox\\Active Projects\\LDI-domain-modeling\\ClozeResultsSparfa")
source("litecvO.R")

#==========================Data Preparation==============================
setwd("C:\\Users\\ppavl\\Dropbox\\Active Projects\\LDI-domain-modeling\\")

val<-setDT(read.table("ds126_tx_All_Data_297_2015_0802_174904.txt",sep="\t",
                      header=TRUE,na.strings="NA",quote="",comment.char = ""))
val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val$CF..ansbin.<-as.numeric(val$CF..ansbin.)
val<-val[val$CF..ansbin.!=-1,]
#val<-val[val$KC..Default.!="done",]
val<-val[val$KC..Default.!="",]



aggdata<- val[,mean(CF..ansbin.),by=list(KC..Default.,Anon.Student.Id)]
colnames(aggdata)<-c('KC..Default.','Anon.Student.Id','CF..ansbin.')
aggdata<-aggdata[with(aggdata,order(KC..Default.)),]
mydata<-dcast(aggdata, KC..Default. ~ Anon.Student.Id, value.var="CF..ansbin.") #reshape to wide data format
rownamesmydata<-mydata$KC..Default.
mydata<-mydata[,-1]
# determine the column names that contain NA values
nm <- names(mydata)[colSums(is.na(mydata)) != 0]
## replace with the mean - by 'id'
mydata[, (nm) := lapply(nm, function(x) {
  x <- get(x)
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})]

rownames(mydata)<-rownamesmydata
newMydata<-as.data.frame(mydata)
rownames(newMydata)<-rownamesmydata

#==========================Sparfalite Analysis================================
mydata_matrix<-as.matrix(mydata)
Z<-cvlite(mydata_matrix)
df<-t(Z$z)
colnames(df)<-rownames(mydata)


x<<-data.frame()

temp<-grid_search(testKCmodel,params=list(KCthreshm=c(.1),RSVDcomp=c(2:10),posKC=c(2:10)),val=(val))
names(x)<-c("KCs","Components","R-squared","R-squared_Gain")

y1[grepl("logsucAC",rownames(y1)),]
y1[grepl("logfailAC",rownames(y1)),]

y2[grepl("logsucAC",rownames(y1)),]
y2[grepl("logfailAC",rownames(y1)),]

x$KCs <- sprintf('%i KCs', x$KCs)
bar(dv = "R-squared_Gain",factors = c(KCs,Components),dataframe = x,errbar = FALSE,
    ylim=c(0, .02))






#==========================Data Preparation==============================

setwd("C:/Users/ppavl/OneDrive - The University of Memphis/IES Data")
val<-setDT(read.table("ds1465_tx_All_Data_64_2016_0720_222352.txt",sep="\t", header=TRUE,na.strings="NA",quote="",comment.char = ""))

val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val$CF..ansbin.<-as.numeric(val$CF..ansbin.)
val<-val[val$CF..ansbin.!=-1,]
val$KC..Default.<-as.numeric(regmatches(x =val$KC..Default.,regexpr("^[^-]*[^ -]",text = val$KC..Default.)))
val$KC..Default.<-ifelse(val$KC..Default.>17,val$KC..Default.-18,val$KC..Default.)
val$KC..Default.<-paste( val$KC..Default.,val$CF..Stimulus.Version.,gsub(" ","",val$CF..Correct.Answer.),sep="-")


aggdata<- val[,mean(CF..ansbin.),by=list(KC..Default.,Anon.Student.Id)]
colnames(aggdata)<-c('KC..Default.','Anon.Student.Id','CF..ansbin.')
aggdata<-aggdata[with(aggdata,order(KC..Default.)),]
mydata<-dcast(aggdata, KC..Default. ~ Anon.Student.Id, value.var="CF..ansbin.") #reshape to wide data format
rownamesmydata<-mydata$KC..Default.
mydata<-mydata[,-1]
# determine the column names that contain NA values
nm <- names(mydata)[colSums(is.na(mydata)) != 0]
## replace with the mean - by 'id'
mydata[, (nm) := lapply(nm, function(x) {
  x <- get(x)
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})]

rownames(mydata)<-rownamesmydata
newMydata<-as.data.frame(mydata)
rownames(newMydata)<-rownamesmydata

#==========================Sparfalite Analysis================================
mydata_matrix<-as.matrix(mydata)
Z<-cvlite(mydata_matrix)
df<-t(Z$z)
colnames(df)<-rownames(mydata)



x<<-data.frame()

temp<-grid_search(testKCmodel,params=list(KCthreshm=c(.1),RSVDcomp=c(2:10),posKC=c(2:10)),val=(val))
names(x)<-c("KCs","Components","R-squared","R-squared_Gain")

y1[grepl("logsucAC",rownames(y1)),]
y1[grepl("logfailAC",rownames(y1)),]

y2[grepl("logsucAC",rownames(y1)),]
y2[grepl("logfailAC",rownames(y1)),]

x$KCs <- sprintf('%i KCs', x$KCs)
bar(dv = "R-squared_Gain",factors = c(KCs,Components),dataframe = x,errbar = FALSE,
    ylim=c(0, .02))



