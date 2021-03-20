
setwd("C:\\Users\\ppavl\\Dropbox\\Active Projects\\LDI-domain-modeling\\ClozeResultsSparfa")
library(reshape2)
library(car)
library(zoo)
library(gplots)
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



testKCmodel<-function (iter,posKC,KCthreshm,RSVDcomp,valtemp){

  #==========================Reduce matrix================================
  reducedmatrix<-rsvd(df,RSVDcomp)
  rownames(reducedmatrix$v)<-rownames(mydata)
  #==========================cluster matrix==============================
  cm <- (cmeans(reducedmatrix$v,centers=posKC))

  #=================extrapolate KC model==============
  if(usethresh) {
    KCmodel <-
      as.data.frame(sapply(apply(cm$membership, 1, function(x)
        which(x > KCthresh)), paste, collapse = " "))
  } else{
    KCmodel <-
      as.data.frame(sapply(apply(cm$membership, 1, function(x)
        which(x == max(x))), paste, collapse = " "))
  }
  colnames(KCmodel)[1] <- "AC"
  KCmodel$rows<-rownames(KCmodel)
  valtemp<-merge(valtemp,
                 KCmodel,
                 by.y = 'rows',
                 by.x = 'KC..Default.',
                 sort = FALSE)
  valtemp<-valtemp[order(valtemp$Anon.Student.Id,valtemp$Time),]

  #=================Test===============================

  modelob<-LKT(data=rlvl(valtemp),components=c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.","AC","AC"),
               features=c("intercept","intercept","logsuc$","logfail$","logsuc$","logfail$"),
               fixedpars=c(.9,.7),interc=TRUE,verbose=FALSE)

  valtemp[,("AC"):=NULL]
  trows<-KCmodel$rows
  KCmodel$AC<-sample(KCmodel$AC)
  KCmodel$rows<-trows
  valtemp<-merge(valtemp,KCmodel,
                 by.y = 'rows',by.x='KC..Default.',sort=FALSE)
  valtemp<-valtemp[order(valtemp$Row),]
  modelob2<-LKT(data=rlvl(valtemp),components=c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.","AC","AC"),
                features=c("intercept","intercept","logsuc$","logfail$","logsuc$","logfail$"),
                fixedpars=c(.9,.7),interc=TRUE,verbose=FALSE)

  cat(paste(posKC,KCthreshm,RSVDcomp,modelob$r2,
            modelob2$r2,mean(modelob$subjectrmse$x),
            mean(modelob2$subjectrmse$x),(modelob$r2-modelob2$r2)/(modelob2$r2),"\n",sep=","))
  x<<-rbind(x,c(posKC,RSVDcomp,modelob$r2,(modelob$r2-modelob2$r2)/(modelob2$r2)))

  y1<<-modelob$coefs
  y2<<-modelob2$coefs
  mod<<-KCmodel
}


x<<-data.frame()

temp<-grid_search(testKCmodel,params=list(KCthreshm=c(.1),RSVDcomp=c(2:9),posKC=c(2:9)),val=(val))
names(x)<-c("KCs","Components","R-squared","R-squared_Gain")

y1[grepl("logsucAC",rownames(y1)),]
y1[grepl("logfailAC",rownames(y1)),]

y2[grepl("logsucAC",rownames(y1)),]
y2[grepl("logfailAC",rownames(y1)),]

bar(dv = "R-squared_Gain",factors = c(Components,KCs),dataframe = x,errbar = FALSE,
    ylim=c(0, .02))
