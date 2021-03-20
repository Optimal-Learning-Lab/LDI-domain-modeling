

#2 datasets

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

#parameters
posKC<-3
usethresh<-FALSE
KCthresh<-.2
usethreshm<-TRUE
KCthreshm<-.2
RSVDcomp<-2
#rm(posKC,KCthreshm,RSVDcomp)



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



testKCmodel<-function (iter,posKC,KCthreshm,RSVDcomp,val){

#==========================Reduce matrix================================

  #==========================Reduce matrix================================
  #print(df[1:10,])
  reducedmatrix<-rsvd(df,RSVDcomp)
  rownames(reducedmatrix$v)<-rownames(mydata)
  #==========================cluster matrix==============================
  #View(reducedmatrix$v)
  cm <- (cmeans(reducedmatrix$v,centers=posKC))
  #print(cm)


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
  #View(KCmodel)
  colnames(KCmodel)[1] <- "AC"
  #print(KCmodel)
  KCmodel$rows<-rownames(KCmodel)
  val<-merge(val,
             KCmodel,
             by.y = 'rows',
             by.x = 'KC..Default.',
             sort = FALSE)


val<-val[order(val$Anon.Student.Id,val$Time),]

#=================Test===============================

modelob<-LKT(data=rlvl(val),components=c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.","AC"),
             features=c("intercept","intercept","linesuc$","linefail$","linecomp$"),
             #covariates=c(NA,NA,NA,NA,"KC..Default.","KC..Default."),
             interc=TRUE,verbose=FALSE)

val[,("AC"):=NULL]

trows<-KCmodel$rows

KCmodel$AC<-sample(KCmodel$AC)
KCmodel$rows<-trows
val<-merge(val,KCmodel,
            by.y = 'rows',by.x='KC..Default.',sort=FALSE)
val<-val[order(val$Row),]

modelob2<-LKT(data=rlvl(val),components=c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.","AC"),
              features=c("intercept","intercept","linesuc$","linefail$","linecomp$"),
              #covariates=c(NA,NA,NA,NA,"KC..Default.","KC..Default."),
              interc=TRUE,verbose=FALSE)

cat(paste(posKC,KCthreshm,RSVDcomp,modelob$r2,
          modelob2$r2,mean(modelob$subjectrmse$x),
          mean(modelob2$subjectrmse$x),(modelob$r2-modelob2$r2)/(modelob2$r2),"\n",sep=","))
x<<-rbind(x,c(posKC,RSVDcomp,modelob$r2,(modelob$r2-modelob2$r2)/(modelob2$r2)))

y1<<-modelob$coefs
y2<<-modelob2$coefs
mod<<-KCmodel
}

x<<-data.frame()

temp<-grid_search(testKCmodel,params=list(KCthreshm=c(.1),RSVDcomp=c(2:9),posKC=c(2:9                                                                                  )),val=val)
names(x)<-c("KCs","Components","R-squared","R-squared_Gain")
#heatmap(as.matrix(dcast(x,KCs~Components, value.var="R-squared_Gain")[,2:4]))
y1[grepl("linecomp",rownames(y1)),]
y2[grepl("linecomp",rownames(y2)),]
mean(y1[grepl("linecomp",rownames(y1)),])
mean(y2[grepl("linecomp",rownames(y2)),])
range(y1[grepl("linecomp",rownames(y1)),])
sd(y2[grepl("linecomp",rownames(y2)),])
View(mod[order(mod$AC),])

bar(dv = "R-squared_Gain",
    factors = c(Components,KCs),
    dataframe = x,
    errbar = FALSE,
    ylim=c(0,.01
           ))




