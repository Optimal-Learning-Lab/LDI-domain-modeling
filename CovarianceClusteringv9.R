

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


setwd("C:\\Users\\ppavl\\Dropbox\\Active Projects\\LDI-domain-modeling\\")

val<-setDT(read.table("ds126_tx_All_Data_297_2015_0802_174904.txt",sep="\t",
                      header=TRUE,na.strings="NA",quote="",comment.char = ""))
#val$KC..Default.<-val$KC..Article_Rule.

val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val$CF..ansbin.<-as.numeric(val$CF..ansbin.)
val<-val[val$CF..ansbin.!=-1,]
#val<-val[val$KC..Default.!="done",]
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

df[df>7] <- 0

testKCmodel<-function (iter,posKC,KCthreshm,RSVDcomp,val){
  set.seed(42)
#==========================Reduce matrix================================

reducedmatrix<-rsvd(df,RSVDcomp)
rownames(reducedmatrix$v)<-rownames(mydata)

#==========================cluster matrix==============================

cm <- (cmeans(reducedmatrix$v,centers=posKC))
#print(cm)
#===========================visualizations====================

# library(factoextra)
# x<-fviz_cluster(list(data = reducedmatrix$v, cluster=cm$cluster),
#              ellipse.type = "norm",
#              ellipse.level = .999,
#              palette = "jco",
#              repel=TRUE,
#              ggtheme = theme_minimal(),xlab="",ylab="")
# plot(x)


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

KCmodel$rows<-rownames(KCmodel)
val<-merge(val,
           KCmodel,
           by.y = 'rows',
           by.x = 'KC..Default.',
           sort = FALSE)



if (usethreshm) {
  KCmodelm <- as.data.frame(ifelse(cm$membership > KCthreshm, 1, 0))
} else {
  KCmodelm <- as.data.frame(cm$membership)
}
#View(KCmodelm)
colnames(KCmodelm)<-paste0("c", colnames(KCmodelm), sep = "")

KCmodelm$rows<- rownames(KCmodelm)

val<-merge(val,
           KCmodelm,
           by.y = 'rows',
           by.x = 'KC..Default.',
           sort = FALSE
)

#print(KCmodelm)


val<-val[order(val$Row),]
#=================Visualize============

expand.matrix <- function(A){
  m <- nrow(A)
  n <- ncol(A)
  B <- matrix(0,nrow = m, ncol = m)
  C <- matrix(0,nrow = n, ncol = n)
  cbind(rbind(B,t(A)),rbind(A,C))
}
KCmodelmat<-KCmodelm
KCmodelmat$rows<-NULL
g<-expand.matrix(as.matrix(KCmodelmat))



rownames(g)<-gsub( "|",  ".", colnames(g), fixed = TRUE)
colnames(g)<-rownames(g)

#am.graph<-new("graphAM", adjMat=g, edgemode="undirected")
#plot(am.graph, attrs = list(graph = list(overlap="prism"),
#                            node = list(fillcolor = "lightblue",fontsize=300,height=800),
 #                           edge = list(arrowsize=0.5)),"neato")


#=================Test===============================

compKC<-paste(paste("c",1:posKC,sep=""),collapse="__")

modelob<-LKT(data=rlvl(val),components=c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.","AC"),
             features=c("intercept","intercept","linesuc$","linefail$","linecomp$"),
             #covariates=c(NA,NA,NA,NA,"KC..Default.","KC..Default."),
             interc=FALSE,verbose=FALSE,cost=1024)

val[,("AC"):=NULL]
val[,(paste0("c",1:posKC)):=NULL]

trows<-KCmodel$rows
KCmodel$AC<-sample(KCmodel$AC)
KCmodel$rows<-trows
val<-merge(val,KCmodel,
            by.y = 'rows',by.x='KC..Default.',sort=FALSE)
val<-val[order(val$Row),]

trows<-KCmodelm$rows
KCmodelm<-KCmodelm[sample(nrow(KCmodelm)),]
KCmodelm$rows<-trows
val<-merge(val,KCmodelm,
            by.y = 'rows',by.x='KC..Default.',sort=FALSE)
val<-val[order(val$Row),]
#View(val)
modelob2<-LKT(data=rlvl(val),components=c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.","AC"),
              features=c("intercept","intercept","linesuc$","linefail$","linecomp$"),
              #covariates=c(NA,NA,NA,NA,"KC..Default.","KC..Default."),
              interc=FALSE,verbose=FALSE,cost=1024)

cat(paste(posKC,KCthreshm,RSVDcomp,modelob$r2,
          modelob2$r2,mean(modelob$subjectrmse$x),
          mean(modelob2$subjectrmse$x),modelob$r2-modelob2$r2,"\n",sep=","))
x<<-rbind(x,c(posKC,RSVDcomp,modelob$r2-modelob2$r2))
}

x<<-data.frame()

temp<-grid_search(testKCmodel,params=list(KCthreshm=c(.1),RSVDcomp=c(2,3,4,5,6,7,8),posKC=c(6,7,8,10,11,12)),val=val)
names(x)<-c("KCs","Components","R-squared_Gain")
#heatmap(as.matrix(dcast(x,KCs~Components, value.var="R-squared_Gain")[,2:4]))


bar(dv = "R-squared_Gain",
    factors = c(Components,KCs),
    dataframe = x,
    errbar = FALSE,
    ylim=c(0, .005))

LKT(data=rlvl(val),components=c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default."),
    features=c("intercept","intercept","logsuc$","logfail$"),
    fixedpars=c(.9,.7),interc=TRUE,verbose=FALSE)$r2


#testKCmodel(KCthreshm=.3,RSVDcomp=2,posKC=4,val=val)








