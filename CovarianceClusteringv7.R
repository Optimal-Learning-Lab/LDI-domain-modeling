
library(reshape2)
library(car)
library(zoo)
library(gplots)
#library(LKT)
library(rsvd)
library(e1071)
library(Rgraphviz)

#parameters
posKC<-3
usethresh<-FALSE
KCthresh<-.2
usethreshm<-TRUE
KCthreshm<-.2
RSVDcomp<-2
rm(posKC,KCthreshm,RSVDcomp)



#==========================Data Preparation==============================

setwd("C:/Users/ppavl/Dropbox/Documents - Academic/Data archives/GLRKT_datasets/KDD_algebra")
val<-read.table("algebra_2005_2006_train.txt",sep="\t", header=TRUE,na.strings="NA",quote="",comment.char = "")


smallSet <- function(data,nSub){#returns data.frame with nSub participant data
  totsub=length(unique(data$Anon.Student.Id))
  datasub=unique(data$Anon.Student.Id)
  smallSub=datasub[sample(1:totsub)[1:nSub]]

  smallIdx=which(data$Anon.Student.Id %in% smallSub)
  smalldata = data[smallIdx,]
  smalldata=droplevels(smalldata)
  return(smalldata)
}

val=smallSet(val,250)

colnames(val)[colnames(val)=="KC.Default."] <- "KC..Default."

#Corrects is total correct attempts by student for step. Only increases if encountered more than once
#Most steps not encountered twice.
val$sum_hintincor=(val$Incorrects+val$Hints)
val=val[which(val$sum_hintincor==1 | val$Corrects==1),]
val=val[-which(val$KC..Default.==""),]#I think the unlabelled rows also tend to be beginning steps, I'm going to review the original papers to see whats going on with that - LE
val=droplevels(val)

colnames(val)[14] <- "CF..ansbin."
colnames(val)[11] <-"Duration..sec."
val$CF..Time. <- as.numeric(as.POSIXct(as.character(val$Step.Start.Time),format="%Y-%m-%d %H:%M:%S"))
val<-val[order(val$Anon.Student.Id, val$CF..Time.),]
val<-val[!is.na(val$CF..Time.),]


val$Outcome=rep("CORRECT",length(val[,1]))
val$Outcome[which(val$CF..ansbin.==0)] = "INCORRECT"

#Replace large outliers with 95%ile
dur.outlier = quantile(val$Duration..sec.,seq(0,1,.05))[20]
if(any(val$Duration..sec.>dur.outlier)){
  val$Duration..sec.[which(val$Duration..sec.>dur.outlier)] = quantile(val$Duration..sec.,seq(0,1,.05))[20]
}

#to get unique first attempts of a problem (not step): paste("ProblemH.Hierarchy","")
#e.g., something like unique(paste(val$Problem.Hierarchy,val$Problem.View)[1:100])..

aggdata<-aggregate(val$CF..ansbin.,by=list(val$KC..Default.,val$Anon.Student.Id),FUN=mean)
colnames(aggdata)<-c('KC..Default.','Anon.Student.Id','CF..ansbin.')

aggdata<-aggdata[with(aggdata,order(KC..Default.)),]

mydata<-dcast(aggdata, KC..Default. ~ Anon.Student.Id, value.var="CF..ansbin.") #reshape to wide data format

rownames(mydata)<-mydata[,1]
mydata<-mydata[,-1]
mydata<-na.aggregate(mydata)

mydata<-apply(mydata,1:2,logit)
mydata[which(mydata>2)] <- 2
mydata[which(mydata<(-2))] <- -2

#==========================Feature matrix================================

df<-data.frame()
for (i in 1:ncol(mydata)){
  disVector<-mydata[,i]-mean(mydata[,i])  #means for each subject
  diagvectors<-disVector %*% t(disVector) #matrix for each subject
  if(i>1){
    df=df+diagvectors # sum of matrixes for all students _-> feature matrix
  }else{
    df=diagvectors
  }
}
df<-df/nrow(df)

rownames(df)<-1:nrow(mydata)
colnames(df)<-rownames(mydata)


testKCmodel<-function (iter,posKC,KCthreshm,RSVDcomp){
#==========================Reduce matrix================================

reducedmatrix<-rsvd(df,RSVDcomp)
rownames(reducedmatrix$v)<-rownames(mydata)

#==========================cluster matrix==============================

cm <- (cmeans(reducedmatrix$v,centers=posKC))

#===========================visualizations====================

# library(factoextra)
# x<-fviz_cluster(list(data = reducedmatrix$v, cluster=cm$cluster),
#              ellipse.type = "norm",
#              ellipse.level = .999,
#              palette = "jco",
#              repel=TRUE,
#              ggtheme = theme_minimal(),xlab="",ylab="")
# plot(x)

val3<-val
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
val3<-merge(val3,
            KCmodel,
            by.y = 0,
            by.x = 'KC..Default.',
            sort = FALSE)
CCKCs<-KCmodel

if (usethreshm) {
  KCmodelm <- ifelse(cm$membership > KCthreshm, 1, 0)
} else {
  KCmodelm <- cm$membership
}
#View(KCmodelm)
colnames(KCmodelm)<-paste0("c", colnames(KCmodelm), sep = "")

val3<-merge(val3,
            KCmodelm,
            by.y = 0,
            by.x = 'KC..Default.',
            sort = FALSE
)


val3<-val3[order(val3$Row),]
#=================Visualize============

expand.matrix <- function(A){
  m <- nrow(A)
  n <- ncol(A)
  B <- matrix(0,nrow = m, ncol = m)
  C <- matrix(0,nrow = n, ncol = n)
  cbind(rbind(B,t(A)),rbind(A,C))
}

g<-expand.matrix(KCmodelm)
rownames(g)<-gsub( "|",  ".", colnames(g), fixed = TRUE)
colnames(g)<-rownames(g)

am.graph<-new("graphAM", adjMat=g, edgemode="undirected")
plot(am.graph, attrs = list(graph = list(overlap="prism"),
                            node = list(fillcolor = "lightblue",fontsize=200,height=500),
                            edge = list(arrowsize=0.5)),"neato")


#=================Test===============================

compKC<-paste(paste("c",1:posKC,sep=""),collapse="_")

modelob<-LKT(data=val3,components=c("Anon.Student.Id","KC..Default.","KC..Default.",compKC,compKC),
             features=c("logitdec","logitdec","logafm","clogsuc","clinefail"),
             fixedpars=c(.9,.85),interc=TRUE,verbose=FALSE)
cat(paste(posKC,KCthreshm,RSVDcomp,modelob$r2,sep=","),"\n")
return(modelob$r2)


}





library(paramtest)

grid_search(testKCmodel,params=list(KCthreshm=c((1:4)*.15),
                                  RSVDcomp=c(2,3,4),posKC=c(2,3,5,7)))












compKC<-paste(paste("c",1:posKC,sep=""),collapse="_")




modelob<-LKT(data=val3,components=c("Anon.Student.Id","KC..Default.","KC..Default.",compKC),
             features=c("logitdec","logitdec","logafm","clineafm"),
             fixedpars=c(.9,.85,.85),interc=TRUE,verbose=FALSE)
modelob$r2

modelob<-LKT(data=val3,components=c("Anon.Student.Id","KC..Default.","KC..Default.",compKC),
             features=c("logitdec","logitdec","logafm","clogitdec"),
             fixedpars=c(.9,.85,.85),interc=TRUE,verbose=FALSE)
modelob$r2

modelob<-LKT(data=val3,components=c("Anon.Student.Id","KC..Default.","KC..Default.",compKC,compKC),
             features=c("logitdec","logitdec","logafm","clogsuc","clinefail"),
             fixedpars=c(.9,.85),interc=TRUE,verbose=TRUE)
modelob$r2

modelob<-LKT(data=val3,components=c("Anon.Student.Id","KC..Default.","KC..Default.","AC"),
             features=c("logitdec","logitdec","logafm","logitdec$"),
             fixedpars=c(.9,.85,.85),interc=TRUE,verbose=FALSE)
modelob$r2

modelob<-LKT(data=val3,components=c("Anon.Student.Id","AC","AC"),
             features=c("logitdec","logitdec","logafm"),
             fixedpars=c(.9,.85,.85),interc=TRUE,verbose=FALSE)
modelob$r2

#=================================Randomize KC models to get comparison===================================
val3<-val

KCmodel$AC<-sample(KCmodel$AC)
val3<-merge(val3,KCmodel,by.y=0,by.x='KC..Default.',sort=FALSE)
val3<-val3[order(val3$Row),]

KCmodelm<-KCmodelm[sample(nrow(KCmodelm)),]
rownames(KCmodelm)<-rownames(KCmodel)
val3<-merge(val3,KCmodelm,by.y=0,by.x='KC..Default.',sort=FALSE)
val3<-val3[order(val3$Row),]

modelob<-LKT(data=val3,components=c("Anon.Student.Id","KC..Default.","KC..Default.",compKC),
             features=c("logitdec","logitdec","logafm","clineafm"),
             fixedpars=c(.9,.85,.85),interc=TRUE,verbose=FALSE)
modelob$r2

modelob<-LKT(data=val3,components=c("Anon.Student.Id","KC..Default.","KC..Default.",compKC),
             features=c("logitdec","logitdec","logafm","clogitdec"),
             fixedpars=c(.9,.85,.85),interc=TRUE,verbose=FALSE)
modelob$r2

modelob<-LKT(data=val3,components=c("Anon.Student.Id","KC..Default.","KC..Default.",compKC,compKC),
             features=c("logitdec","logitdec","logafm","clogsuc","clinefail"),
             fixedpars=c(.9,.85,.85),interc=TRUE,verbose=TRUE)
modelob$r2

modelob<-LKT(data=val3,components=c("Anon.Student.Id","KC..Default.","KC..Default.","AC"),
             features=c("logitdec","logitdec","logafm","logitdec$"),
             fixedpars=c(.9,.85,.85),interc=TRUE,verbose=FALSE)
modelob$r2

modelob<-LKT(data=val3,components=c("Anon.Student.Id","AC","AC"),
             features=c("logitdec","logitdec","logafm"),
             fixedpars=c(.9,.85,.85),interc=TRUE,verbose=FALSE)
modelob$r2



#
# val3<-computeSpacingPredictors(val3,"c1")
# val3<-computeSpacingPredictors(val3,"c2")
# val3<-computeSpacingPredictors(val3,"c3")
# val3<-computeSpacingPredictors(val3,"c4")
#
# val3<-computeSpacingPredictors(val3,"KC..Default.")
#
# modelob<-LKT(data=val3,components=c("Anon.Student.Id","KC..Default.","KC..Default.",compKC,"KC..Default."),
#              features=c("logitdec","logitdec","logafm","clogitdec","recency"),
#              fixedpars=c(.9,.85,.85,NA),seedpars=c(NA,NA,NA,.3),interc=TRUE,verbose=TRUE)
# modelob$r2
#
#
# modelob<-LKT(data=val3,components=c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default."),
#              features=c("logitdec","logitdec","logafm","recency"),
#              fixedpars=c(.9,.85,.85,NA),seedpars=c(NA,NA,NA,.3),interc=TRUE,verbose=TRUE)
# modelob$r2
