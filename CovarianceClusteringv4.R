
library(reshape2)
library(car)
library(zoo)
library(gplots)
library(LKT)
library(rsvd)
library(e1071)
#==========================Data Preparation==============================

setwd("C:/Users/ppavl/OneDrive/IES Data")
val<-read.table("ds1465_tx_All_Data_64_2016_0720_222352.txt",sep="\t", header=TRUE,na.strings="NA",quote="",comment.char = "")


val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val$CF..ansbin.<-as.numeric(val$CF..ansbin.)
val<-val[val$CF..ansbin.!=-1,]
val$KC..Default.<-as.numeric(regmatches(x =val$KC..Default.,regexpr("^[^-]*[^ -]",text = val$KC..Default.)))
val$KC..Default.<-ifelse(val$KC..Default.>17,val$KC..Default.-18,val$KC..Default.)
val$KC..Default.<-paste( val$KC..Default.,val$CF..Stimulus.Version.,gsub(" ","",val$CF..Correct.Answer.),sep="-")

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

rownames(df)<-1:144 
colnames(df)<-rownames(mydata)


reducedmat2<-rsvd(df,3)
rownames(reducedmat2$v)<-rownames(mydata)

cm <- (cmeans(reducedmat2$v,centers=5))

#===========================visualizations====================


library(factoextra)
x<-fviz_cluster(list(data = reducedmat2$v, cluster=cm$cluster), 
             ellipse.type = "norm",
             ellipse.level = .68,
             palette = "jco", repel=TRUE,
             ggtheme = theme_minimal(),xlab="",ylab="")
plot(x)


#=================extrapolate KC model==============
KCmodel<-as.data.frame(sapply(apply(cm$membership,1,function(x) which(x==max(x))), paste, collapse = " "))
View(KCmodel)
colnames(KCmodel)[1] <- "AC"
#KCmodel$AC<-1
val3<-merge(val,KCmodel,by.y=0,by.x='KC..Default.',sort=FALSE)
val3<-val3[order(val3$Row),]


modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","KC..Default.","KC..Default."),
             features=c("logitdec","logitdec","logafm"),
             fixedpars=c(.9,.85,.85),interc=TRUE)

modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","KC..Default.","KC..Default.","AC"),
             features=c("logitdec","logitdec","logafm","logitdec$"),
             fixedpars=c(.9,.85,.85),interc=TRUE)
print(summary(modelob$model))

modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","AC","AC"),
             features=c("logitdec","logitdec","logafm"),
             fixedpars=c(.9,.85,.85))

KCmodel<-as.data.frame(sapply(apply(cm$membership,1,function(x) which(x==max(x))), paste, collapse = " "))
colnames(KCmodel)[1] <- "AC"
KCmodel$AC<-sample(KCmodel$AC)
val3<-merge(val,KCmodel,by.y=0,by.x='KC..Default.',sort=FALSE)
val3<-val3[order(val3$Row),]

modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","KC..Default.","KC..Default.","AC"),
             features=c("logitdec","logitdec","logafm","logitdec$"),
             fixedpars=c(.9,.85,.85))

modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","AC","AC"),
             features=c("logitdec","logitdec","logafm"),
             fixedpars=c(.9,.85,.85))

