
#==========================Data Preparation==============================

x<-getwd()
setwd("C:/Users/ppavl/OneDrive/IES Data")
#setwd(x)
val<-read.table("ds1465_tx_All_Data_64_2016_0720_222352.txt",sep="\t", header=TRUE,na.strings="NA",quote="",comment.char = "")

val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val$CF..ansbin.<-as.numeric(val$CF..ansbin.)
val<-val[val$CF..ansbin.!=-1,]
val$KC..Cluster.<-regmatches(x =val$KC..Cluster.,regexpr("\\s(.*)",text = val$KC..Cluster.))
#val$KC..Cluster.<-ifelse(val$KC..Cluster.>17,val$KC..Cluster.-18,val$KC..Cluster.)
#val$KC..Cluster.<-paste( val$KC..Cluster.,val$CF..Stimulus.Version.,val$CF..Correct.Answer.,sep="-")
#Step 0
aggdata<-aggregate(val$CF..ansbin.,by=list(val$KC..Cluster.,val$Anon.Student.Id),FUN=mean)
colnames(aggdata)<-c('KC..Cluster.','Anon.Student.Id','CF..ansbin.')

aggdata<-aggdata[with(aggdata,order(KC..Cluster.)),]

library(reshape2)
library(car)
library(gplots)
mydata<-dcast(aggdata, KC..Cluster. ~ Anon.Student.Id, value.var="CF..ansbin.") #reshape to wide data format


rownames(mydata)<-mydata[,1]
mydata<-mydata[,-1]
mydata<-apply(mydata,1:2,logit)
mydata[which(mydata>2)] <- 3
mydata[which(mydata<(-2))] <- -3

###Q1: NAN Value problem
for(i in 1:ncol(mydata)){
  mydata[is.na(mydata[,i]), i] <- mean(mydata[,i], na.rm = TRUE)}


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

rownames(df)<-1:18 #rownames(mydata)
colnames(df)<-rownames(mydata)

library(rsvd)
reducedmat2<-rsvd(df,2)
rownames(reducedmat2$v)<-rownames(mydata)

library(e1071)
cm <- (cmeans(reducedmat2$v,centers=12))

#===========================visualizations====================


library(factoextra)
x<-fviz_cluster(list(data = reducedmat2$v, cluster=cm$cluster), 
             ellipse.type = "norm",
             ellipse.level = .95,
             palette = "jco", repel=TRUE,
             ggtheme = theme_minimal(),xlab="",ylab="")
plot(x)


#=================extrapolate KC model==============
#View(sapply(apply(cm$membership,1,function(x) which(x>.2)), paste, collapse = " "))
#make one kC column
KCmodel<-as.data.frame(sapply(apply(cm$membership,1,function(x) which(x==max(x))), paste, collapse = " "))
#KCmodel<-as.data.frame(sapply(apply(cm$membership,1,function(x) which(x>.2)), paste, collapse = " "))

View(KCmodel)
colnames(KCmodel)[1] <- "name"
val3<-merge(val,KCmodel,by.y=0,by.x='KC..Cluster.',sort=FALSE)
val3<-val3[order(val3$Row),]

library(LKT)
modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","KC..Cluster.","KC..Cluster.","name"),
             features=c("logitdec","logitdec","logafm","logitdec"),
             fixedpars=c(.9,.85,.85))

#print(summary(modelob$model))
print(round(sqrt(mean((val3$CF..ansbin.-modelob$prediction)^2)),4))
print(round(1-logLik(modelob$model)/logLik(modelob$nullmodel),4)[1])

modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","name","name"),
             features=c("logitdec","logitdec","logafm"),
             fixedpars=c(.9,.85,.85))


print(round(sqrt(mean((val3$CF..ansbin.-modelob$prediction)^2)),4))
print(round(1-logLik(modelob$model)/logLik(modelob$nullmodel),4)[1])

modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","KC..Cluster.","KC..Cluster."),
             features=c("logitdec","logitdec","logafm"),
             fixedpars=c(.9,.85,.85))

print(round(sqrt(mean((val3$CF..ansbin.-modelob$prediction)^2)),4))
print(round(1-logLik(modelob$model)/logLik(modelob$nullmodel),4)[1])

KCmodel<-as.data.frame(sapply(apply(cm$membership,1,function(x) which(x>.2)), paste, collapse = " "))
colnames(KCmodel)[1] <- "name"
KCmodel$name<-sample(KCmodel$name)
val3<-merge(val,KCmodel,by.y=0,by.x='KC..Cluster.',sort=FALSE)
val3<-val3[order(val3$Row),]

modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","KC..Cluster.","KC..Cluster.","name"),
             features=c("logitdec","logitdec","logafm","logitdec"),
             fixedpars=c(.9,.85,.85))
print(round(sqrt(mean((val3$CF..ansbin.-modelob$prediction)^2)),4))
print(round(1-logLik(modelob$model)/logLik(modelob$nullmodel),4)[1])

modelob<-LKT(data=val3,
             components=c("Anon.Student.Id","name","name"),
             features=c("logitdec","logitdec","logafm"),
             fixedpars=c(.9,.85,.85))

print(round(sqrt(mean((val3$CF..ansbin.-modelob$prediction)^2)),4))
print(round(1-logLik(modelob$model)/logLik(modelob$nullmodel),4)[1])
