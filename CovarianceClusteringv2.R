
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
print(rownames(mydata))

pca - svd -rsvd
factor analysis




library(rsvd)
reducedmat2<-rsvd(df,2)

rownames(reducedmat2$v)<-rownames(mydata)

View(reducedmat2$v)
cor(c(reducedmat2$v[,1]),c(reducedmat2$v[,2]))
library(e1071)


cm <- (cmeans(reducedmat2$v,centers=3))



library(factoextra)
x<-fviz_cluster(list(data = reducedmat2$v, cluster=cm$cluster), 
             ellipse.type = "norm",
             ellipse.level = 0.68,
             palette = "jco", repel=TRUE,
             ggtheme = theme_minimal(),xlab="",ylab="")

#===========================visualizations====================

#Distance/Dissimilarity matrix
d<-dist(df,method="euclidean")
heatmap.2(df,cexRow=1,cexCol=1,symm=T)

#Hierarchical clustering
hclust(d, method = "complete" )

#Correlation matrix
heatmap.2(cor(df),cexRow=1,cexCol=1,symm=T)

silmax<-numeric(5)
for(i in 2:5){
fit<-pam(df,i,diss=FALSE)
silmax[i]<-fit$silinfo$avg.width}
print(silmax)
k.best <- which.max(silmax)

fit<-pam(df,2,diss=FALSE)
# vary parameters for most readable graph
fit$silinfo$avg.width
library(cluster)
x<-clusplot(fit, color=TRUE, shade=TRUE,
         labels=2, lines=0)





#=================extrapolate KC model==============

