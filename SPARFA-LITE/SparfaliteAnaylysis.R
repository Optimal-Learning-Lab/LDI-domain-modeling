# this code is for the Sparfalite Analysis, which includes two functions: litecv.R and sparfalite.R


setwd("C:\\Users\\Liang Zhang\\Desktop\\2022_Spring\\LDI\\LDI-domain-modeling\\SPARFA-LITE")

source("litecv.R")

setwd("C:\\Users\\Liang Zhang\\Desktop\\2022_Spring\\LDI\\LDI-domain-modeling\\SPARFA-LITE\\testdata")

#setwd("C:\\Users\\Liang Zhang\\Desktop\\2022_Spring\\LDI\\LDI-domain-modeling\\ClozeResultsSparfa\\testdata")
val<-read.table("ds1465_tx_All_Data_64_2016_0720_222352short.txt",sep="\t", header=TRUE,na.strings="NA",quote="",comment.char = "")
#val<-read.table("ds1462_tx_All_Data_61_2016_0121_231536.txt",sep="\t", header=TRUE,na.strings="NA",quote="",comment.char = "")
val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,NaN))
val$CF..ansbin.<-as.numeric(val$CF..ansbin.)
val<-val[!is.nan(val$CF..ansbin.),]    #remove rows containing NaN in val$CF..ansbin.

rownames<-val$KC..Cluster.
val$KC..Cluster.<-as.numeric(regmatches(x =val$KC..Cluster.,regexpr("^[0-9]+",text = val$KC..Cluster.)))
val$KC..Cluster.<-ifelse(val$KC..Cluster.>17,val$KC..Cluster.-18,val$KC..Cluster.)  # the questions are repeated when >17
rownumbers<-val$KC..Cluster.

#Used to do colnames
row<-data.frame(rownumbers,rownames)
#Remove duplicate rownames based on a variable
library(dplyr)
rowClean<- distinct(row,rownumbers, .keep_all= T)
rowClean<-rowClean[with(rowClean,order(rownumbers)),]

#val$KC..Cluster.<-paste( val$KC..Cluster.,val$CF..Stimulus.Version.,val$CF..Correct.Answer.,sep="-")

aggdata<-aggregate(val$CF..ansbin.,by=list(val$KC..Cluster.,val$Anon.Student.Id),FUN=mean)
colnames(aggdata)<-c('KC..Cluster.','Anon.Student.Id','CF..ansbin.')
aggdata<-aggdata[with(aggdata,order(KC..Cluster.)),]

library(reshape2)
mydata<-dcast(aggdata, KC..Cluster. ~ Anon.Student.Id, value.var="CF..ansbin.") #reshape to wide data format

#remove the NA values
for(i in 1:ncol(mydata)){
  mydata[is.na(mydata[,i]), i] <- mean(mydata[,i], na.rm = TRUE)}

#rownames(mydata)<-mydata[,1]
rownames(mydata)<-rowClean$rownames
mydata<-mydata[,-1]

mydata<-ifelse(mydata>0.5,1,0) #transfer to binary matrix, may have many other methods

mydata<-as.matrix(mydata)

Z<-cvlite(mydata)
rownames(Z)<-rownames(mydata)
colnames(Z)<-colnames(mydata)
ZOut<-cbind(rownames(Z),Z)
write.table(ZOut,file="Z.txt",sep="\t",row.names=F, col.names=T,eol = "\n", quote=F)

library(Hmisc)
library(ggplot2)
res<-rcorr(t(Z),type="spearman")
#extract the p-values or the correlation coefficients
cor<-res$r
corp<-res$P

#search elements that is larger than 0.60 in cor matrix
corSearch<-which( cor > 0.60, arr.ind=T )
corSearch<-as.data.frame(corSearch)
for (r in 1:nrow(corSearch)){
  corSearch$Correlation_Coefficient[r]<-cor[corSearch$row[r],corSearch$col[r]]
  corSearch$Significance[r]<-corp[corSearch$row[r],corSearch$col[r]]
}
corSearch<-na.omit(corSearch)
corSearch$row<-corSearch$row-1
corSearch$col<-corSearch$col-1
corSearch<-corSearch[with(corSearch,order(row)),]
colnames(corSearch)[which(names(corSearch) == "row")]="Question No. 1st"
colnames(corSearch)[which(names(corSearch) == "col")]="Question No. 2nd"

write.table(corSearch,file="corSearch.txt",sep="\t",row.names=F, col.names=T,eol = "\n", quote=F)
library(flextable)
library(webshot)
ft <- flextable(corSearch)
ft <- align(ft, align = "left", part = "all")
ft <- autofit(ft)
print(ft)
save_as_image(ft, path = "corSearch.png")
