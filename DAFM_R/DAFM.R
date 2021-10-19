setwd("C:\\Users\\Liang Zhang\\Desktop\\2021_Fall\\LDIProject\\LDI-domain-modeling\\DAFM_R")
val<-read.table("ds1465_tx_All_Data_64_2016_0720_222352.txt",sep="\t", header=TRUE,na.strings="NA",quote="\"",comment.char = "")

#transfer original data:
#1."correct"->1, "incorrect"->0, other->-1
#2. Remove other
val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val$CF..ansbin.<-as.numeric(val$CF..ansbin.)
val<-val[val$CF..ansbin.!=-1,]

#
val$KC..Default.<-as.numeric(regmatches(x =val$KC..Default.,regexpr("^[^-]*[^ -]",text = val$KC..Default.)))

#
val$KC..Default.<-ifelse(val$KC..Default.>17,val$KC..Default.-18,val$KC..Default.)



val$index<-paste(val$Anon.Student.Id,val$KC..Default.,sep="")

countOutcome <-function(data,index,item) {
  data$temp<-ave(as.character(data$Outcome),index,FUN =function(x) as.numeric(cumsum(tolower(x)==tolower(item))))
  data$temp[tolower(as.character(data$Outcome))==tolower(item)]<-
    as.numeric(data$temp[tolower(as.character(data$Outcome))==tolower(item)])
  as.numeric(data$temp)}



val$cor<-countOutcome(val,val$index,"correct")
val$incor<-countOutcome(val,val$index,"incorrect")


library(dplyr)

#val<-
#  val %>%
#  group_by(index) %>%
#  slice(which.min(cor))

library(writexl)
write_xlsx(val,"C:\\Users\\Liang Zhang\\Desktop\\2021_Fall\\LDIProject\\LDI-domain-modeling\\DAFM_R\\Output\\outputdata.xlsx")

myData<-data.frame('correctness'=val$CF..ansbin.,'problem_id'=val$KC..Default.,'skill_name'=val$KC..Item.Model.,'Unit'=val$Level..Unitname.,'user_id'=val$Anon.Student.Id)

#We need column "base_sequence_id" for the representation="w2v-(withCorrectness / withoutCorrectness)"


headers<-gsub("Unique[.]step","Unique-step",colnames(myData))
headers<-gsub("[.]1","",headers)
headers<-gsub("[.]2","",headers)
headers<-gsub("[.]3","",headers)
headers<-gsub("Single[.]KC","Single-KC",headers)
headers<-gsub("[.][.]"," (",headers)
headers<-gsub("[.]$",")",headers)
headers<-gsub("[.]"," ",headers)
headers<-paste(headers,collapse="\t")

outputFilePath<-"C:\\Users\\Liang Zhang\\Desktop\\2021_Fall\\LDIProject\\LDI-domain-modeling\\DAFM\\datasets\\Example\\test.txt"

write.table(headers,file=outputFilePath,sep="\t",quote=FALSE,na = "",col.names=FALSE,append=FALSE,row.names = FALSE)
write.table(myData,file=outputFilePath,sep="\t",quote=FALSE,na = "",col.names=FALSE,append=TRUE,row.names = FALSE)
