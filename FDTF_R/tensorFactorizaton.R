
setwd("C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization")
val<-read.table("ds1465_tx_All_Data_64_2016_0720_222352short.txt",sep="\t", header=TRUE,na.strings="NA",quote="\"",comment.char = "")

#datafile<-"C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization\\ds1465_tx_All_Data_64_2016_0720_222352short.txt"
#val<-read.table(colClasses = c("Anon.Student.Id"="character"),datafile,sep="\t", header=TRUE,quote="\"")

#transfer original data
val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val$CF..ansbin.<-as.numeric(val$CF..ansbin.)
val<-val[val$CF..ansbin.!=-1,]

#REMOVE backslash and quotations in string
#val$KC..Default.<-cat(val$KC..Default.)
#val$KC..Default.<-gsub("\\\"","",val$KC..Default.)

val$KC..Default.<-as.numeric(regmatches(x =val$KC..Default.,regexpr("^[^-]*[^ -]",text = val$KC..Default.)))
val$KC..Default.<-ifelse(val$KC..Default.>17,val$KC..Default.-18,val$KC..Default.)
val$KC..Default.<-paste( val$KC..Default.,val$CF..Stimulus.Version.,gsub(" ","",val$CF..Correct.Answer.),sep="-")
index<-paste(val$Anon.Student.Id,val$KC..Default.)
val$Attempts<-ave(val$Outcome,index,FUN =function(x) cumsum(x>-1))


#create new dataframe for stroring the extracted columns from val
myData<-data.frame("Anon.Student.Id"=val$Anon.Student.Id,"Attempts"=val$Attempts,"KC..Default"=val$KC..Default.,"CF..ansbin."=val$CF..ansbin.)

StudentLevs<-unique(myData$Anon.Student.Id)
myData$StudentLevels<-factor(
  myData$Anon.Student.Id, levels=StudentLevs, labels=seq_along(StudentLevs)
)

QuestionLevs<-unique(myData$KC..Default)
myData$QuestionLevels<-factor(
  myData$KC..Default, levels=QuestionLevs, labels=seq_along(QuestionLevs)
)

tfData<-data.frame(as.numeric(myData$StudentLevels),as.numeric(myData$Attempts),as.numeric(myData$QuestionLevels),as.numeric(myData$CF..ansbin.))
header1<-"Student"
header2<-"Attempt"
header3<-"Question"
header4<-"Score"
header5<-"Resource"
names(tfData)<-c(header1,header2,header3,header4)
tfData$Resource<-0

numStudent<-max(unique(tfData$Student))
numStudentAttempt<-max(unique(tfData$Attempt))
numQuestion<-max(unique(tfData$Question))


library(caret)
#Train/Validation Split
#set.seed(42)
partition <- createDataPartition(y = tfData$Student, p = 0.8, list = F)
train<-tfData[partition, ]
tfData_test<-tfData[-partition, ]
idx.validation <- createDataPartition(y = train$Student, p = 0.25, list = FALSE) # Draw a random, stratified sample of ratio p of the data
tfData_validation <- train[idx.validation, ] #validation set with p = 0.8*0.25 = 0.2
tfData_training <- train[-idx.validation, ] #final train set with p= 0.8*0.75 = 0.6

library(writexl)
write_xlsx(tfData_test,"C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization\\1_tfData_test.xlsx")
write_xlsx(tfData_training,"C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization\\1_tfData_training.xlsx")
write_xlsx(tfData_validation,"C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization\\1_tfData_validation.xlsx")

