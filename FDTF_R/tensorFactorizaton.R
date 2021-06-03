
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

numStudentMax<-max(unique(tfData$Student))
numAttemptMax<-max(unique(tfData$Attempt))
numQuestionMax<-max(unique(tfData$Question))

print(paste("numStudent: ",numStudentMax,sep=""))
print(paste("numStudentAttempt: ",numAttemptMax,sep=""))
print(paste("numQuestion: ",numQuestionMax,sep=""))

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
write_xlsx(tfData_test,"C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization\\data\\morf\\Quiz\\1_tfData_test.xlsx")
write_xlsx(tfData_training,"C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization\\data\\morf\\Quiz\\1_tfData_training.xlsx")
write_xlsx(tfData_validation,"C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization\\data\\morf\\Quiz\\1_tfData_validation.xlsx")


###Dimensions for tensor

#dimensions for tensor
numStudent=numStudentMax+1
numAttempt=numAttemptMax+1
numQuestion=numQuestionMax+1

#################################################################################################################
library(reticulate)
use_python("C:/Users/Liang Zhang/AppData/Local/Programs/Python/Python38")
#repl_python()

#use the def run_morf()
data_str = "morf"
course_str = 'Quiz'
model_str = 'fdtf'

print(course_str=="Quiz")

tfData_training<-as.matrix(sapply(tfData_training, as.numeric))
tfData_validation<-as.matrix(sapply(tfData_validation, as.numeric))

#Store Each Row of a Data Frame in a List
colnames(tfData_training)<-NULL
colnames(tfData_validation)<-NULL
rownames(tfData_training)<-NULL
rownames(tfData_validation)<-NULL

tfData_training_list<-tfData_training[1:nrow(tfData_training),]
#tfData_validation_list<-split(tfData_validation, seq(nrow(tfData_validation)))
tfData_validation_list<-tfData_validation[1:nrow(tfData_validation),]

np <- import("numpy", convert=FALSE)
tfData_training_Tuple <- np$array(tfData_training_list)
tfData_validation_Tuple<-np$array(tfData_validation_list)

#tfData_training_array<-np_array(array(tfData_training),dtype = NULL, order = "C")

if (course_str=="Quiz"){
  concept_dim = 15
  lambda_t = 0
  lambda_q = 0.01
  lambda_bias = 0
  slr = 0.5
  lr = 0.1
  max_iter = 30

  validation = FALSE
  metrics = c("rmse", "mae", "auc")

  num_folds = 1

  for(fold in 1:num_folds){
    #print(fold)

    para = list("data_str"=data_str, "course_str"=course_str, "model_str"=model_str, "fold"=fold, "concept_dim"=concept_dim,
          "lambda_t"=lambda_t, "lambda_q"=lambda_q, "lambda_bias"=lambda_bias, "slr"=slr, "lr"=lr, "max_iter"=max_iter)

    para$metrics=metrics

    para$validation=validation

    #print(para)

    ###################################def run_fdtf_exp###########################
    if(course_str == "Quiz"){
      views = "100"
    }else if(course_str == "Lecture"){
      views = "010"
    }else if(course_str == "Discussion"){
      views = "001"
    }else{
      print("IOError")
    }

    #Import the data set

    data=list("num_users"=numStudent,'num_quizzes'=numQuestion,'num_lectures'=0,'num_discussions'=0,'num_attempts'=numAttempt,"train"=tfData_test,'test'=tfData_training,'val'=tfData_validation)
    #data=dict('views'=views, convert = TRUE)

    #print(data)

    #add array of train to dict

    config = dict(
      views= views,
      num_users= data$num_users,
      num_attempts= data$num_attempts,
      num_questions= data$num_quizzes,
      num_concepts= concept_dim,
      lambda_t= lambda_t,
      lambda_q= lambda_q,
      lambda_bias=lambda_bias,
      lr= lr,
      max_iter= max_iter,
      tol= 1e-3,
      slr= slr,
      metrics= metrics,
      validation=TRUE,
      train=tfData_training_Tuple,
      val=tfData_validation_Tuple,
      convert = TRUE
    )

    #py_dict(keys, values, convert = FALSE)
    #py_dict('views',views)

    #fdtf_config<<-config
    #print(config)

    #s <- import("os")

    source_python("data_helper.py")

    #model_config = fdtf_config(
    #  data_str, course_str, views, concept_dim, fold, lambda_t, lambda_q,
    #  lambda_bias, slr, lr, max_iter, metrics, validation)

    if (model_str=="fdtf"){
      source_python("fdtf.py")
      model<-FDTF(config)
      #model

    }

  }

}





#reticulate::py_config()
#devtools::session_info()

#Option one
#devtools::install_github("rstudio/reticulate")

#source_python()

#use_virtualenv("myenv")

#main FDTF code



#repl_python()



#library(hash)



#reticulate::py_config()




















