
#------------------------------------------------------Start of Preprocessing of the dataset------------------------------------------
# Import the dataset
setwd("C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization")
val<-read.table("ds1465_tx_All_Data_64_2016_0720_222352short.txt",sep="\t", header=TRUE,na.strings="NA",quote="\"",comment.char = "")


# Transfer original data
#1."correct"->1, "incorrect"->0, other->-1
#2. Remove other
val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val$CF..ansbin.<-as.numeric(val$CF..ansbin.)
val<-val[val$CF..ansbin.!=-1,]


#1. Extract the initial numeric string, e.g. the "14" of "14-2 The variance for a sample is..."
#2. Map numeric string (>18) into (1,17)
val$KC..Default.<-as.numeric(regmatches(x =val$KC..Default.,regexpr("^[^-]*[^ -]",text = val$KC..Default.)))
val$KC..Default.<-ifelse(val$KC..Default.>17,val$KC..Default.-18,val$KC..Default.)


#1. Concast the three columns KC..Default.,CF..Stimulus.Version. and CF..Correct.Answer. as one column (KC..Default.) by "-"
val$KC..Default.<-paste( val$KC..Default.,val$CF..Stimulus.Version.,gsub(" ","",val$CF..Correct.Answer.),sep="-")


#1. Compute the attempts of each student at each level of question
index<-paste(val$Anon.Student.Id,val$KC..Default.)
val$Attempts<-ave(val$Outcome,index,FUN =function(x) cumsum(x>-1))


# Create new dataframe (myData) for storing the extracted columns from val, which will be used in tensor factorization model. 
myData<-data.frame("Anon.Student.Id"=val$Anon.Student.Id,"Attempts"=val$Attempts,"KC..Default"=val$KC..Default.,"CF..ansbin."=val$CF..ansbin.)

#1. Get all levels of Anon.Student.Id
#2. Map the levels into numeric (This is used to match the model's requirements)
StudentLevs<-unique(myData$Anon.Student.Id)
myData$StudentLevels<-factor(
  myData$Anon.Student.Id, levels=StudentLevs, labels=seq_along(StudentLevs)
)

#1. Get all levels of Questions
#2. Map the levels into numeric (This is used to match the model's requirements)
QuestionLevs<-unique(myData$KC..Default)
myData$QuestionLevels<-factor(
  myData$KC..Default, levels=QuestionLevs, labels=seq_along(QuestionLevs)
)


#1. Create new dataframe (tfData) inluding the StudentLevels,Attempts,QuestionLevels,CF..ansbin.,
#2. Rename their headername as "Student","Attempt","Question","Score","Resource".(set the Resource column as 0)
#This is also used to match the example dataset format in tensor factorization model.
tfData<-data.frame(as.numeric(myData$StudentLevels)-1,as.numeric(myData$Attempts)-1,as.numeric(myData$QuestionLevels)-1,as.numeric(myData$CF..ansbin.))
header1<-"Student"
header2<-"Attempt"
header3<-"Question"
header4<-"Score"
header5<-"Resource"
names(tfData)<-c(header1,header2,header3,header4)
tfData$Resource<-0


#1. Compute the Dimensions for tensor (numStudent,numAttempt, and numQuestion)
numStudent<-length(unique(tfData$Student))
numAttempt<-length(unique(tfData$Attempt))
numQuestion<-length(unique(tfData$Question))


library(caret)
#To do Train/Validation Split by percentage 
# 20% of dataset as the tfData_test
# 20% of dataset as the tfData_validation
# 60% of dataset as the tfData_training
# This is just from my own idea, whatever method that you like to make it.
partition <- createDataPartition(y = tfData$Student, p = 0.8, list = F)
train<-tfData[partition, ]
tfData_test<-tfData[-partition, ]

idx.validation <- createDataPartition(y = train$Student, p = 0.25, list = FALSE) # Draw a random, stratified sample of ratio p of the data
tfData_validation <- train[idx.validation, ] #validation set with p = 0.8*0.25 = 0.2
tfData_training <- train[-idx.validation, ] #final train set with p= 0.8*0.75 = 0.6


#library(writexl)
#write_xlsx(tfData_test,"C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization\\data\\morf\\Quiz\\1_tfData_test.xlsx")
#write_xlsx(tfData_training,"C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization\\data\\morf\\Quiz\\1_tfData_training.xlsx")
#write_xlsx(tfData_validation,"C:\\Users\\Liang Zhang\\Desktop\\2020_Spring\\TensorFactorization\\data\\morf\\Quiz\\1_tfData_validation.xlsx")

#------------------------------------------------------End of Preprocessing------------------------------------
#------------------------------------------------------Start of Tensor Factorization Modeling------------------

#Import the Python library
library(reticulate)
#use_python("C:/Users/Liang Zhang/AppData/Local/Programs/Python/Python38")
use_python("C:/Pyhon39")


#Set the parameters for Tensor Factorization Model, 
data_str = "morf"
course_str = 'Quiz'
model_str = 'fdtf'

#Set initial parameters
if (course_str=="Quiz"){
  concept_dim = 15
  lambda_t = 0
  lambda_q = 0.01
  lambda_bias = 0
  slr = 0.5
  lr = 0.1
  max_iter = 30

  #The "validation" is one boolean value
  validation = FALSE
  
  #Three types of matircs for assessing prediction
  metrics = c("rmse", "mae", "auc")

  #Iterations, how many time times do you want to repeat the process, which may be necessary to do comparison for our results. 
  #This idea from original code
  num_folds = 1

  for(fold in 1:num_folds){
    
    #create the para tuple to indlude all parameters
    para = list("data_str"=data_str, "course_str"=course_str, "model_str"=model_str, "fold"=fold, "concept_dim"=concept_dim,
          "lambda_t"=lambda_t, "lambda_q"=lambda_q, "lambda_bias"=lambda_bias, "slr"=slr, "lr"=lr, "max_iter"=max_iter)

    para$metrics=metrics

    para$validation=validation

    #Set the views by course_str which will be used in tensor factorization model
    if(course_str == "Quiz"){
      views = "100"
    }else if(course_str == "Lecture"){
      views = "010"
    }else if(course_str == "Discussion"){
      views = "001"
    }else{
      print("IOError")
    }

    data=list("num_users"=numStudent,'num_quizzes'=numQuestion,'num_lectures'=0,'num_discussions'=0,'num_attempts'=numAttempt,"train"=tfData_training,'test'=tfData_test,'val'=tfData_validation)

    #Generate train_set, test_set for general training and testing
    # if it is for validation, we only use the first 30 attempts for cross validation to
    # do the hyperparameter tuning.
    # This idea is from original code
    validation_limit=30

    library(tidyverse)
    train_data<-data.frame()
    if(validation){
      train_data<-data$train %>% filter(Attempt<validation_limit)
    }else{
      train_data<-data$train
    }

    val_data<-data.frame()
    if(validation){
      val_data<-data$val %>% filter(Attempt<validation_limit)
    }else{
      val_data<-data$val
    }

    test_set<-data.frame()
    if(validation){
      test_set<-data$test %>% filter(Attempt<validation_limit)
    }else{
      test_set<-data$test
    }

    if(validation){
      num_attempts<-validation_limit
    }

    
    train_data_list<-train_data[1:nrow(train_data),]
    val_data_list<-val_data[1:nrow(val_data),]
    test_set_list<-test_set[1:nrow(test_set),]

    #Create the train_data_Tuple, val_data_Tuple, and test_set_Tuple, this is the Python tuple, which will be used in the Python-version of tensor factorization model 
    np <- import("numpy", convert=FALSE)
    train_data_Tuple <- np$array(train_data_list)
    val_data_Tuple<-np$array(val_data_list)
    test_set_Tuple<-np$array(test_set_list)

    #create the config including all parameters for tensor factorization model 
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
      validation=FALSE,
      train=train_data_Tuple,
      val=val_data_Tuple,
      test=test_set_Tuple,
      convert = TRUE
    )

    #Use the config the build the FDTF model
    if (model_str=="fdtf"){
      source_python("fdtf.py")
      print("create model")
      model<-FDTF(config)
      print("done creation of model")
      }

    #We set the validation as false by default
    #if we don't need the validation, we can add validation data into test data
    if(validation){
      test_data<-config$val
    }else{
      test_data<-rbind(config$test,config$val)
      model$train_data<-rbind(model$train_data,config$val,config$test)
    }

    test_start_attempt<-NULL
    # since the test start attempt for different students are different,
    # we need to find the first testing attempt, and add all lectures and discussion before
    # test_start_attempt into train_data
    test_data<-test_data[order(test_data[,2],decreasing=FALSE),]

    #We set the test_data[,5]=0 by default
    #if(test_data[,5]==0){
      test_data2<-test_data[test_data[,5]==0,]
      test_start_attempt<-test_data2[,2][1]
    #}

    
    #Start of the training dataset, by different dimensions of attempts, model$training
    # we still need to specify the current_test_attempt and lr
    source_python("RestartTraining.py")

    for (test_attempt in test_start_attempt:model$num_attempts){
      model$current_test_attempt<-test_attempt
      model$lr<-lr
      
      #initialize the bias for each attempt, student, question, lecture, or discussion
      restart_training(model)
      print("Start Training")
      #def training(self) in fdtf.py
      train_perf = model$training()

      test_set<-data.frame();
      test_set<-test_data[test_data[,2]==test_start_attempt,]
      model$train_data<-rbind(model$train_data,test_set)
    }
    
    #Output the best Q matrix
    print(model$Q)

    #Use the test dataset to do the testing, model$testing
    
    test_set <- data.frame(apply(test_set, 2, function(x) as.numeric(as.character(x))))

    test_perf <-model$testing(test_set)

    #Compute the final results of prediction, model$eval
    perf_dict<-dict()

    overall_perf<-model$eval(model$test_obs_list,model$test_pred_list)
    if(validation){
      perf_dict$val<-overall_perf
    }else{
      perf_dict$test<-overall_perf
    }

    print(perf_dict)

  }

}

