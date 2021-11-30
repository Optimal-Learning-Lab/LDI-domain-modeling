

testKCmodel<-function (iter,posKC,KCthreshm,RSVDcomp,valtemp){

  #==========================Reduce matrix================================
  reducedmatrix<-rsvd(df,RSVDcomp)
  rownames(reducedmatrix$v)<-colnames(df)
  #print(reducedmatrix$v)
  
  #==========================cluster matrix==============================
  cm <- kmeans(reducedmatrix$v,centers=posKC) #(cmeans(reducedmatrix$v,centers=posKC))
  
  #print(cm$membership)

  #=================extrapolate KC model==============
   #if(usethresh) {
   #  KCmodel <-
   #    as.data.frame(sapply(apply(cm$membership, 1, function(x)
   #      which(x > KCthresh)), paste, collapse = " "))
   #} else{
  #   KCmodel <-
    #   as.data.frame(sapply(apply(cm$membership, 1, function(x)
   #      which(x == max(x))), paste, collapse = " "))
   #}
  KCmodel<-as.data.frame(cm$cluster)
  
  #print(KCmodel)
  
  colnames(KCmodel)[1] <- "AC"
  KCmodel$AC<-as.character(KCmodel$AC)
  KCmodel$rows<-rownames(KCmodel)
  #print(KCmodel$rows)
  
  valtemp<-merge(valtemp,
                 KCmodel,
                 by.y = 'rows',
                 by.x = 'KC..Default.',
                 sort = FALSE)
  valtemp<-valtemp[order(valtemp$Anon.Student.Id,valtemp$Time),]
  #=================Test===============================
  
  modelob<-LKT(data=rlvl(valtemp),components=c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.","AC","AC"),
               features=c("intercept","intercept","logsuc$","logfail$","logsuc$","logfail$"),
               fixedpars=c(.9,.7),interc=TRUE,verbose=FALSE)

  valtemp[,("AC"):=NULL]
  trows<-KCmodel$rows
  KCmodel$AC<-sample(KCmodel$AC)
  KCmodel$rows<-trows
  valtemp<-merge(valtemp,KCmodel,
                 by.y = 'rows',by.x='KC..Default.',sort=FALSE)
  valtemp<-valtemp[order(valtemp$Row),]
  modelob2<-LKT(data=rlvl(valtemp),components=c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.","AC","AC"),
                features=c("intercept","intercept","logsuc$","logfail$","logsuc$","logfail$"),
                fixedpars=c(.9,.7),interc=TRUE,verbose=FALSE)
  modelob3<-LKT(data=rlvl(valtemp),components=c("Anon.Student.Id","KC..Default."),
                features=c("intercept","intercept"),
                fixedpars=c(.9,.7),interc=TRUE,verbose=FALSE)
  #cat(paste(posKC,KCthreshm,RSVDcomp,modelob$r2,
  #          modelob2$r2,mean(modelob$subjectrmse$x),
  #          mean(modelob2$subjectrmse$x),(modelob$r2-modelob2$r2)/(modelob2$r2),(modelob$r2-modelob2$r2)/(modelob2$r2-modelob3$r2),"\n",sep=","))
  x<<-rbind(x,c(posKC,RSVDcomp,modelob$r2,(modelob$r2-modelob2$r2)/(modelob2$r2),(modelob$r2-modelob2$r2)/(modelob2$r2-modelob3$r2)))

  y1<<-modelob$coefs
  y2<<-modelob2$coefs
  mod<<-KCmodel
}

