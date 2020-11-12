
#==========================Data Preparation==============================

setwd("C:\\Users\\Liang Zhang\\Desktop\\Dissertation\\GA\\R_code\\Convariance\\CovarianceCodeLibra")
val<-read.table("ds1465_tx_All_Data_64_2016_0720_222352.txt",sep="\t", header=TRUE,na.strings="NA",quote="",comment.char = "")

val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))

val<-val[val$CF..ansbin.!=-1,]
val$KC..Cluster.<-regmatches(x =val$KC..Cluster.,regexpr("\\s(.*)",text = val$KC..Cluster.))
#val$KC..Cluster.<-as.numeric(regmatches(x =val$KC..Cluster.,regexpr("^[0-9]+",text = val$KC..Cluster.)))

#val$KC..Cluster.<-ifelse(val$KC..Cluster.>17,val$KC..Cluster.-18,val$KC..Cluster.)
#val$KC..Cluster.<-paste( val$KC..Cluster.,val$CF..Stimulus.Version.,val$CF..Correct.Answer.,sep="-")
#Step 0
aggdata<-aggregate(val$CF..ansbin.,by=list(val$KC..Cluster.,val$Anon.Student.Id),FUN=mean)
colnames(aggdata)<-c('KC..Cluster.','Anon.Student.Id','CF..ansbin.')

aggdata<-aggdata[with(aggdata,order(KC..Cluster.)),]

library(reshape2)
mydata<-dcast(aggdata, KC..Cluster. ~ Anon.Student.Id, value.var="CF..ansbin.") #reshape to wide data format
#mydata<-reshape(df1, idvar = "Anon.Student.Id", timevar = "KC..Cluster.", direction = "wide")
#mydata$KC..Cluster.<-as.factor(mydata$KC..Cluster.)

#order by question number
#rowLabelsNumbers<-regmatches(x =mydata$KC..Cluster.,regexpr("^[0-9]+",text = mydata$KC..Cluster.))
#mydata$rowLabelsNumbers<-as.numeric(rowLabelsNumbers)
#mydata<-mydata[with(mydata,order(rowLabelsNumbers)),]
#mydata$rowLabelsNumbers<-NULL

rownames(mydata)<-mydata[,1]
mydata<-mydata[,-1]

logit <- function(x, min=0, max=1)
{
  p <- (x-min)/(max-min)
  log(p/(1-p))
}

mydata<-apply(mydata,1:2,logit)

mydata[which(mydata>2)] <- 2
mydata[which(mydata<(-2))] <- -2


###Q1: NAN Value problem
for(i in 1:ncol(mydata)){
  mydata[is.na(mydata[,i]), i] <- mean(mydata[,i], na.rm = TRUE)}

#Step 1 and Step 2
#covMydata<-cov(t(mydata))

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

#remames by labels
#rowLabelsNumbersNew<-rownames(mydata)
#rownames(df)<-rowLabelsNumbersNew
#colnames(df)<-rowLabelsNumbersNew

###Q2: NAN Value problem
#mydata<-scale(mydata) 
#mydata[is.nan(mydata)] <-0

#write.table(df1,file="df.txt",quote=F,col.name=F,row.names=F)
#DIANA, Divisive Analysis, rarely used
#HAC, Hierarchical Agglomerative Clustering
#The distance matrics: three methods to measure the distance (or similarity) between two clusters of observations:
#1.Single Linkage; 2.Complete Linkage;3.Average Linkage;4.Centroid Method;5.Ward's Method;

rownames(df)<-1:18 #rownames(mydata)
colnames(df)<-rownames(mydata)
print(rownames(mydata))

#=============RSVD====================
library(rsvd)
reducedmat2<-rsvd(df,2)
rownames(reducedmat2$v)<-rownames(mydata)
View(reducedmat2$v)

cor(c(reducedmat2$v[,1]),c(reducedmat2$v[,2]))

library(e1071)
cm <- cmeans(reducedmat2$v,centers=3)

library(factoextra)
fviz_cluster(list(data = reducedmat2$v, cluster=cm$cluster), 
             ellipse.type = "norm",
             ellipse.level = 0.68,
             palette = "jco", repel=TRUE,
             ggtheme = theme_minimal(),xlab="",ylab="")

#=============SVD=============================================
df.svd<-svd(df)
df.eign<-df.svd$d
u<-df.svd$u
v<-df.svd$v

svd.scree <- function(svd.obj, subr=NULL, maintitle="Scree Plot", axis.title.x="Singular Vectors", axis.title.y="Percent Variance Explained") {
  if(is.list(svd.obj) & all(names(svd.obj) %in% c("u","d","v"))) {
    print("Your input data is treated as a SVD output, with u, d, v corresponding to left singular vector, singular values, and right singular vectors, respectively.")
  } else {
    print("Your input data is treated as a vector of singular values. For example, it should be svd.obj$d from a SVD output.")
    svd.obj = list(d=svd.obj)
  }
  
  print("Scree Plot")
  
  if(is.null(names(svd.obj$d))) {
    names(svd.obj$d) = paste0("V",1:ncol(svd.obj$v))
  }
  
  pve = svd.obj$d^2/sum(svd.obj$d^2) * 100
  pve = data.frame(names=names(svd.obj$d), pve)
  pve$names = factor(pve$names, levels=unique(names(svd.obj$d)))
  g = ggplot(pve, aes(names, pve)) + geom_point() + theme_bw()
  gout = g + ylim(0,NA) +
    labs(title=maintitle, axis.title.x=axis.title.x, axis.title.y=axis.title.y)
  
  if(!is.null(subr)) {
    gsub = g + coord_cartesian(xlim = c(0.5, subr+.5)) + ylim(min(pve$pve[1:subr])-1, max(pve$pve[1:subr])+1) +
      labs(title=paste("First",subr,"singular values"), axis.title.x=axis.title.x, axis.title.y=axis.title.y)
    gout = grid.arrange(gout, gsub, nrow=1)
  }
  
  return(gout)
}

#scree plot
svd.scree(df.svd, subr = NULL, maintitle = "Scree Plot of SVD",
          axis.title.x = "Components",
          axis.title.y = "Proportion of Variance (%)")

pve <- df.svd$d^2/sum(df.svd$d^2)*100 #Proportion of Variance
plot(x=1:nrow(df),y=pve,main="Scree Plot of SVD",type="p",xlim=c(1,nrow(df)),ylim=c(0,65),xlab="Components", ylab="Proportion of Variance (%)")
axis(1, at = seq(1,nrow(df),by=1))
axis(2, at = seq(0,65,by=5))
grid()

#
library(GGally)
svd.scatter <- function(svd.obj, r=NULL, group=NULL, weights=NULL, alpha=0.7, axisLabels="none", ...) {
  if(is.list(svd.obj) & all(names(svd.obj) %in% c("u","d","v"))) {
    print("Your input data is treated as a SVD output, with u, d, v corresponding to left singular vector, singular values, and right singular vectors, respectively.")
  } else {
    print("Your input data is treated as (typically, right) singular vectors. For example, it should be svd.obj$v  from a SVD output.")
    svd.obj = list(v=svd.obj)
  }
  
  print("Multiple Scatter Plots")
  
  if(is.null(dimnames(svd.obj$v))) {
    colnames(svd.obj$v) = paste0("V",1:ncol(svd.obj$v))
    rownames(svd.obj$v) = paste0("ID ",1:nrow(svd.obj$v))
  }
  
  if(is.null(r)) r=ncol(svd.obj$v)
  if(r > 15) print("It may not be good to visualize too many singular vectors or principal components at one.")
  if(is.null(group)) group=rep(1,nrow(svd.obj$v))
  if(!is.null(weights)) {
    if(weights == "sv") {
      print("Singular values are used as weights.")
      weights = svd.obj$d[1:r]
    } else if (length(weights) != r) {
      stop("The length of weights must equal r.")
    }
  }
  
  if(!is.null(weights)) {
    mv = as.data.frame(weights * svd.obj$v[, 1:r])
    mv = cbind(mv, group)
  } else {
    mv = as.data.frame(svd.obj$v[, 1:r])
    mv = cbind(mv, group)
  }
  
  g = ggpairs(mv, columns = 1:(r), color="group", alpha=alpha, axisLabels=axisLabels, diag=list(continuous="density"), upper=list(continuous='blank'), ...)
  return(g)
}

svd.scatter(df.svd)
svd.scatter(df.svd, r=3, alpha=.5)

plot(df.svd$u[, 1]~ df.svd$u[, 2], main = "SVD", xlab = "U1", ylab = "U2", pch=19,cex=2,col="lightblue",data=df.svd)
text(df.svd$u[, 1] ~df.svd$u[, 2], labels=colnames(df),data=df, cex=0.9, font=2)
text(df.svd$u[, 1] ~df.svd$u[, 2], labels=rownames(df),data=df, cex=0.9, font=2)

library(svdvisual)
svd3dplot(df,iimage=TRUE)


#=============PCA=========================================
#res<-rcorr(df,type="spearman")
#sd1=scale(df)
sd1=df
#sd1=t(mydata)  # maybe use mydata instead of df
df.pca1<-princomp(sd1,cor=FALSE, scores = TRUE) #reference: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#princomp() uses the spectral decomposition approach
#The functions prcomp() and PCA()[FactoMineR] use the singular value decomposition (SVD).

# Results for Variables
var <- get_pca_var(df.pca1)
View(var$coord) # Coordinates
View(var$contrib) # Contributions to the PCs
View(var$cos2)   # Quality of representation
summary(df.pca1)

#Principal components are created in order of the amount of variation they cover: PC1 captures the most variation, PC2 — the second most, and so on. 
#Kaiser rule: pick PCs with eigenvalues of at least 1.
#Proportion of variance plot: the selected PCs should be able to describe at least 80% of the variance.

screeplot(df.pca1,npcs=nrow(df),type='lines',main='ScreePlot of PCA',lwd=2) #the amount of variation
abline(h=1,lty=2,col='orange')
library(factoextra)
fviz_eig(df.pca1,ncp=nrow(df),addlabels=TRUE,choice="eigenvalue") # Proportion of Variance

PoV <- df.pca1$sdev^2/sum(df.pca1$sdev^2)*100 #Proportion of Variance
plot(x=1:nrow(df),y=PoV,main="Scree Plot of PCA",type="p",xlim=c(1,nrow(df)),ylim=c(0,65),xlab="Components", ylab="Proportion of Variance (%)")
axis(1, at = seq(1,nrow(df),by=1))
axis(2, at = seq(0,65,by=5))
grid()


#Loading plot
fviz_pca_ind(df.pca1,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(df.pca1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#=============Andrews Plot================
library(andrews) # reference: https://cran.r-project.org/web/packages/andrews/andrews.pdf
df.nor<-scale(df)
andrews(df.nor,type=1,clr=nrow(df),ymax=4,step=100,main="Andrews' Curves")

#andrews(t(scale(t(mydata))),type=1,clr=10,ymax=15,main="Andrews' Curves")

#library(pracma)
#andrewsplot(df.nor, f=as.integer(df), style = "cart") #style=`cart' or `pol'.

#===========================Hierarchical Clustering====================

#Ditance/Dissimilarity matrix
d<-dist(df,method="euclidean")
hc1 <- hclust(d, method = "complete" )
heatmap.2(df,cexRow=1,cexCol=1,symm=T)

pdf("heatmap.pdf",width=25,height=25)

heatmap.2(df,cexRow=.6,cexCol=.6,symm=T)
dev.off()






image(as.matrix(d),d)

#Output the legend
legend<-data.frame("numQues"=rowLabelsNumbersNew,"question"=rownames(mydata))

library(flextable)
ft <- flextable(legend)
ft <- align(ft, align = "left", part = "all")
ft <- autofit(ft)
print(ft)
save_as_image(ft, path = "legend.png")

#==========================Correlation Analysis========================

library(Hmisc)
library(ggplot2)
res<-rcorr(df,type="spearman")
#extract the p-values or the correlation coefficients
cor<-res$r
corp<-res$P

#output cor
corOut<-cbind(rownames(cor),cor)
write.table(corOut,file="cor.txt",sep="\t",row.names=F, col.names=T,eol = "\n", quote=F)

#search elements that is larger than 0.61 in cor matrix
corSearch<-which( cor > 0.80, arr.ind=T )
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
ft <- flextable(corSearch)
ft <- align(ft, align = "left", part = "all")
ft <- autofit(ft)
print(ft)
save_as_image(ft, path = "corSearch.png")

#Remove the NAN value
#cor<-na.exclude(cor)
#cor<-na.omit(cor)
#cor[is.nan(cor)] <-0

#Visualize correlation matrix
#######
library(corrplot)
#corrplot(cor,method="number")
#corrplot(cor,type="upper")
png("Correlation1.png",width = 1000, height = 1000)
corrplot1<-corrplot(cor,title="Corretaion Analysis 01", type = "upper", p.mat = res$P, sig.level = 0.05, insig = "blank")
dev.off()

#######Contour Plots: https://warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/matrix_contour/

library(gplots)
#filled.contour(cor, main="Correlations of the KCs")
#Labeling the Axes
matrix.axes <- function(data) {
  # Do the rows, las=2 for text perpendicular to the axis
  x <- (1:dim(data)[1] - 1) / (dim(data)[1] - 1);
  axis(side=1, at=x, labels=rownames(data), las=2);
  # Do the columns
  x <- (1:dim(data)[2] - 1) / (dim(data)[2] - 1);
  axis(side=2, at=x, labels=colnames(data), las=2);
  # Add a solid black grid
  grid(nx=(dim(data)[1]-1), ny=(dim(data)[2]-1), col="black", lty="dotted");
}
png("Correlation2.png",width = 1000, height = 1000)
corrplot2<-filled.contour(cor,plot.axes=matrix.axes(cor), main="Corretaion Analysis 02")
dev.off()

#==========================Hierarchical cluster analysis:enhanced with dendextend package R========================
library(dendextend)


#==========================PCA Analysis=============================================







