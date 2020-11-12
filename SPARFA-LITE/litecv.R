# cv for sparfalite

# "cross validation" - this code loops over different
# values of the lambda parameter, which controls the rank of the
# underlying matrix we want to recover

source("sparfalite.R")

cvlite <- function(Y){
	
	cvfold <- 4  #use K-fold cross-validation, k=4
	cvt <- 1 
	siz <- dim(Y)
	lamcv <- seq(200,2000,100)
	mask_orig <- is.nan(Y)
	# this code records correct prediction percentage rather than things
	# like AUC, you can change to/add other metrics
	correc <- matrix(0,1,length(lamcv))
	
	for (cv in seq(1,cvt)){
		
		set.seed(cv) 
	  # Randomly split the data set into k-subsets (or k-fold), k=4
		foldi <- matrix(sample(1:cvfold,siz[1]*siz[2],replace=T),siz[1],siz[2])
		
		for (fo in seq(1,cvfold)){   # Specify the group as the test dataset
			
			#print(fo)
			mask_temp <- foldi==fo
			Ytemp <- Y
			Ylo <- Y
			Ytemp[mask_temp] <- NaN    # 1->NaN, keep other 3 groups for training 
			Ylo[!mask_temp] <- NaN     # !1->NaN, keep other group 1 for testing
			Ylo_v <- Ylo[!is.nan(Ylo)] # 1->True, the group 1, keep keep elements in the group 1 as the test dataset (vector)
			
			for (lam in seq(1,length(lamcv))){
				
				Zdec <- sparfalite(Ytemp,lamcv[lam])
				pred <- Zdec>0
				Ypred_v <- pred[!is.nan(Ylo)]
				cor_pct <- sum(Ypred_v == Ylo_v)/length(Ypred_v)
				correc[lam] <- correc[lam] + cor_pct
				
			}
				
		}
		
	}
	
	correc <- correc/cvfold/cvt
	#plot(correc)
	ord <- order(-correc)
	optlam <- lamcv[ord[1]] #got the minimum value by tunning parameters
	Z <- sparfalite(Y,optlam)
	#print(rankMatrix(Z))
	return(Z)
	
}


