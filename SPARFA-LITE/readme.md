## SPARFA-LITE analysis

SPARFA-Lite aims at recovering the unknown, low-rank matrix Z that governs the learners’ responses to questions, solely from quantized (ordinal) graded learner responses. This analysis can be started from the main code file, SparfaliteAnaylysis.R.

#### Analysis Process
1. Transfer raw dataset to binary matrix (with binary values 0 and 1) without NA or NaN values. 
2. The litecv.R is used for cross-validation (K-fold cross-validation, the default k value is 4).
3. The tunning parameters use the sequence of {200,...,2000} by step size 200. 
4. The next step for the obatained Z matrix



#### The Sparfa-lite Statistical Model (in view of math problem)

The Quantized Matrix Completion (Q-MC)



#### Pending Questions:
Q1: The cost/loss function is 

    costfnlite <- function(Y,Z) {
      probm <- 1/(1+exp(-(2*Y-1)*Z))   # where is the algorithm
      cost <- sum(-log(probm[!is.nan(Y)]))
      return(cost)
    }
Where is the algorithm? 

Q2: The reasons behind for using the initial definition for some parameters: cvfold=4, cvt<-1, max_It <- 10000, tol <- 1e-6, lamcv <- seq(200,2000,100).
    
    
Q3: Any idea about the potential usage for the obtained Z matrix (the unknown, low-rank matrix) in future? Correlation analysis? Hierarchical Analysis? 



### References:
1. https://arxiv.org/pdf/1412.5968.pdf
2. https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6854548
3. https://www.csl.cornell.edu/~studer/papers/14JMLR-sparfa.pdf
