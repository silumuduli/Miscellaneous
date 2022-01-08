
# Correlation table with starts
corstat <- function(x){ 
 if (!require(pacman)) install.packages("pacman")
pacman::p_load(stargazer,xtable,Hmisc)
  x=na.omit(x)
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  xtable(Rnew)
  return(Rnew) 
}


# Summary Statistics
summarystat=function(x){
if (!require(pacman)) install.packages("pacman")
pacman::p_load(stargazer,xtable)
smsst=stargazer(data.frame(na.omit(x)),summary.logical = T, summary.stat = c("n", "mean","median", "sd","max","min"), type = "latex", title="Summary Statistics",digits=2)
smsst
}
