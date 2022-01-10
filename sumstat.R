
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
  return(Rnew)
}


# Summary Statistics
summarystat=function(x,type="latex"){
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(stargazer,xtable,dplyr,tidyverse)
  smsst=stargazer(data.frame(na.omit(x)), summary.stat = c("n", "mean","median", "sd","max","min"), type = "text", title="Summary Statistics",digits=2)
 summary <-
  x %>%
  # Keep numeric variables
  select_if(is.numeric) %>%
  # gather variables
  gather(variable, value) %>%
  # Summarize by variable
  group_by(variable) %>%
  # summarise all columns
  dplyr::summarise(N = sum(!is.na(value)),
            `Mean` = mean(value,na.rm = T),
            `Median` = median(value,na.rm = T),
            `SD` = sd(value,na.rm =T),
            `Min.` = min(value,na.rm=T),
            `Max.` = max(value,na.rm = T))


foo <- xtable(summary, digits = 2, caption = 'Summary Statistics') %>%
  print(type = "latex", caption.placement = 'top',
        html.table.attributes = "",
        include.rownames = FALSE,
        format.args = list(big.mark = ","))
}


# Correlation Plot
corplot=function(x){
if (!require(pacman)) install.packages("pacman")
pacman::p_load(stargazer,xtable,corrplot)
 x=na.omit(x)
cor_5 <- rcorr(as.matrix(x))
M <- cor_5$r
p_mat <- cor_5$P
corrplot(cor(x,use = "complete.obs"), method = "color", type = "lower",addCoef.col = "black", tl.col = "darkblue", diag = FALSE,p.mat = p_mat, sig.level = 0.05, insig = "blank")
}


## Creation of Dummy Variable
dummy=function(x,d=1,e=0){
ifelse(x,d,e)
}


TS=function(x,t){
year=as.numeric(year(t))[1]
f=length(unique(month(t)))
if (f==1){
n=1
fr=1
}
if (f==4){
  n=month(t)[1]/3
  fr=4
}
if (f==12){
  n=month(t)[1]
  fr=12
}
d=ts(x,start=c(year,n),frequency = fr)
return(d)
}
