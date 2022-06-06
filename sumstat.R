
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
  print(type = type, caption.placement = 'top',
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


### Cross-validation of Models
crossval_lm=function(data,type="latex"){
if (!require(pacman)) install.packages("pacman")
pacman::p_load(caret,Metrics,xtable,texreg)
                 
dt=na.omit(data)
n <- nrow(dt)
ntest <- round(0.2*n)
ntrain <- n - ntest

train_rows <- sample(1:n, ntrain)
dt_train <- dt[train_rows,]
dt_test <- dt[-train_rows,]


m <- lm(y ~ ., data = dt_train)
rmse <- rmse(dt_test$y, predict(m, dt_test))
mae <- mae(dt_test$y, predict(m, dt_test))
mape=mape(dt_test$y, predict(m, dt_test))
a1=rmse
a2=mae
a3=summary(m)$r.squared


pred <- vector("numeric", nrow(mtcars))
rsq=vector("numeric", nrow(mtcars))
for(i in 1:nrow(dt))
{
  # Fit model to all observations except observation i:
  m <- lm(y ~ ., data = dt[-i,])
  
  # Make a prediction for observation i:
  pred[i] <- predict(m, dt[i,])
  rsq[i]=summary(m)$r.squared
}


rmse <- rmse(dt$y, pred)
mae <- mae(dt$y, pred)
mape=mape(dt$y, pred)
b1=rmse
b2=mae
b3=mean(rsq)





tc <- trainControl(method = "repeatedcv", number = 10, repeats = 500)
mt2 <- train(y~ .,
           data =dt,
           method = "lm",
           trControl = tc)

c1=as.numeric(mt2$results[2])
c2=as.numeric(mt2$results[4])
c3=as.numeric(mt2$results[3])


tc <- trainControl(method = "boot",
                   number = 999)
mt3 <- train(y~ .,
             data =dt,
             method = "lm",
             trControl = tc)

d1=as.numeric(mt3$results[2])
d2=as.numeric(mt3$results[4])
d3=as.numeric(mt3$results[3])
v1=c(a1,a2,a3)
v2=c(b1,b2,b3)
v3=c(c1,c2,c3)
v4=c(d1,d2,d3)
vdt=data.frame(v1, v2,v3,v4)
colnames(vdt)=c("20 % Testing","One-point Validation","k-fold Validation","Bootstarpping")
rownames(vdt)=c("RMSE", "MAE","R2")
print(xtable(vdt, digits=2),type)
}




# Elastic Net
net_lm=function(data){
 if (!require(pacman)) install.packages("pacman")
pacman::p_load(caret,Metrics,xtable,texreg)
dt=na.omit(data)
tc <- trainControl(method = "repeatedcv",
                   number = 10, repeats = 100,
                   selectionFunction = "oneSE")
mn <- train(y ~ .,
            data = dt,
            method = "glmnet", 
            tuneGrid = expand.grid(alpha = seq(0, 1, 0.1),
                                   lambda = seq(0, 10, 0.1)),
            metric = "RMSE",
            trControl = tc) 



# Print the RMSE and MAE for the best model:
print(mn$results[which(rownames(mn$results) == rownames(mn$bestTune)),])

# Print the coefficients of the best model:
print(coef(mn$finalModel, mn$bestTune$lambda, mn$bestTune$alpha))
}



##### Exporting to CSV
save_csv=function(data,filename="CSVFile.csv"){
 if (!require(pacman)) install.packages("pacman")
 pacman::p_load(openxlsx,readxl,seasonal,dplR,BCDating,mFilter, ggplot2, utils,astsa,RColorBrewer)
utils::write.table(data, filename,na = "",row.names =FALSE,col.names = TRUE,append = FALSE,sep = ",")
}


# Read excel
## d=list("d1 name"=data,"d2 name"=d2)
save_excel=function(d,filename="File.xlsx"){
 if (!require(pacman)) install.packages("pacman")
 pacman::p_load(openxlsx,readxl,seasonal,dplR,BCDating,mFilter, ggplot2, utils,astsa,RColorBrewer)
openxlsx::write.xlsx(d, filename,row.names=T, append=T)
}



paneldata=function(data,pname,ptime){
if (!require(pacman)) install.packages("pacman")
pacman::p_load(openxlsx,readxl,plm)
colnames(data)[which(colnames(data)==pname)]="Panel"
colnames(data)[which(colnames(data)==ptime)]="Time"
pdata= plm::pdata.frame(data, index = c("Panel", "Time"), drop.index = TRUE)
return(pdata)
}



## Dynamic Panel Reg
pgmmreg=function(model){
 if (!require(pacman)) install.packages("pacman")
pacman::p_load(openxlsx,readxl,plm,texreg)
ss=summary(model, robust =TRUE)
coefficient.names <- rownames(ss$coefficients)  # extract coef names
coefficients <- ss$coefficients[,1]  # extract coefficient values
standard.errors <- ss$coefficients[,2]  # extract standard errors
significance <- ss$coefficients[,4]  #extract p-values
n<-  nobs(model) # extract log likelihood
ar1 <- ss$m1$p.value  # extract AIC
ar2 <- ss$m2$p.value  # extract BIC
sargan <- ss$sargan$p.value  # extract number of observations
gof <- c(n, ar1, ar2,sargan)  # create a vector of GOF statistics
gof.names <- c("Observations", "AR(1) test p-value", "AR(2) test p-value", "Sargan test p-value")  # names of GOFs
decimal.places <- c(FALSE, TRUE, TRUE, TRUE)  # last one is a count variable

tr <- texreg::createTexreg(coef.names = coefficient.names,
                   coef = coefficients,
                   se = standard.errors,
                   pvalues = significance,
                   gof.names = gof.names,
                   gof = gof,
                   gof.decimal = decimal.places)
 return(tr)
 }

################################################################
## Xtile in R
xtile=function(x,n,labels){
  data=data.frame(y=x)
  x=data$y
  vTert = quantile(x, c(0:n/n), na.rm=T)
  xtl = with(data, cut(x, vTert, include.lowest = T, labels=labels))
  return(xtl)
}

# an Example
#x=rnorm(10)
# n=3
# labels= c("l","m","h")
# xtile(x,3,labels=c("l","m","h"))
################################################################






##################################################################
# Panel xtile in R
pxtile=function(x,by=y,n){
df=data.frame(x=x,by=by)
m=length(df$x)
xtl=1:m
fun=function(x){quantile(x, probs=1:n/n, na.rm=T)}
dff=aggregate(df$x, by=list(df$by), FUN=fun)
colnames(dff)[1]="by"
dfff=merge(df, dff, by="by")
for (i in which(is.na(dfff$x.x))){
xtl[i]=NA
}
for (i in which(!is.na(dfff$x.x))){ 
xtl[i]=min(which(as.vector(dfff[,3][i,])>=dfff$x.x[i]))
}
return(xtl)
}


# An Example
#df <- data.frame(team=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A','B', 'B', 'B', 'B', 'B', 'B', 'B', 'B','C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'),wins=c(2, 4, 4, 5, 7, 9, NA, 13, 15, 15, 14, 13,11, 9, 9, 8, 8, 16, 19, NA, 24, 20, 19, 18))
#pxtile(x=df$wins,by=df$team,n=3)

prob=function(x,lower=1, upper=2){
dens=density(x)
probval=integrate(approxfun(dens), lower=lower, upper=upper)
return(probval)
}

