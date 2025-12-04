##Correlation Matrix
corstat <- function(df, var_list, type = "latex", file_out = "correlation_table.tex") {
  # Load required packages
  if (!require(dplyr))    install.packages("dplyr")
  if (!require(Hmisc))    install.packages("Hmisc")
  if (!require(stargazer)) install.packages("stargazer")
  
  library(dplyr)
  library(Hmisc)
  library(stargazer)
  
  # Subset and clean data
  data <- df %>% 
    select(all_of(var_list)) %>%
    na.omit() %>%
    as.data.frame()
  
  # Compute correlation and p-values
  corr_result <- rcorr(as.matrix(data), type = "pearson")
  R <- corr_result$r
  p <- corr_result$P
  
  # Add significance stars
  stars <- ifelse(p < 0.01, "***",
                  ifelse(p < 0.05, "**",
                         ifelse(p < 0.10, "*", "")))
  
  # Combine correlation + stars, round to 1 decimal
  R_display <- matrix(paste0(format(round(R, 1), nsmall = 1), stars), 
                      nrow = nrow(R), ncol = ncol(R))
  diag(R_display) <- paste0(format(round(diag(R), 1), nsmall = 1), "   ")  # 1.0 on diagonal
  rownames(R_display) <- colnames(data)
  colnames(R_display) <- colnames(data)
  
  # Remove upper triangle (optional: keep full or lower only)
  R_display[upper.tri(R_display)] <- ""
  
  # Convert to data frame
  corr_df <- as.data.frame(R_display)
                           
                    # Print clean version in console
                           cat("\nCorrelation Matrix (1 decimal + significance):\n")
                           print(corr_df, row.names = FALSE)
                           
                           # Export with stargazer (BEAUTIFUL LaTeX table)
                           stargazer(corr_df,
                                     type = type,
                                     summary = FALSE,
                                     title = "Correlation Matrix with Significance Levels",
                                     notes = c("*** p<0.01, ** p<0.05, * p<0.10",
                                               "Lower triangle shown; diagonal = 1.0"),
                                     notes.align = "l",
                                     out = file_out,
                                     digits = 1,
                                     digit.separator = "",
                                     rownames = TRUE,
                                     colnames = TRUE)
                           
                           if (type == "latex") {
                             cat(paste0("\nLaTeX correlation table saved as: ", file_out, "\n"))
                           }
                           
                           # Return the clean data frame (optional use)
                           invisible(corr_df)
}

# Summary Statistics
summarystat <- function(df, var_list, type = "latex", file_out="Summary_Stat.tex") {
  
  # Load required packages
  if (!require(dplyr))    stop("Please install dplyr")
  if (!require(stargazer)) stop("Please install stargazer")
  
  library(dplyr)
  library(stargazer)
  
  # Validate variables exist
  missing_vars <- var_list[!var_list %in% names(df)]
  if (length(missing_vars) > 0) {
    stop("These variables are not in the data: ", paste(missing_vars, collapse = ", "))
  }
  
  # Subset data
  df_subset <- df %>% 
    select(all_of(var_list)) %>%
    ungroup() %>%
    as.data.frame()
  
  # Create summary_df with explicit calculations and 1 decimal
  summary_df <- data.frame(
    Variable = names(df_subset),
    N        = sapply(df_subset, function(x) sum(!is.na(x))),
    Mean     = sapply(df_subset, function(x) round(mean(x, na.rm = TRUE), 1)),
    SD       = sapply(df_subset, function(x) round(sd(x, na.rm = TRUE), 1)),
    Min      = sapply(df_subset, function(x) round(min(x, na.rm = TRUE), 1)),
    Median   = sapply(df_subset, function(x) round(median(x, na.rm = TRUE), 1)),
    Max      = sapply(df_subset, function(x) round(max(x, na.rm = TRUE), 1)),
    stringsAsFactors = FALSE
  )
  
  # Print the summary_df
  cat("\n=== Summary Statistics (1 decimal place) ===\n")
  print(summary_df, row.names = FALSE)
  
  # Run stargazer
  stargazer(df_subset,
            type = type,
            digits = 1,
            summary.stat = c("n", "mean", "sd", "min", "median", "max"),
            title = "Summary Statistics",
            notes = "All values rounded to one decimal place (except N).",
            header = FALSE,
            out = file_out)
  # Return the summary_df invisibly (so you can assign it)
  invisible(summary_df)
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

cluster_reg=function(model,cluster.var){
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(openxlsx,readxl,texreg,clubSandwich)
  clmodel=coef_test(model, vcov = "CR1", cluster=cluster.var)
  coefficient.names <- rownames(m4_cl)  # extract coef names
  coefficients <- clmodel[,2]  # extract coefficient values
  standard.errors <- clmodel[,3]  # extract standard errors
  significance <- clmodel[,6]  #extract p-values
  n<-  nobs(model) # extract log likelihood
  r2=summary(model)$r.squared
  gof <- c(n, r2)  # create a vector of GOF statistics
  gof.names <- c("Observations", "R2")  # names of GOFs
  decimal.places <- c(FALSE, TRUE)  # last one is a count variable
  
  tr <- texreg::createTexreg(coef.names = coefficient.names,
                             coef = coefficients,
                             se = standard.errors,
                             pvalues = significance,
                             gof.names = gof.names,
                             gof = gof,
                             gof.decimal = decimal.places)
  return(tr)
}




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
probval=integrate(approxfun(dens$x,dens$y,rule=2), lower=lower, upper=upper)
return(probval)
}




density_plot=function(u,v,xtitle="Inflation",name1="Before", name2="After",adj=2,ll=0.05,ul=0.95){
if (!require(pacman)) install.packages("pacman")
  pacman::p_load(ggplot2,data.table)
  g1=u
  g2=v
  value <- c(g1, g2)
  site  <- c(rep(name1, length(g1)), rep(name2, length(g2)))
  dt    <- data.table(site,value)
  #  generate kdf
  gg <- dt[,list(x=density(value,adjust = adj)$x, y=density(value,adjust = adj)$y),by="site"]
  #  calculate quantiles
  q11 <- quantile(dt[site==name1,value],ll)
  q12 <- quantile(dt[site==name1,value],ul)
  
  q21 <- quantile(dt[site==name2,value],ll)
  q22 <- quantile(dt[site==name2,value],ul)
  # generate the plot
  pt=ggplot(dt,aes(x=value,fill=site, color=site))+geom_density(aes(color=site, fill=site),adjust=adj, alpha=0)+scale_color_manual(values=c("indianred3","dodgerblue4" ))+scale_fill_manual(values=c("indianred3","dodgerblue4" ))+ 
    geom_ribbon(data=subset(gg,site==name1 & x<q11  ),
                aes(x=x,ymax=y,fill=name1),ymin=0, alpha=0.8, fill="dodgerblue4")+
    geom_ribbon(data=subset(gg,site==name1 & x>q12  ),
                aes(x=x,ymax=y,fill=name1),ymin=0, alpha=0.8, fill="dodgerblue4")+
    geom_ribbon(data=subset(gg,site==name2 & x<q21  ),
                aes(x=x,ymax=y,fill=name2),ymin=0, alpha=0.8, fill="indianred3")+
    geom_ribbon(data=subset(gg,site==name2 & x>q22),
                aes(x=x,ymax=y,fill=name2),ymin=0, alpha=0.8, fill="indianred3")+
    geom_ribbon(data=subset(gg,site==name2 & x>max(value)),
                aes(x=x,ymax=y,fill=name2),ymin=0, alpha=0.9)+theme_bw()
  pt=pt+xlab(xtitle) + ylab("Density")
  pt=pt+theme(legend.position ="bottom",legend.title = element_blank(),axis.text = element_text(size =12))
  pt
}

#Note: u=series 1, v=series2, name1= before name, name2=after name, ll=lower region, ul=upper region probability.

#x=rnorm(10000)
#y=rnorm(10000,0.5,1.3)

#density_plot(x,y, ll=0.01, ul=0.99)

#######################################################################
breakpoint=function(x,breaks=5){
if (!require(pacman)) install.packages("pacman")
pacman::p_load(strucchange)
bp.ri=strucchange::breakpoints(x ~ 1, h = 0.1, breaks=breaks)
return(summary(bp.ri))
}



breakpoint_plot=function(x,breaks){
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(strucchange)
  tt=1:length(x)
  fit <- ts(loess(x ~ tt, span = 0.2)$fitted, start = c(start(x)[1],start(x)[2]), frequency =frequency(x))
  bp.ri=strucchange::breakpoints(x ~ 1, h = 0.1)
  plot(x, type='l')
  lines(fit, col = 4)
  lines(confint(bp.ri, breaks=breaks), col ="red")
  grid()
}



## Spatial Model Reg
spatialmodelreg=function(model){
 if (!require(pacman)) install.packages("pacman")
pacman::p_load(openxlsx,readxl,plm,texreg)
ss=summary(model)
coefficient.names <- rownames(ss$CoefTable) # extract coef names
coefficients <- ss$CoefTable[,1]  # extract coefficient values
standard.errors <- ss$CoefTable[,2]  # extract standard errors
significance <- ss$CoefTable[,4]  #extract p-values
tr <- texreg::createTexreg(coef.names = coefficient.names,
                   coef = coefficients,
                   se = standard.errors,
                   pvalues = significance)
 return(tr)
 }




spatialmodelreg2=function(model){
 if (!require(pacman)) install.packages("pacman")
pacman::p_load(openxlsx,readxl,plm,texreg)
ss=summary(model)
coefficient.names <- rownames(ss$coef_par_table) # extract coef names
coefficients <- ss$coef_par_table[,1]  # extract coefficient values
standard.errors <- ss$coef_par_table[,2]  # extract standard errors
significance <- ss$coef_par_table[,4]  #extract p-values
tr <- texreg::createTexreg(coef.names = coefficient.names,
                   coef = coefficients,
                   se = standard.errors,
                   pvalues = significance)
 return(tr)
 }



## Aggregating Time Series

ts_transform=function(data,year,number,freq,nfreq,fun="mean"){
if (!require(pacman)) install.packages("pacman")
pacman::p_load(readxl)

if (fun=="mean"){
  end=function(x)(mean(na.omit(x)))
  monthly <- ts(data, start = c(year, number), frequency =freq)
  quarterly <- aggregate(monthly, nfrequency = nfreq, FUN =end)
  return(quarterly)
}

if (fun=="sum"){
  end=function(x)(sum(na.omit(x)))
  monthly <- ts(data, start = c(year, number), frequency =freq)
  quarterly <- aggregate(monthly, nfrequency = nfreq, FUN =end)
  return(quarterly)
}

if (fun=="sd"){
  std_dev=function(x)(sd(na.omit(x)))
  monthly <- ts(data, start = c(year, number), frequency =freq)
  quarterly <- aggregate(monthly, nfrequency = nfreq, FUN =std_dev)
  return(quarterly)
}

if (fun=="end"){
  end=function(x)(x[length(x)])
  monthly <- ts(data, start = c(year, number), frequency =freq)
  quarterly <- aggregate(monthly, nfrequency = nfreq, FUN =end)
  return(quarterly)
}

if (fun=="median"){
  end=function(x)(median(na.omit(x)))
  monthly <- ts(data, start = c(year, number), frequency =freq)
  quarterly <- aggregate(monthly, nfrequency = nfreq, FUN =end)
  return(quarterly)
}

}




##Make Panel

make_panel=function(dataname=data, panelname="Panel Name", variable="Variable Name"){
  data=dataname
  colnames(data)[which(colnames(data)==panelname)]=c("Panel")
  colnames(data)[which(colnames(data)==variable)]=c("Variable")
  molted=melt(data,id.vars=c("Panel","Variable"))
  pdata=dcast(molted,Panel+variable~Variable, fun.aggregate =mean)   #id is column
  colnames(pdata)[which(colnames(pdata)=="Panel")]=panelname
  colnames(pdata)[which(colnames(pdata)=="variable")]=c("Time")
  return(pdata)
}



## Frequency Distribution
count_f=function(z){
d1=transform(table(z))
d2=transform(round(prop.table(table(z))*100,digits=1))
dff=cbind(d1,d2[2])
colnames(dff)=c("Variable","Freq","Percentage")
return(dff)
 }


#### CODES FOR GENERATING RANDOM WEIGHTS
# Set the number of values
n <- 10

random_points <- sort(runif(n - 1, 0, 1)) # Generate n-1 random numbers between 0 and 1
random_points <- unique(c(0, random_points, 1))
weights <- diff(random_points) # Compute the differences to get the random numbers that sum to 1
weights

panel_unit_root <- function(df,               # your data frame (already cleaned)
                                var_list,         # vector of variable names (as strings)
                                index = c("Bank", "Year"),   # change if needed
                                lags = "AIC",     # or fixed number, e.g. 4
                                pmax = 2,         # max lags
                                type = "latex",    # "html" (for Word), "latex", or "text"
                                file_out = "CIPS_Unit_Root_Results.tex",
                                title = "Panel Unit Root Tests – Pesaran (2007) CIPS") {
  
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(plm,dplyr,stargazer)
  # 1. Convert to pdata.frame
  panel_data <- pdata.frame(df, index = index)
  
  # 2. Run CIPS test for each variable
  results <- lapply(var_list, function(v) {
    test <- purtest(reformulate("1", response = v),
                    data = panel_data,
                    test = "ips",
                    exo = "intercept",
                    lags = lags,
                    pmax = pmax,
                    test.type = "cips")
    
    stat <- round(test$statistic$statistic, 3)
    pval <- round(test$statistic$p.value, 4)
    
    # Stars (Pesaran 2007, no trend)
    stars <- ifelse(stat <= -2.66, "***",
                    ifelse(stat <= -2.44, "**",
                           ifelse(stat <= -2.33, "*", "")))
    
    order <- ifelse(stat <= -2.33, "I(0)", "I(1)")
    
    data.frame(
      Variable = v,
      p_value = pval,
      Significance = stars,
      Order = order,
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()
  
  # 3. Pretty variable names (customize as needed)
  results <- results %>%
    mutate(Variable = case_when(
      Variable == "CoD"               ~ "Cost of Deposits (CoD)",
      Variable == "CoF"               ~ "Cost of Funds (CoF)",
      Variable == "Return_on_Advances"~ "Return on Advances",
      Variable == "NIM"               ~ "Net Interest Margin (NIM)",
      Variable == "Cred_g"            ~ "Credit Growth",
      Variable == "CD_ratio"          ~ "Credit–Deposit Ratio",
      Variable == "CRAR"              ~ "Capital Adequacy Ratio (CRAR)",
      Variable == "NetNPA"            ~ "Net NPA Ratio",
      TRUE                            ~ Variable
    ))
  
  # 4. Export with stargazer
  stargazer(results,
            type = type,
            summary = FALSE,
            rownames = FALSE,
            digits = 3,
            title = title,
            notes = c("***, **, * indicate rejection at 1%, 5%, 10%.",
                      "Critical values (no trend): −2.66 (1%), −2.44 (5%), −2.33 (10%).",
                      paste("Lag order:", lags, "(max =", pmax, "lags).")),
            notes.align = "l",
            notes.append = FALSE,
            out = file_out)
  
  # 5. Also return the data frame + print to console
  cat("\n=== CIPS Unit Root Test Results ===\n")
  print(results, row.names = FALSE)
  
  invisible(results)  # returns silently
}

