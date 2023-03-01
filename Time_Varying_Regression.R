#### INSTALLING PACKAGES ######
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ARDL,DescTools,pglm,roll,mfx,tvReg,rollRegres,AER,readr,lsa,textdata,janeaustenr,tidyverse,glue,stringr,tm,SnowballC,tidytext,wordcloud,syuzhet,Metrics,gmm,tsDyn,RStata,ivreg,vars,bvarsv,dyn, systemfit,tseries, texreg,TSstudio,forecast, mFilter, lmtest,tidyverse, survival,tidyverse, skewt,survival, ggfortify, survminer, plotly, gridExtra, 
               Epi,  eha, shiny, ctqr, scales,tigerstats, data.table,astsa,sn, 
               eha, shiny, ctqr, scales,tigerstats, data.table,astsa, foreign, haven,
               tidyverse,ROI.plugin.quadprog,ROI.plugin.glpk,rriskDistributions,fGarch,ctqr,ITNr,RColorBrewer,scales,graphsim,
               igraph,readxl,plyr,quantreg,bayesQR, spatstat,ggplot2,ggpubr, gtools, car,
               devtools,statar,reshape2,MASS,dynpanel,plm,ggridges,ggthemes, PortfolioAnalytics, ROI)

github_source=function(url){
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(base)
  return(source(paste0(url)))
}
github_source("https://raw.githubusercontent.com/silumuduli/Time-Series-Models/main/Lag%20and%20Difference.R")
github_source("https://raw.githubusercontent.com/silumuduli/Graph-Codes/main/Graph_Codes.R")
github_source("https://raw.githubusercontent.com/silumuduli/Miscellaneous/main/sumstat.R")
github_source("https://raw.githubusercontent.com/silumuduli/Panel-Data-Models/main/Panel.R")





roll_plot=function(y,ind_list,formula,time,v,w,ci, year, number, freq,ylabname){
  zz=qnorm(p=(1-ci)/2, lower.tail=FALSE)
  roll_model=roll_lm(as.matrix(data.frame(ind_list)), y,width=w)
  coef.lm <- stats::lm(formula)$coef
  ttts=ts(time,start=c(year,number), frequency =freq)
  dff=data.frame(a=as.Date(ttts),b=roll_model$coefficients[,v+1],se=roll_model$std.error[,v+1])
  dff=na.omit(dff)
  
  plt=ggplot(dff, aes(a))+ geom_ribbon(aes(ymin = b - zz*se, ymax = b + zz*se), fill = "grey85") 
  plt=plt+geom_line(aes(y = b), color="turquoise4", size=0.5)+theme_bw()+  xlab("Time") + ylab(ylabname)
  #plot=plot + annotate("rect", xmin =as.Date("2014-09-01"),xmax =as.Date("2017-05-01"), ymin=-0.21,ymax=0.3, fill="blue",alpha=0.1)
  #plot=plot + annotate("text", x =as.Date("2016-01-01"), y = -0.15, label = paste("Shaded regions are",ci*100,"% confidence intervals"))
  plt=plt+geom_hline(yintercept=0, linetype="dashed", color = "red",size=1)
  plt
}

roldata <- read_excel("CO_Emission.xlsx")
roldata=na.omit(roldata)

# An Example
#y1=roldata$`One Year Ahead` - mean(roldata$`One Year Ahead`- roldata$`Actual Inflation`) # dependent variable
y=roldata$CO_Emission
x1= roldata$PC_GDP
x2=roldata$Energy_use
time=roldata$Year #time variable
ind_list=list(x1) # making the list of independent variables
formula=y ~ x1+x2 # formula for the linear model
year=1966 # starting year of time  series
number=1 # month number or quarter number
freq=1 # frequency of the time series, quarterly =4, monthly =12
v= 1  #Order of variable of interest to be plotted
w=20 # Window Size
ci=0.95 # confidence interval

ylabname="Coefficient of Inflation" # y-axis name

roll_plot(y, ind_list, formula, time, v, w, ci, year, number, freq, ylabname)


data=data.frame(y,x1,x2)
tvlm.fit <- tvLM(formula, data = data, bw =0.5)
coeft=tvlm.fit$coefficients[,2]
tv_ts=ts(coeft, start = c(year, number), frequency = freq)
tsplot(tv_ts)

tvlm.fit95=confint(tvlm.fit, level = 0.95, runs = 1000)
l=tvlm.fit95$Lower[,v+1]
u=tvlm.fit95$Upper[,v+1]
tvcoef=tvlm.fit95$coefficients[,v+1]
shaded_plot(x=time,y=coeft,l=l,u=u, xlab = "Year",ylab = "Sensitivity of CO2 Emission to GDP Growth")
