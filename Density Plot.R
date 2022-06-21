density_plot=function(u,v,xtitle="Inflation",name1="Before", name2="After",adj=2,ll=0.05,ul=0.95){
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
  pt=ggplot(dt,aes(x=value,fill=site, color=site))+geom_density(adjust=adj, alpha=0)+scale_color_manual(values=c("indianred3","steelblue4" ))+ 
    geom_ribbon(data=subset(gg,site==name1 & x<q11  ),
                aes(x=x,ymax=y,fill=name1),ymin=0, alpha=0.8, fill="steelblue4")+
    geom_ribbon(data=subset(gg,site==name1 & x>q12  ),
                aes(x=x,ymax=y,fill=name1),ymin=0, alpha=0.8, fill="steelblue4")+
    geom_ribbon(data=subset(gg,site==name2 & x<q21  ),
                aes(x=x,ymax=y,fill=name2),ymin=0, alpha=0.8, fill="indianred3")+
    geom_ribbon(data=subset(gg,site==name2 & x>q22),
                aes(x=x,ymax=y,fill=name2),ymin=0, alpha=0.8, fill="indianred3")+
    geom_ribbon(data=subset(gg,site==name2 & x>max(value)),
                aes(x=x,ymax=y,fill=name2),ymin=0, alpha=0.6)+theme_bw()
  #+
   # scale_color_manual(values = c(name2="brown",name1="deepskyblue3"))+scale_fill_manual(values = c(name2="brown",name1="deepskyblue3"))
  #+annotate("text", x = -300, y = 0.004, label = paste ("Scenario 1 KF@R(0.01)=", round(q1,2)))+
  #  annotate("text", x = -300, y = 0.0036, label = paste ("Scenario 2 KF@R(0.01)=", round(q2,2)))+
  # annotate("text", x = -300, y = 0.0032, label = paste ("Scenario 3 KF@R(0.01)=", round(q3,2)))
  
  
  pt=pt+xlab(xtitle) + ylab("Density")
  pt=pt+theme(legend.position ="bottom",legend.title = element_blank(),axis.text = element_text(size =12))
  pt
}

#Note: u=series 1, v=series2, name1= before name, name2=after name, ll=lower region, ul=upper region probability.

#x=rnorm(1000000)
#y=rnorm(1000000,0.5,1.3)

#density_plot(x,y, ll=0.01, ul=0.99)







