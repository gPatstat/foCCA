      ############## SIMULATION STUDY #############
      
      
source("foCCA.fd.R")
source("foCCA.scores.R")
source("foCCA.CV.R")
source("foCCA.confmat.R")
source("pca.scores.R")
source("pca.CV.R")
source("pca.confmat.R")
source("foFD.fd.R")
source("foFD.scores.R")
source("foFD.CV.R")
source("foFD.confmat.R")
      
      
    ############### Simulations ############
      
      
      
source("MAE_calculator.R")
source("scenarioA.R")
source("scenarioB.R")
      
      
q5=function(x){
  return(quantile(x,0.05))
}
q95=function(x){
  return(quantile(x,0.95))
}
      
B=500
########## Monte Carlo simulation: MAE varying the order-based increment

set.seed(110203)
q=seq(0.1,1,by=0.1)
mod=3
MAE_cv_foCCA1=matrix(nrow=B,ncol=length(q))
MAE_cv_fPCA1=matrix(nrow=B,ncol=length(q))
MAE_cv_foFD1=matrix(nrow=B,ncol=length(q))

for(i in 1:length(q)){
  mae=numeric(3)
  for(b in 1:B){
    sim.fd=scenarioA(q[i])
    mae=MAE(sim.fd)
    MAE_cv_foCCA1[b,i]=mae[1]
    MAE_cv_fPCA1[b,i]=mae[2]
    MAE_cv_foFD1[b,i]=mae[3]
    #print(paste("it:",b))
  }
  print(paste("q:",q[i]))
}

box_foCCA1=rbind(apply(MAE_cv_foCCA1,2,q5),apply(MAE_cv_foCCA1,2,mean),apply(MAE_cv_foCCA1,2,q95))
box_fPCA1=rbind(apply(MAE_cv_fPCA1,2,q5),apply(MAE_cv_fPCA1,2,mean),apply(MAE_cv_fPCA1,2,q95))
box_foFD1=rbind(apply(MAE_cv_foFD1,2,q5),apply(MAE_cv_foFD1,2,mean),apply(MAE_cv_foFD1,2,q95))


########## Monte Carlo simulation: MAE varying the ratio

B=500
########## Monte Carlo simulation: MAE varying the order-based increment

set.seed(110203)
q=seq(0.1,1,by=0.1)
mod=3
MAE_cv_foCCA2=matrix(nrow=B,ncol=length(q))
MAE_cv_fPCA2=matrix(nrow=B,ncol=length(q))
MAE_cv_foFD2=matrix(nrow=B,ncol=length(q))

for(i in 1:length(q)){
  mae=numeric(3)
  for(b in 1:B){
    sim.fd=scenarioB(q[i])
    mae=MAE(sim.fd)
    MAE_cv_foCCA2[b,i]=mae[1]
    MAE_cv_fPCA2[b,i]=mae[2]
    MAE_cv_foFD2[b,i]=mae[3]
  }
  print(paste("q:",q[i]))
}


box_foCCA2=rbind(apply(MAE_cv_foCCA2,2,q5),apply(MAE_cv_foCCA2,2,mean),apply(MAE_cv_foCCA2,2,q95))
box_fPCA2=rbind(apply(MAE_cv_fPCA2,2,q5),apply(MAE_cv_fPCA2,2,mean),apply(MAE_cv_fPCA2,2,q95))
box_foFD2=rbind(apply(MAE_cv_foFD2,2,q5),apply(MAE_cv_foFD2,2,mean),apply(MAE_cv_foFD2,2,q95))



### Plot performances ###

library(ggplot2)

d1=data.frame(x1=q,
              y1=box_foCCA1[2,],
              y2=box_fPCA1[2,],
              y3=box_foFD1[2,])


g_1=ggplot(data=d1,mapping=aes(x=x1))+
  geom_line(linewidth=1.5,aes(y=y1, color="foCCA"))+
  geom_ribbon(aes(x=q,ymin = box_foCCA1[1,], ymax = box_foCCA1[3,], fill="foCCA"),alpha=0.2)+
  geom_line(linewidth=1.5,aes(y=y2, color="fPCA"))+
  geom_ribbon(aes(x=q,ymin = box_fPCA1[1,], ymax = box_fPCA1[3,], fill="fPCA"),alpha=0.2)+
  geom_line(linewidth=1.5,aes(y=y3, color="foFD"))+
  geom_ribbon(aes(x=q,ymin = box_foFD1[1,], ymax = box_foFD1[3,], fill="foFD"),alpha=0.2)+
  ggtitle(expression(bold(paste('k-fold CV MAE varying ', q[a], ' in scenario (a)'))))+
  xlab(expression(q[a]))+ylab("MAE")+
  scale_color_manual(name="Method",values=c("foCCA"="salmon2","fPCA"="magenta","foFD"="magenta4"))+
  scale_fill_manual(name="Method",values=c("foCCA"="salmon2","fPCA"="magenta","foFD"="magenta4"))+
  scale_x_continuous(breaks=q)+  theme(text=element_text(size=15), #change font size of all text
                                       axis.text=element_text(size=15), #change font size of axis text
                                       axis.title=element_text(size=15), #change font size of axis titles
                                       plot.title=element_text(size=15), #change font size of plot title
                                       legend.text=element_text(size=15), #change font size of legend text
                                       legend.title=element_text(size=15))

library(ggplot2)

d2=data.frame(x1=q,
              y1=box_foCCA2[2,],
              y2=box_fPCA2[2,],
              y3=box_foFD2[2,])




g_2=ggplot(data=d2,mapping=aes(x=x1))+
  geom_line(linewidth=1.5,aes(y=y1, color="foCCA"))+
  geom_ribbon(aes(x=q,ymin = box_foCCA2[1,], ymax = box_foCCA2[3,], fill="foCCA"),alpha=0.2)+
  geom_line(linewidth=1.5,aes(y=y2, color="fPCA"))+
  geom_ribbon(aes(x=q,ymin = box_fPCA2[1,], ymax = box_fPCA2[3,], fill="fPCA"),alpha=0.2)+
  geom_line(linewidth=1.5,aes(y=y3, color="foFD"))+
  geom_ribbon(aes(x=q,ymin = box_foFD2[1,], ymax = box_foFD2[3,], fill="foFD"),alpha=0.2)+
  ggtitle(expression(bold(paste('k-fold CV MAE varying ', q[b], ' in scenario (b)'))))+
  xlab(expression(q[b]))+ylab("MAE")+
  scale_color_manual(name="Method",values=c("foCCA"="salmon2","fPCA"="magenta","foFD"="magenta4"))+
  scale_fill_manual(name="Method",values=c("foCCA"="salmon2","fPCA"="magenta","foFD"="magenta4"))+
  scale_x_continuous(breaks=q)+  theme(text=element_text(size=15), #change font size of all text
                                       axis.text=element_text(size=15), #change font size of axis text
                                       axis.title=element_text(size=15), #change font size of axis titles
                                       plot.title=element_text(size=15), #change font size of plot title
                                       legend.text=element_text(size=15), #change font size of legend text
                                       legend.title=element_text(size=15))


library(gridExtra)
grid.arrange(g_1,g_2,ncol=2)
      