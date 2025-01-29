source("MAE_calculator.R")


B=500
########## Monte Carlo simulation: MAE varying the order-based increment

set.seed(110203)
q=seq(0.1,1,by=0.1)
mod=3
MAE_cv_foCCA=matrix(nrow=B,ncol=length(q))
MAE_cv_fPCA=matrix(nrow=B,ncol=length(q))
MAE_cv_foFD=matrix(nrow=B,ncol=length(q))

for(i in 1:length(q)){
  mae=numeric(3)
  for(b in 1:B){
    sim.fd=sim_against_FPCA(q[i])
    mae=MAE(sim.fd)
    MAE_cv_foCCA[b,i]=mae[1]
    MAE_cv_fPCA[b,i]=mae[2]
    MAE_cv_foFD[b,i]=mae[3]
  }
  print(i)
}



x11()
par(mfrow=c(1,3))
box_foCCA1=boxplot(MAE_cv_foCCA,ylim=c(0,250))$stats[2:4,]
box_fPCA1=boxplot(MAE_cv_fPCA,ylim=c(0,250))$stats[2:4,]
box_foFD1=boxplot(MAE_cv_foFD,ylim=c(0,250))$stats[2:4,]






########## Monte Carlo simulation: MAE varying the ratio

B=500
########## Monte Carlo simulation: MAE varying the order-based increment

set.seed(110203)
q=seq(0.1,1,by=0.1)
mod=3
MAE_cv_foCCA=matrix(nrow=B,ncol=length(q))
MAE_cv_fPCA=matrix(nrow=B,ncol=length(q))
MAE_cv_foFD=matrix(nrow=B,ncol=length(q))

for(i in 1:length(q)){
  mae=numeric(3)
  for(b in 1:B){
    sim.fd=sim_against_Fisher(q[i])
    mae=MAE(sim.fd)
    MAE_cv_foCCA[b,i]=mae[1]
    MAE_cv_fPCA[b,i]=mae[2]
    MAE_cv_foFD[b,i]=mae[3]
  }
  print(i)
}



x11()
par(mfrow=c(1,3))
box_foCCA2=boxplot(MAE_cv_foCCA,ylim=c(0,250))$stats[2:4,]
box_fPCA2=boxplot(MAE_cv_fPCA,ylim=c(0,250))$stats[2:4,]
box_foFD2=boxplot(MAE_cv_foFD,ylim=c(0,250))$stats[2:4,]



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
  ggtitle(expression(bold(paste('k-fold CV MAE varying ', q[a], 'in scenario (a)'))))+
  xlab(expression(q[a]))+ylab("MAE")+
  scale_color_manual(name="Method",values=c("foCCA"="salmon2","fPCA"="magenta","foFD"="magenta4"))+
  scale_fill_manual(name="Method",values=c("foCCA"="salmon2","fPCA"="magenta","foFD"="magenta4"))+
  scale_x_continuous(breaks=q)+  theme(text=element_text(size=20), #change font size of all text
                                       axis.text=element_text(size=20), #change font size of axis text
                                       axis.title=element_text(size=20), #change font size of axis titles
                                       plot.title=element_text(size=20), #change font size of plot title
                                       legend.text=element_text(size=20), #change font size of legend text
                                       legend.title=element_text(size=20))

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
  ggtitle(expression(bold(paste('k-fold CV MAE varying ', q[b], 'in scenario (b)'))))+
  xlab(expression(q[b]))+ylab("MAE")+
  scale_color_manual(name="Method",values=c("foCCA"="salmon2","fPCA"="magenta","foFD"="magenta4"))+
  scale_fill_manual(name="Method",values=c("foCCA"="salmon2","fPCA"="magenta","foFD"="magenta4"))+
  scale_x_continuous(breaks=q)+  theme(text=element_text(size=20), #change font size of all text
                                       axis.text=element_text(size=20), #change font size of axis text
                                       axis.title=element_text(size=20), #change font size of axis titles
                                       plot.title=element_text(size=20), #change font size of plot title
                                       legend.text=element_text(size=20), #change font size of legend text
                                       legend.title=element_text(size=20))

x11()
library(gridExtra)
grid.arrange(g_1,g_2,ncol=2)




