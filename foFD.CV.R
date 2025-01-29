foFD.CV <- function(fdobj1, levels, ncan = 2, nf=100,lam1,seed)
{
  library(fda)
  library(MASS)
  library(FNN)
  library(caret)
  require(lolR)
  set.seed(seed)
  OL=numeric(length(lam1))
  n_iter=length(lam1)
  labels=apply(levels, 1,sum)
  pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  x11()
  for(i1 in 1:length(lam1)){
    n=dim(fdobj1$coefs)[2] #number of statistical units
    perm=sample(1:n)
    
    for(k in 1:trunc(n/nf)){
      kfold.out=perm[((nf*(k-1)+1):(nf*k))]
      foFD.fit=foFD.fd(fdobj1[-kfold.out],levels[-kfold.out,], ncan = 2,lam1[i1])
      scores.in=as.data.frame(foFD.fit$fdavar1)
      scores.out=as.data.frame(foFD.scores(fdobj1[kfold.out],foFD.fit))
      classifier <- lol.classify.nearestCentroid(scores.in, labels[-kfold.out])
      preds=predict(classifier,scores.out)
      OL[i1]=OL[i1]+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))
      #plot(scores.out,col=colors[preds+1],cex=1.5,pch=20)
    }
    setTxtProgressBar(pb, i1)
  }
  return(list(OL=OL,l1=lam1[(which(OL==min(OL)))]))
}



lam1=c(10^{0:10})

foFD.cv=foFD.CV(fdobj1=ROI_curves, levels=ROI_levs_mat, ncan = 2, nf=200,lam1,seed=030362)

x11()
plot(0:10,log(foFD.cv$OL),type="l")

#foFD.cv.smooth=smooth.spline(x=(0:10), y=log(foFD.cv$OL), lambda = 10^-3)
foFD.cv.smooth=ksmooth(0:10, log(foFD.cv$OL), kernel = c("normal"), bandwidth = 2)

plot(foFD.cv.smooth,type="l")
points(0:10,log(foFD.cv$OL))

################# splines ###########

library(mgcv)

x=0:10
y=log(foFD.cv$OL)
reg <- gam(y~s(x),select=T)
foFD.cv.smooth=predict(reg, newdata=data.frame(x = seq(0,10,0.1)))


plot(seq(0,10,0.1),foFD.cv.smooth,type="l")
#plot(foFD.cv.smooth,type="l")
points(0:10,log(foFD.cv$OL))

foFD.cv.smooth=data.frame(x=seq(0,10,0.1),y=foFD.cv.smooth)
# Change the color
library(ggplot2)

ggplot(data=as.data.frame(foFD.cv.smooth), aes(x=x, y=y)) +
  geom_line(color="purple",size=1.5)+
  geom_point(data=data.frame(x1=0:10,y1=log(foFD.cv$OL)),mapping=aes(x=x1,y=y1))+
  geom_point(data=data.frame(x2=8,y2=predict(reg,data.frame(x=8))),aes(x2,y2),shape=8, color="purple4", size=3)+
  scale_x_continuous(breaks = 0:10)+xlab("log_10(lambda)")+ylab("log_e(loss)")+ggtitle("ordFDA: k-fold CV for penalty parameter")+
  ylim(c(6.2,6.75))
  

