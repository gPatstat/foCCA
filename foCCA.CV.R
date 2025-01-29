foCCA.CV <- function(fdobj1, levels, ncan = 2, nf=100,lam1,lam2,seed,
                       centerfns=TRUE)
{
  library(fda)
  library(MASS)
  library(FNN)
  library(caret)
  library(lolR)
  set.seed(seed)
  OL=matrix(0,length(lam1),length(lam2))
  n_iter=length(lam1)*length(lam2)
  
  pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  labels=apply(levels, 1,sum)
  for(i1 in 1:length(lam1)){
    for(i2 in 1:length(lam2)){
      n=dim(fdobj1$coefs)[2] #number of statistical units
      perm=sample(1:n)
      
      for(k in 1:trunc(n/nf)){
        kfold.out=perm[((nf*(k-1)+1):(nf*k))]
        cca.fit=foCCA.fd(fdobj1[-kfold.out],levels[-kfold.out,],ncan=2,lambda1=lam1[i1],lambda2 = lam2[i2])
        scores.in=as.data.frame(cca.fit$ccavar1)
        scores.out=as.data.frame(foCCA.scores(fdobj1[kfold.out],cca.fit))
        classifier <- lol.classify.nearestCentroid(scores.in, labels[-kfold.out])
        preds=predict(classifier,scores.out)
        OL[i1,i2]=OL[i1,i2]+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
      }
      setTxtProgressBar(pb, i2+(length(lam2)*(i1-1)))
    }
  }
  return(list(OL=OL,l1=lam1[(which(OL==min(OL),arr.ind=T)[1])],l2=lam2[(which(OL==min(OL),arr.ind=T)[2])]))
}

lam1=c(10^{0:10})
lam2=c(10^{-10:10})

cca.cv=foCCA.CV(fdobj1=ROI_curves, levels=ROI_levs_mat, ncan = 2, nf=200,lam1,lam2,seed=110203)
#cca.cv_case_study=cca.cv

image(x=(0:10),y=(-10:10),log(cca.cv$OL),xlab="log(lambda2)",ylab="log(lambda1)")

graphics.off()


############# smoothing splines ############
library(mgcv)
x1=0:10
x2=-10:10
y=matrix(log(cca.cv$OL),11*21,byrow=F)
space_grid=data.frame(x1=expand.grid(x1,x2)[,1],x2=expand.grid(x1,x2)[,2],y)


reg <- gam(y~te(x1,x2),select=T,data=space_grid)

plot(reg)
cca.cv.smooth=predict(reg, newdata=data.frame(x1 = expand.grid(0:10,-10:10)[,1],x2=expand.grid(0:10,-10:10)[,2]))

cca.cv.smooth=im(matrix(cca.cv.smooth,11,21),xcol=-10:10,yrow=0:10)

par(mfrow=c(1,2))
plot.im(cv_case_study_im,ribargs=list(axes=T),main="Raw loss",cex.lab=2,col=colcv)
plot.im(cca.cv.smooth,ribargs=list(axes=T),main="Smoothed loss",cex.lab=2,col=colcv)
plot(owin(xrange=c(2.5,3.5),yrange=c(1.5,+2.5)),add=T,hatch=T,hatchargs=list(spacing=0.2))

lam=which(cca.cv.smooth$v==min(cca.cv.smooth$v),arr.ind=T)

cbind(l1=lam1[lam[1]],l2=lam2[lam[2]])

graphics.off()
