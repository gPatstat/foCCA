foFD.confmat=function(fdobj1, levels, ncan = 2, nf=100,centerfns=TRUE,l1)
{
  library(fda)
  library(MASS)
  library(FNN)
  library(caret)
  require(lolR)
  
  labels=apply(levels, 1,sum)
  n=dim(fdobj1$coefs)[2] #number of statistical units
  perm=sample(1:n)
  tot_preds=numeric()
  tot_true=numeric()
  
  for(k in 1:trunc(n/nf)){
    kfold.out=perm[((nf*(k-1)+1):(nf*k))]
    foFD.fit=foFD.fd(fdobj1[-kfold.out],levels[-kfold.out,], ncan = 2,l1)
    scores.in=as.data.frame(foFD.fit$fdavar1)
    scores.out=as.data.frame(foFD.scores(fdobj1[kfold.out],foFD.fit))
    classifier <- lol.classify.nearestCentroid(scores.in, labels[-kfold.out])
    preds=predict(classifier,scores.out)
    tot_preds=append(tot_preds,preds)
    tot_true=append(tot_true,labels[kfold.out])
  }
  tot_true[tot_true==0]=1
  tot_preds[tot_preds==0]=1
  return(confusionMatrix(as.factor(tot_preds),as.factor(tot_true)))
}



foFD.CV.confmat(fdobj1=fd, levels=levs_mat, ncan = 2, nf=200,l1=10^7)  


x11()

