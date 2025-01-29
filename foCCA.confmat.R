foCCA.confmat=function(fdobj1, levels, ncan = 2, nf=100,centerfns=TRUE,l1,l2)
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
      cca.fit=foCCA.fd(fdobj1[-kfold.out],levels[-kfold.out,],ncan=2,lambda1=l1,lambda2 = l2)
      scores.in=as.data.frame(cca.fit$ccavar1)
      scores.out=as.data.frame(foCCA.scores(fdobj1[kfold.out],cca.fit))
      classifier <- lol.classify.nearestCentroid(scores.in, labels[-kfold.out])
      preds=predict(classifier,scores.out)
      #preds=voronoi_class(data=scores.in,levels=labels[-kfold.out],newdata=scores.out)
      tot_preds=append(tot_preds,preds)
      tot_true=append(tot_true,labels[kfold.out])
    }
  tot_true[tot_true==0]=1
  tot_preds[tot_preds==0]=1
  return(confusionMatrix(as.factor(tot_preds),as.factor(tot_true)))
}



