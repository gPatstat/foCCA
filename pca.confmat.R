pca.confmat=function(fdobj1, levels, ncan = 2, nf=100,centerfns=TRUE,l1)
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
    harmfdPar<- fdPar(fdobj1$basis, 2, lambda=l1)
    pca.fit=pca.fd(fdobj1[-kfold.out],nharm=2,harmfdPar=harmfdPar)
    scores.in=as.data.frame(pca.fit$scores)
    scores.out=as.data.frame(pca.scores(fdobj1[kfold.out],pca.fit))
    #z <- lda(labels[-kfold.out]~., scores.in)
    #preds=as.numeric(predict(z, scores.out)$class)-1
    classifier <- lol.classify.nearestCentroid(scores.in, labels[-kfold.out])
    preds=predict(classifier,scores.out)
    tot_preds=append(tot_preds,preds)
    tot_true=append(tot_true,labels[kfold.out])
  }
  tot_true[tot_true==0]=1
  tot_preds[tot_preds==0]=1
  return(confusionMatrix(as.factor(tot_preds),as.factor(tot_true)))
}
