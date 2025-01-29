MAE_foCCA=function(sim, ncan = 2, nf=100,centerfns=TRUE)
{
  library(fda)
  library(MASS)
  library(FNN)
  library(caret)
  require(lolR)
  OL=0
  sim=sim.fd
  fdobj1=sim$fun
  labels=sim$levels
  
  levels=data.frame(ifelse(labels>0,1,0),ifelse(labels>1,1,0),ifelse(labels>2,1,0),ifelse(labels>3,1,0),
                                    ifelse(labels>4,1,0),ifelse(labels>5,1,0),ifelse(labels>6,1,0),
                                    ifelse(labels>7,1,0))
    
  n=dim(fdobj1$coefs)[2] #number of statistical units
  perm=sample(1:n)
  
  for(k in 1:trunc(n/nf)){
    kfold.out=perm[((nf*(k-1)+1):(nf*k))]
    cca.fit=foCCA.fd(fdobj1[-kfold.out],levels[-kfold.out,],ncan=2,lambda1=0,lambda2 = 0)
    scores.in=as.data.frame(cca.fit$ccavar1)
    scores.out=as.data.frame(foCCA.scores(fdobj1[kfold.out],cca.fit))
    classifier <- lol.classify.nearestCentroid(scores.in, labels[-kfold.out])
    preds=predict(classifier,scores.out)
    OL=OL+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
  }


  return(OL)
}


MAE_fPCA=function(sim, ncan = 2, nf=100,centerfns=TRUE)
{
  library(fda)
  library(MASS)
  library(FNN)
  library(caret)
  require(lolR)
  OL=0
  
  fdobj1=sim$fun
  labels=sim$levels
  
  levels=data.frame(ifelse(labels>0,1,0),ifelse(labels>1,1,0),ifelse(labels>2,1,0),ifelse(labels>3,1,0),
                    ifelse(labels>4,1,0),ifelse(labels>5,1,0),ifelse(labels>6,1,0),
                    ifelse(labels>7,1,0))
  
  n=dim(fdobj1$coefs)[2] #number of statistical units
  perm=sample(1:n)
  
  for(k in 1:trunc(n/nf)){
    kfold.out=perm[((nf*(k-1)+1):(nf*k))]
    pca.fit=pca.fd(fdobj1[-kfold.out],nharm=2,harmfdPar=fdPar(fdobj1$basis, 2, lambda=0))
    scores.in=as.data.frame(pca.fit$scores)
    scores.out=as.data.frame(pca.scores(fdobj1[kfold.out],pca.fit))
    classifier <- lol.classify.nearestCentroid(scores.in, labels[-kfold.out])
    preds=predict(classifier,scores.out)
    OL=OL+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
  }
  
  
  return(OL)
}

MAE_foFD=function(sim, ncan = 2, nf=100,centerfns=TRUE)
{
  library(fda)
  library(MASS)
  library(FNN)
  library(caret)
  require(lolR)
  OL=0
  
  fdobj1=sim$fun
  labels=sim$levels
  
  levels=data.frame(ifelse(labels>0,1,0),ifelse(labels>1,1,0),ifelse(labels>2,1,0),ifelse(labels>3,1,0),
                    ifelse(labels>4,1,0),ifelse(labels>5,1,0),ifelse(labels>6,1,0),
                    ifelse(labels>7,1,0))
  
  n=dim(fdobj1$coefs)[2] #number of statistical units
  perm=sample(1:n)
  
  for(k in 1:trunc(n/nf)){
    kfold.out=perm[((nf*(k-1)+1):(nf*k))]
    foFD.fit=foFD.fd(fdobj1[-kfold.out],levels=levels[-kfold.out,],ncan=2,lambda1=0)
    scores.in=as.data.frame(foFD.fit$fdavar1)
    scores.out=as.data.frame(foFD.scores(fdobj1[kfold.out],foFD.fit))
    classifier <- lol.classify.nearestCentroid(scores.in, labels[-kfold.out])
    preds=predict(classifier,scores.out)
    OL=OL+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
  }
  
  
  return(OL)
}


############# MAE full ############


MAE=function(sim, ncan = 2, nf=100,centerfns=TRUE)
{
  library(fda)
  library(MASS)
  library(FNN)
  library(caret)
  require(lolR)
  
  OL=numeric(3)
  sim=sim.fd
  fdobj1=sim$fun
  labels=sim$levels
  
  levels=data.frame(ifelse(labels>0,1,0),ifelse(labels>1,1,0),ifelse(labels>2,1,0),ifelse(labels>3,1,0),
                    ifelse(labels>4,1,0),ifelse(labels>5,1,0),ifelse(labels>6,1,0),
                    ifelse(labels>7,1,0))
  
  n=dim(fdobj1$coefs)[2] #number of statistical units
  perm=sample(1:n)
  
  for(k in 1:trunc(n/nf)){
    kfold.out=perm[((nf*(k-1)+1):(nf*k))]
    ### foCCA ###
    cca.fit=foCCA.fd(fdobj1[-kfold.out],levels[-kfold.out,],ncan=2,lambda1=0,lambda2 = 0)
    scores.in=as.data.frame(cca.fit$ccavar1)
    scores.out=as.data.frame(foCCA.scores(fdobj1[kfold.out],cca.fit))
    centroids=aggregate(scores.in,list(as.factor(labels[-kfold.out])),mean)
    preds=(knnx.index(centroids[,-1],scores.out,k=1))-1
    OL[1]=OL[1]+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
    ### fPCA ###
    pca.fit=pca.fd(fdobj1[-kfold.out],nharm=2,harmfdPar=fdPar(fdobj1$basis, 2, lambda=0))
    scores.in=as.data.frame(pca.fit$scores)
    scores.out=as.data.frame(pca.scores(fdobj1[kfold.out],pca.fit))
    centroids=aggregate(scores.in,list(as.factor(labels[-kfold.out])),mean)
    preds=(knnx.index(centroids[,-1],scores.out,k=1))-1
    OL[2]=OL[2]+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
    ### foFD ###
    foFD.fit=foFD.fd(fdobj1[-kfold.out],levels=levels[-kfold.out,],ncan=2,lambda1=0)
    scores.in=as.data.frame(foFD.fit$fdavar1)
    scores.out=as.data.frame(foFD.scores(fdobj1[kfold.out],foFD.fit))
    length(scores.in)
    length(labels[-kfold.out])
    centroids=aggregate(scores.in,list(as.factor(labels[-kfold.out])),mean)
    preds=(knnx.index(centroids[,-1],scores.out,k=1))-1
    OL[3]=OL[3]+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
    }
  return(OL)
}

MAE(sim.fd)

