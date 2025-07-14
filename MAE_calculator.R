#install.packages("https://cran.r-project.org/src/contrib/Archive/Matrix/Matrix_1.5-4.1.tar.gz", repos = NULL, type = "source")

############# MAE full ############
#remotes::install_github("ilariabonavita/ncca_r")
#install.packages("irlba", type="source")

MAE=function(sim, ncan = 2, nf=100,centerfns=TRUE)
{
  library(fda)
  library(MASS)
  library(Matrix)
  library(MASSExtra)
  library(FNN)
  #library(caret)
  require(lolR)
  library(FREG)
  library(pracma)
  library(irlba)
  
  OL=numeric(5)
  valid_foLR<<-0
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
    centroids=aggregate(scores.in,list(as.factor(labels[-kfold.out])),mean)
    preds=(knnx.index(centroids[,-1],scores.out,k=1))-1
    OL[3]=OL[3]+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
    ### foLR ###
  #   train_ord<<-as.ordered(as.numeric(labels[-kfold.out]))
  #   test_ord<-as.numeric(labels[kfold.out])
  #   train_fd<<-fdobj1[-kfold.out]
  #   test_fd<-fdobj1[kfold.out]
  #   
  # # Manually create formula and set environment so it sees train_fd
  #   fit <- FREG::olfreg(train_ord ~ train_fd)
  #   
  #   pr <- predict(fit, newdata = list(train_fd = test_fd), type = "labels")
  #   pr <- as.integer(pr) - 1
  #   
  #   
  #   
  #   #print(pr)
  #   #print(test_ord)
  #   sum_<-sum(abs(pr-test_ord))/(trunc(n/nf))
  #   OL[4]<- OL[4]+sum_
    
    OL[4] <- tryCatch({
      train_ord <<- as.ordered(as.numeric(labels[-kfold.out]))
      test_ord  <- as.numeric(labels[kfold.out])
      train_fd  <<- fdobj1[-kfold.out]
      test_fd   <- fdobj1[kfold.out]
      
      # Fit del modello
      fit <- FREG::olfreg(train_ord ~ train_fd)
      
      # Predizione
      pr <- predict(fit, newdata = list(train_fd = test_fd), type = "labels")
      pr <- as.integer(pr) - 1
      valid_foLR<<-valid_foLR+1
      

      OL[4]+sum(abs(pr - test_ord))
      
      
    }, error = function(e) {
      message(sprintf("Errore al fold %d: %s", k, e$message))
      OL[4]  
    })
    
    ### NCCA ###
    X0 <- t(eval.fd(0:100,fdobj1, Lfd=0))
    Y0 <- as.matrix(labels)
    
    train_ord=labels[-kfold.out]
    train_fun=X0[-kfold.out,]
    test_ord=labels[kfold.out]
    test_fun=X0[kfold.out,]
    
    ncca_res <- ncca(X=train_fun,Y=as.matrix(train_ord),
                     XV=test_fun,YV=as.matrix(test_ord),
                     d = 2, hx = 0.75, hy = 0.75, nx = NumNNs_X, ny=NumNNs_Y)
    
    
    classifier <- lol.classify.nearestCentroid(ncca_res$X_new, train_ord)
    preds=predict(classifier,ncca_res$XV_new)
    OL[5]=OL[5]+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
    
  }
  OL[4]=OL[4]/valid_foLR
  return(list(OL=OL,valid_foLR=valid_foLR))
}

sim.fd=scenarioB(0.1)
MAE(sim.fd)





########## Old functions ##########

# MAE_foCCA=function(sim, ncan = 2, nf=100,centerfns=TRUE)
# {
#   library(fda)
#   library(MASS)
#   library(FNN)
#   library(caret)
#   require(lolR)
#   OL=0
#   sim=sim.fd
#   fdobj1=sim$fun
#   labels=sim$levels
#   
#   levels=data.frame(ifelse(labels>0,1,0),ifelse(labels>1,1,0),ifelse(labels>2,1,0),ifelse(labels>3,1,0),
#                                     ifelse(labels>4,1,0),ifelse(labels>5,1,0),ifelse(labels>6,1,0),
#                                     ifelse(labels>7,1,0))
#     
#   n=dim(fdobj1$coefs)[2] #number of statistical units
#   perm=sample(1:n)
#   
#   for(k in 1:trunc(n/nf)){
#     kfold.out=perm[((nf*(k-1)+1):(nf*k))]
#     cca.fit=foCCA.fd(fdobj1[-kfold.out],levels[-kfold.out,],ncan=2,lambda1=0,lambda2 = 0)
#     scores.in=as.data.frame(cca.fit$ccavar1)
#     scores.out=as.data.frame(foCCA.scores(fdobj1[kfold.out],cca.fit))
#     classifier <- lol.classify.nearestCentroid(scores.in, labels[-kfold.out])
#     preds=predict(classifier,scores.out)
#     OL=OL+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
#   }
# 
# 
#   return(OL)
# }
# 
# 
# MAE_fPCA=function(sim, ncan = 2, nf=100,centerfns=TRUE)
# {
#   library(fda)
#   library(MASS)
#   library(FNN)
#   library(caret)
#   require(lolR)
#   OL=0
#   
#   fdobj1=sim$fun
#   labels=sim$levels
#   
#   levels=data.frame(ifelse(labels>0,1,0),ifelse(labels>1,1,0),ifelse(labels>2,1,0),ifelse(labels>3,1,0),
#                     ifelse(labels>4,1,0),ifelse(labels>5,1,0),ifelse(labels>6,1,0),
#                     ifelse(labels>7,1,0))
#   
#   n=dim(fdobj1$coefs)[2] #number of statistical units
#   perm=sample(1:n)
#   
#   for(k in 1:trunc(n/nf)){
#     kfold.out=perm[((nf*(k-1)+1):(nf*k))]
#     pca.fit=pca.fd(fdobj1[-kfold.out],nharm=2,harmfdPar=fdPar(fdobj1$basis, 2, lambda=0))
#     scores.in=as.data.frame(pca.fit$scores)
#     scores.out=as.data.frame(pca.scores(fdobj1[kfold.out],pca.fit))
#     classifier <- lol.classify.nearestCentroid(scores.in, labels[-kfold.out])
#     preds=predict(classifier,scores.out)
#     OL=OL+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
#   }
#   
#   
#   return(OL)
# }
# 
# MAE_foFD=function(sim, ncan = 2, nf=100,centerfns=TRUE)
# {
#   library(fda)
#   library(MASS)
#   library(FNN)
#   library(caret)
#   require(lolR)
#   OL=0
#   
#   fdobj1=sim$fun
#   labels=sim$levels
#   
#   levels=data.frame(ifelse(labels>0,1,0),ifelse(labels>1,1,0),ifelse(labels>2,1,0),ifelse(labels>3,1,0),
#                     ifelse(labels>4,1,0),ifelse(labels>5,1,0),ifelse(labels>6,1,0),
#                     ifelse(labels>7,1,0))
#   
#   n=dim(fdobj1$coefs)[2] #number of statistical units
#   perm=sample(1:n)
#   
#   for(k in 1:trunc(n/nf)){
#     kfold.out=perm[((nf*(k-1)+1):(nf*k))]
#     foFD.fit=foFD.fd(fdobj1[-kfold.out],levels=levels[-kfold.out,],ncan=2,lambda1=0)
#     scores.in=as.data.frame(foFD.fit$fdavar1)
#     scores.out=as.data.frame(foFD.scores(fdobj1[kfold.out],foFD.fit))
#     classifier <- lol.classify.nearestCentroid(scores.in, labels[-kfold.out])
#     preds=predict(classifier,scores.out)
#     OL=OL+sum(abs(as.numeric(preds)-as.numeric(labels[kfold.out])))/(trunc(n/nf))
#   }
#   
#   
#   return(OL)
# }
# 











