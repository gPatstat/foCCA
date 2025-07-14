mean_time=function(ncan = 2, B=100,centerfns=TRUE)
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
  

  valid_foLR<<-0
  sum_tC=0
  sum_tL=0
  
  n=dim(fdobj1$coefs)[2] #number of statistical units
  perm=sample(1:n)
  
  for(b in 1:B){
    sim=scenarioA(1)
    fdobj1=sim$fun
    labels=sim$levels
    
    levels=data.frame(ifelse(labels>0,1,0),ifelse(labels>1,1,0),ifelse(labels>2,1,0),ifelse(labels>3,1,0),
                      ifelse(labels>4,1,0),ifelse(labels>5,1,0),ifelse(labels>6,1,0),
                      ifelse(labels>7,1,0))
    
    start=Sys.time()
    ### foCCA ###
    cca.fit=foCCA.fd(fdobj1,levels,ncan=2,lambda1=0,lambda2 = 0)
    scores.in=as.data.frame(cca.fit$ccavar1)
    centroids=aggregate(scores.in,list(as.factor(labels)),mean)
    preds=(knnx.index(centroids[,-1],scores.in,k=1))-1
    #foLR
    sum_tC=sum_tC+Sys.time()-start
    tryCatch({
      train_ord <<- as.ordered(as.numeric(labels))
      train_fd  <<- fdobj1
       # Fit del modello
     start=Sys.time()
     fit<-FREG::olfreg(train_ord ~ train_fd)
     predict(fit, newdata = list(train_fd = train_fd), type = "labels")
     sum_tL=sum_tL+Sys.time()-start
     valid_foLR<<-valid_foLR+1
    }, error = function(e) {
      message(sprintf("Errore al fold %d: %s", k, e$message))
    })
    print(b)
  }
  return(list(time_foCCA=sum_tC/100,time_foLR=sum_tL/valid_foLR,valid_foLR=valid_foLR))
}

set.seed(110203)
mean_time()




#0.03842616*60/0.004830465
