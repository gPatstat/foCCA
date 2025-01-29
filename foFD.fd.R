foFD.fd <- function(fdobj1, levels, ncan = 2,lambda1)
{ library(fda)
  library(MASSExtra)
  library(pracma)

  if (!(inherits(fdobj1, "fd"))) stop(
    "Argument FDOBJ1 not a functional data object.")
  
  labels=apply(levels, 1,sum)
  
  #  extract dimensions for functional data object
  coef1  <- fdobj1$coefs
  coefd1 <- dim(coef1)
  ndim1  <- length(coefd1)
  n_splines <- coefd1[1]
  nrep   <- coefd1[2]
  
  #  check that functional data object is univariate
  if (ndim1 > 2) stop(
    "Function is not univariate.")
  
  #  extract basis information for fd object
  
  basisobj1  <- fdobj1$basis
  nbasis1    <- basisobj1$nbasis
  dropind1   <- basisobj1$dropind
  type1      <- basisobj1$type                 
                        
  #   Set up cross product matrices
  
  Jmat1 <- eval.penalty(basisobj1, 0) # matrix with coefficients \int_{phi_j*phi_k}
  Jx=t(Jmat1%*%coef1)
  
  ############## Building variance within #############

  # mean_levels: matrix whose each column j is a the mean of the splines' coefficients in level j-1
  
  lv=length(unique(labels))
  mean_levels=matrix(NA, n_splines,lv) # matrix 20 x 9 
  
 
  for(l in 1:lv){
    mean_levels[,l]=colMeans(t(coef1)[(labels==(l-1)),]) 
  }
  
  #Vw: matrix of covariance within 
  
  diff_within=matrix(NA, n_splines,nrep)
  
  for(i in 1:nrep){
    diff_within[,i]=(coef1[,i]-mean_levels[,labels[i]+1])
  }
  
  Jw    <- t(Jmat1%*%diff_within)
  
  # groups_mat deve avere come colonne ha i livelli e come righe i samples
  
  Vw  <- crossprod(Jw)
  
  # Variance between for ordinal labels
  
  nl=table(as.factor(labels))

  mean_levels=matrix(NA, n_splines,lv) # matrix 20 x 9 
  
  for(l in 1:lv){
    mean_levels[,l]=colMeans(t(coef1)[labels==(l-1),]) 
  }
  
  
  diff_between=matrix(NA, n_splines,(lv-1))  # 20 x 8
  
  #check
  diff_0=matrix(NA, n_splines,(lv-1))
  diff_1=matrix(NA, n_splines,(lv-1))
  for(l in 1:(lv-1)){
    n0=nl[l]
    n1=nl[l+1]
    centroid=(n0*mean_levels[,l]+n1*mean_levels[,(l+1)])/(n0+n1)
    diff_0[,l]=sqrt(n0)*(mean_levels[,l]-centroid)
    diff_1[,l]=sqrt(n1)*(mean_levels[,(l+1)]-centroid)
    #mean_nl=(nl[l]+nl[l+1])/2
    #diff_between[,l]=(mean_levels[,l]-mean_levels[,(l+1)])*mean_nl
    #diff_between[,l]=(mean_levels[,l])*(nl[l])
  }
  
  #
  
  Vb0  <- crossprod((t(Jmat1%*%diff_0)))
  Vb1 <- crossprod((t(Jmat1%*%diff_1)))
  Vb<- Vb0+Vb1

  #  get linear differential operators and lambdas
  
  Lfdobj1 <- 2
  #  add roughness penalties if lambdas are positive
  
  Kmat1 <- eval.penalty(basisobj1, Lfdobj1)
  Vw  <- Vw + lambda1 * Kmat1
  #print(Vw)

  #result <- geigen::geigen(Vb,Vw,only.values = F,symmetric=T)
  #result <- eigen((solve(Vw)%*%Vb))
  A=(pinv(Vw)%*%Vb)
  A=nearest_spd(A)
  result <- eigen(A)
  #  set up canonical correlations and coefficients for weight functions
  
  canwtcoef1 <- (result$vectors[,1:ncan])
  #print(canwtcoef1)
  
  canwtfdnames      <- fdobj1$fdnames
  canwtfdnames[[2]] <- "Discriminant Variable"
  names(canwtfdnames)[2] <- "Discriminant functions"
  names(canwtfdnames)[3] <-
    paste("FDA wt. fns. for",names(canwtfdnames)[3])
  #print(canwtcoef1)
  canwtfd1 <- fd(canwtcoef1, basisobj1, canwtfdnames) 
  canwtfd1$fdnames <- fdobj1$fdnames
  
  
  #  set up canonical variable values
  
  canvarvalues1 <- Jx %*% canwtcoef1
  
  
  #  set up return list
  
  ccafd        <- list(canwtfd1, canvarvalues1)
  names(ccafd) <- c("fdawtfd1", "fdavar1")
  
  class(ccafd) <- "cca.fd"
  
  return(ccafd)
  
}



