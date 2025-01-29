pca.scores <- function(fdobj1, pca.fit, centerfns=TRUE)
{
  library(fda)
  
  canwtcoef1=pca.fit$harmonics$coefs
  if (!(inherits(fdobj1, "fd"))) stop(
    "Argument FDOBJ1 not a functional data object.")
  
  if (centerfns){
    #  center the data by subtracting the mean function
    for(d in 1:dim(fdobj1$coefs)[2]){
      sub=as.numeric(fdobj1$coefs[,d]-pca.fit$meanfd$coefs)
      fdobj1$coefs[,d]=sub
    }
  }
  #  extract dimensions for both functional data objects
  
  
  coef1  <- fdobj1$coefs
  coefd1 <- dim(coef1)
  ndim1  <- length(coefd1)
  nrep   <- coefd1[2]
  
  #  check that functional data objects are univariate
  if (ndim1 > 2) stop(
    "Function is not univariate.")
  
  #  extract basis information for both objects
  
  basisobj1  <- fdobj1$basis
  nbasis1    <- basisobj1$nbasis
  dropind1   <- basisobj1$dropind
  type1      <- basisobj1$type
  
  
  #   Set up cross product matrices
  
  Jmat1 <- eval.penalty(basisobj1, 0)
  Jx    <- t(Jmat1 %*% coef1)
  
  #  set up canonical variable values
  
  canvarvalues1 <- Jx %*% canwtcoef1
  
  #  set up return list
  
  return(canvarvalues1)
  
}