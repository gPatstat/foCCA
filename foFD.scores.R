foFD.scores <- function(fdobj1, foFD.fit)
{
  library(fda)
  
  canwtcoef1=foFD.fit$fdawtfd1$coef
  if (!(inherits(fdobj1, "fd"))) stop(
    "Argument FDOBJ1 not a functional data object.")
  
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
