foCCA.fd <- function(fdobj1, levels, ncan = 2,lambda1=1e-10, lambda2=10^-5,
                   centerfns=TRUE)
{
  library(fda)
  #  check functional data objects
  
  if (!(inherits(fdobj1, "fd"))) stop(
    "Argument FDOBJ1 not a functional data object.")
  
  centerfun=mean.fd(fdobj1)

  if (centerfns) {
    #  center the data by subtracting the mean function
    fdobj1 <- center.fd(fdobj1)
  }
  
  #  extract dimensions for functional data object
  coef1  <- fdobj1$coefs
  coefd1 <- dim(coef1)
  ndim1  <- length(coefd1)
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
  Jmat1 <- eval.penalty(basisobj1, 0)
  Jx    <- t(Jmat1 %*% coef1)
  Jy <- scale(as.matrix(levels),scale=F)
  
  # levels_mat deve avere come colonne ha i livelli e come righe i samples
  
  #Covariance of functional data
  PVxx  <- crossprod(Jx)/nrep
  #Covariance of ordinal data
  PVyy=crossprod(Jy)/nrep
  #Covariance of functional vs ordinal data
  Vxy   <- crossprod(Jx,Jy)/nrep
  
  #  get linear differential operators
  
  Lfdobj1 <- 2
  
  #  add roughness penalties if lambdas are positive
  
  
  if (lambda1 > 0) {
    Kmat1 <- eval.penalty(basisobj1, Lfdobj1)
    PVxx  <- PVxx + lambda1 * Kmat1
  }
  
  # LASSO penalty for theta?
  
  if (lambda2 > 0) {
    Kmat2 <- diag(dim(PVyy)[1])
    PVyy  <- PVyy + lambda2 * Kmat2
  }
  
  #  do eigenanalysis matrix Vxy with respect to metrics PVxx and PVyy
  
  result <- fda::geigen(Vxy, PVxx, PVyy)
  
  #  set up canonical correlations and coefficients for weight functions
  
  canwtcoef1 <- result$Lmat[,1:ncan]
  canwtcoef2 <- result$Mmat[,1:ncan]
  corrs      <- result$values
  
  #   Normalize the coefficients for weight functions
  
  for (j in 1:ncan) {
    temp <- canwtcoef1[,j]
    temp <- temp/sqrt(sum(temp^2))
    canwtcoef1[,j] <- temp
    temp <- canwtcoef2[,j]
    temp <- temp/sqrt(sum(temp^2))
    canwtcoef2[,j] <- temp
  }
  
  #  set up the canonical weight functions
  
  canwtfdnames      <- fdobj1$fdnames
  canwtfdnames[[2]] <- "Canonical Variable"
  names(canwtfdnames)[2] <- "Canonical functions"
  names(canwtfdnames)[3] <-
    paste("CCA wt. fns. for",names(canwtfdnames)[3])
  canwtfd1 <- fd(canwtcoef1, basisobj1, canwtfdnames)
  canwtfd1$fdnames <- fdobj1$fdnames

  
  #  set up canonical variable values
  
  canvarvalues1 <- Jx %*% canwtcoef1
  canvarvalues2 <- canwtcoef2
  
  #  set up return list
  

  
  ccafd        <- list(canwtfd1, corrs, canvarvalues1, canvarvalues2, centerfun)
  names(ccafd) <- c("ccawtfd1", "ccacorr",
                    "ccavar1",  "ccavar2","fd.center")
  
  class(ccafd) <- "cca.fd"
  
  return(ccafd)
  
}

