# #################### Scenario B ##############################
scenarioB=function(q,n_basis=10, n=1000, rgval=c(0,100),n_order=4,sig=1){
  
  # sample levels uniformly among {0,...,(nlevs-1)}
  levs=sample(0:8,n,replace=T)
  library(fda)

  bobj=create.bspline.basis(rangeval = rgval, norder = n_order, nbasis=n_basis)
  means_tot=matrix(runif(n_basis,-1,1),nrow=n,ncol=n_basis,byrow = T)
  sd_tot=array(sig,n_basis)
  coefs_tot=matrix(rnorm(n*n_basis),n,n_basis)
  
  # Variability of low levels
  
  means_tot1=matrix(runif(n_basis,-1,1),nrow=n,ncol=n_basis,byrow = T)
  
  # Variability of high levels
  
  means_tot2=matrix(runif(n_basis,-1,1),nrow=n,ncol=n_basis,byrow = T)
  
  # Add increments
  means_tot=means_tot+sweep(means_tot1, MARGIN=1,(q*pmin(4,levs)), `*`)+sweep(means_tot2, MARGIN=1, (pmax(0,levs-4)), `*`)
  coefs_tot=coefs_tot%*%diag(sd_tot,n_basis,n_basis)+means_tot
  fd.objects=fd(coef=t(coefs_tot),basisobj=bobj)
  return(list("fun"=fd.objects,"levels"=levs))
}

