#################### Scenario A ##############################

scenarioA=function(ratio,n_basis=10, n=1000, rgval=c(0,100),n_order=4){

# sample levels uniformly among {0,...,(nlevs-1)}
levs=sample(0:8,n,replace=T)

library(fda)

bobj=create.bspline.basis(rangeval = rgval, norder = n_order, nbasis=n_basis)

# General variability
coefs_tot=matrix(rnorm(n*n_basis),n,n_basis)
means_tot=matrix(runif(n_basis,-10,10),nrow=n,ncol=n_basis,byrow = T)
sd_tot=runif(n_basis,0,10)
coefs_tot=coefs_tot%*%diag(sd_tot,n_basis,n_basis)+means_tot

# Level variability
increments1=rnorm(8,10*ratio,1)
mean_lev1=c(0,cumsum(increments1))[levs+1]
basis1=which.min(sd_tot)
coefs_tot[,basis1]=coefs_tot[,basis1]+mean_lev1
fd.objects=fd(coef=t(coefs_tot),basisobj=bobj)
return(list("fun"=fd.objects,"levels"=levs))
}

