################# Introduction to simulations (example plots) ###################

source("scenarioA.R")
source("scenarioB.R")

library(fields)
library(viridis)

colev=magma(9)

########### Example scenario A ##############

set.seed(181292)

par(mfrow=c(2,2))

sim.fd=scenarioA(ratio=0.1,n_basis=10)
plot(sim.fd$fun,col=colev[sim.fd$levels+1], main=expression(bold(paste('Simulated scenario (a) ', q[a], '=0.1'))),ylab="X",cex.main=2,lty=1,cex.lab=1.5,ylim=c(-40,60))     
sim.fd=scenarioA(ratio=0.25,n_basis=10)
plot(sim.fd$fun,col=colev[sim.fd$levels+1], main=expression(bold(paste('Simulated scenario (a) ', q[a], '=0.25'))),ylab="X",cex.main=2,lty=1,cex.lab=1.5,ylim=c(-40,60))
sim.fd=scenarioA(ratio=0.5,n_basis=10)
plot(sim.fd$fun,col=colev[sim.fd$levels+1], main=expression(bold(paste('Simulated scenario (a) ', q[a], '=0.5'))),ylab="X",cex.main=2,lty=1,cex.lab=1.5,ylim=c(-40,60))
sim.fd=scenarioA(ratio=1,n_basis=10)
plot(sim.fd$fun,col=colev[sim.fd$levels+1], main=expression(bold(paste('Simulated scenario (a) ', q[a], '=1'))),ylab="X",cex.main=2,lty=1,cex.lab=1.5,ylim=c(-40,60))


############### Example scenario B ##############

set.seed(181292)

par(mfrow=c(2,2))

sim.fd=scenarioB(q=0.1,n_basis=10)
plot(sim.fd$fun,col=colev[sim.fd$levels+1], main=expression(bold(paste('Simulated scenario (b) ', q[b], '=0.1'))),ylab="X",cex.main=2,lty=1,cex.lab=1.5,ylim=c(-6,6))     
sim.fd=scenarioB(q=0.25,n_basis=10)
plot(sim.fd$fun,col=colev[sim.fd$levels+1], main=expression(bold(paste('Simulated scenario (b) ', q[b], '=0.25'))),ylab="X",cex.main=2,lty=1,cex.lab=1.5,ylim=c(-6,6))
sim.fd=scenarioB(q=0.5,n_basis=10)
plot(sim.fd$fun,col=colev[sim.fd$levels+1], main=expression(bold(paste('Simulated scenario (b) ', q[b], '=0.5'))),ylab="X",cex.main=2,lty=1,cex.lab=1.5,ylim=c(-6,6))
sim.fd=scenarioB(q=1,n_basis=10)
plot(sim.fd$fun,col=colev[sim.fd$levels+1], main=expression(bold(paste('Simulated scenario (b) ', q[b], '=1'))),ylab="X",cex.main=2,lty=1,cex.lab=1.5,ylim=c(-6,6))

