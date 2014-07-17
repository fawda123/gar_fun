require(clusterGeneration)
require(nnet)
library(devtools)
library(RSNNS)

#define number of variables and observations
set.seed(2)
num.vars<-8
num.obs<-10000

#define correlation matrix for explanatory variables 
#define actual parameter values
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
parms<-runif(num.vars,-10,10)
y<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)

#prep data and create neural network
y<-data.frame((y-min(y))/(max(y)-min(y)))
names(y)<-'y'
rand.vars<-data.frame(rand.vars)
mod1<-nnet(rand.vars,y,size=8,linout=T)

#neural net with three hidden layers, 9, 11, and 8 nodes in each
mod2<-mlp(rand.vars, y, size=c(3,5,4),linOut=T)

source_gist(6206737)
source_gist(7471137)

windows()
gar.fun(out.var = 'y', mod.in = mod1)


par(mar=numeric(4),family='serif')
plot.nnet(mod2)

gar.fun(out.var = 'y', mod.in = mod2)
