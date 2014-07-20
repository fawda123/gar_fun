library(clusterGeneration)
library(devtools)
library(nnet)
library(RSNNS)

seed.val<-2
set.seed(seed.val)

num.vars<-6
num.obs<-5000

#input variables
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)

#output variables
parms<-runif(num.vars,-10,10)
y1<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=10)
parms2<-runif(num.vars,-10,10)
y2<-rand.vars %*% matrix(parms2) + rnorm(num.obs,sd=10)

#final datasets
rand.vars<-data.frame(rand.vars)
resp<-data.frame(y1,y2)
names(resp)<-c('Y1','Y2')
dat.in<-data.frame(resp,rand.vars)

#nnet function from nnet package
mod1<-nnet(rand.vars,resp,data=dat.in,size=10,linout=T)

#mlp function from RSNNS package
mod2<-mlp(rand.vars, resp, size=c(5, 6),linOut=T)

# linear mods for comparison
lm_mod1 <- lm('Y1 ~ X1 + X2 + X3 + X4 + X5 + X6', data.frame(resp, rand.vars))
lm_mod2 <- lm('Y2 ~ X1 + X2 + X3 + X4 + X5 + X6', data.frame(resp, rand.vars))

# relative importance from gar.fun
mod1_imp1 <- c(gar.fun('Y1', mod1, bar.plot = F))[[1]]
mod1_imp2 <- c(gar.fun('Y2', mod1, bar.plot = F))[[1]]

mod2_imp1 <- c(gar.fun('Y1', mod2, bar.plot = F))[[1]]
mod2_imp2 <- c(gar.fun('Y2', mod2, bar.plot = F))[[1]]

# plot comparisons
par(mfrow = c(2, 3))
plot(coefficients(lm_mod1)[-1], mod1_imp1)
plot(coefficients(lm_mod1)[-1], mod2_imp1)
plot(mod1_imp1, mod2_imp1)
plot(coefficients(lm_mod2)[-1], mod1_imp2)
plot(coefficients(lm_mod2)[-1], mod2_imp2)
plot(mod1_imp2, mod2_imp2)

######
# only one response variable
# set.seed(4)
num.vars<-6
num.obs<-5000

#define correlation matrix for explanatory variables 
#define actual parameter values
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
parms<-runif(num.vars,-10,10)
y<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=10)

#prep data and create neural networks
y<-data.frame((y-min(y))/(max(y)-min(y)))
names(y)<-'y'
rand.vars<-data.frame(rand.vars)
mod1<-nnet(rand.vars,y,size=12,linout=T)
mod2<-mlp(rand.vars, y, size=c(4, 6),linOut=T)

# importance from each model
mod1_imp1 <- c(gar.fun('y', mod1, bar.plot = F))[[1]]
mod2_imp1 <- c(gar.fun('y', mod2, bar.plot = F))[[1]]

# compare
par(mfrow = c(3, 1))
plot(parms, mod1_imp1)
plot(parms, mod2_imp1)
plot(mod1_imp1, mod2_imp1)
