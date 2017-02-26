library(ez)
# Number of simulations: P-values where df based on ppt and 
# p-values where df based on trial - Power of tests
nSim<-1000
pvals <- array(numeric(0), c(2,nSim))
for (runs in c(1:nSim)){
  n<-10       #total ppt
  ntrial<-100  #total trial
  
  cond1ppt_m<-rnorm(n,5,1)
  
  cond1DV <- numeric(0)
  cond2DV <- numeric(0)
  for (i in c(1:n)){
    cond1DV <- c(cond1DV, rnorm(ntrial, cond1ppt_m[i], 0.8))
    cond2DV <- c(cond2DV, rnorm(ntrial, cond1ppt_m[i], 0.8))
  }
  data <- data.frame(
    ppt=factor(c(sort(rep(c(1:n),ntrial)),sort(rep(c(1:n),ntrial)))),
    trial=factor(rep(1:(ntrial*n*2))),
    IV=factor(sort(rep(1:2,n*ntrial))),
    DV=c(cond1DV, cond2DV)
  )
  
  # ttest where trial is the case/Ss identifier
  tt1<-t.test(data$DV[data$IV==1], data$DV[data$IV==2], var.equal=TRUE,paired=FALSE)
  
  # ttest where ppt is the case/Ss identifier
  data<-aggregate(DV ~ ppt+IV, mean, data=data)
  tt2<-t.test(data$DV[data$IV==1], data$DV[data$IV==2], var.equal=TRUE, paired=TRUE)
  
  pvals[1,runs]<-tt1$p.value
  pvals[2,runs]<-tt2$p.value
  print(runs)
}

### PLOTS
breaks<-100
hist(pvals[1,], breaks=breaks,col="grey", main = "H0 is true: df based on trial")
abline(h=nSim/breaks, col = "red", lty=3)
paste("FP when based on trial:", length(pvals[1,pvals[1,]<0.05])/nSim)

hist(pvals[2,], breaks=breaks,col="grey", main = "H0 is true: df based on ppt")
abline(h=nSim/breaks, col = "red", lty=3)
paste("FP when based on ppt:", length(pvals[2,pvals[2,]<0.05])/nSim)

