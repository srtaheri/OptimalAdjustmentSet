library(tidyverse)
library(psych)

source("../ContinuousPGM.R")

fig4data = read_csv("real_data/obs_data.csv")

fig4data
hist(fig4data$Z5)

ggplot(fig4data,aes(x=Z3,y=Y,col=X)) + geom_point() + geom_smooth(method="gam")

fig_4_DAG = c("Z1 ~ 1",
              "Z3 ~ 1",
              "Z4 ~ 1",
              "Z5 ~ 1",
              "Z2 ~ Z1 + Z3",
              "X ~ Z1 + Z4 + Z5",
              "Y ~ X + Z5 + Z3")

fig4_families = rep(list(gaussian(link=identity)),length(fig_4_DAG))
fig4_families[[6]] = binomial()


## Train and Simulate function 

train_and_simulate = function(data,n_train=nrow(data),n_sim=n_train,n_datasets=1000){
  n = nrow(data)
  stopifnot(n_train <= n)
  
  BN = BayesianNetwork$new(formvec=fig_4_DAG,families=fig4_families) 
  
  if(n_train < nrow(data)){
    sampledata = data[sample(1:n,n_train,replace=FALSE),]
  }else{
    sampledata = data
  }
  
  BN$fit(sampledata)
  
  datasets = vector(mode="list",length=n_datasets)
  
  for(i in 1:n_datasets){
    datasets[[i]] = BN$sample(n_sim)
  }
  
  return(datasets)
}

#Usage: train_and_simulate(data=fig4data,n_train=1000,n_sim=1000,n_datasets=1000) 
#1000 datasets with 1000 points simulated from model trained on 1000
