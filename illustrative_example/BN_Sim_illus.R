library(tidyverse)
library(psych)

source("../ContinuousPGM.R")

fig4data = read_csv("real_data/illustrative_obs_data2.csv")

fig4data
#hist(fig4data$Z5)

#ggplot(fig4data,aes(x=Z3,y=Y,col=X)) + geom_point() + geom_smooth(method="gam")

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

train_and_simulate = function(data,n_train=nrow(data),n_sim=n_train,n_datasets=1000, seed = 10){
  n = nrow(data)
  stopifnot(n_train <= n)
  set.seed(seed)
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
#generate results
illus_simdata_1000 <- train_and_simulate(data=fig4data, n_train=1000, n_sim=1000, n_datasets=1000, seed = 10) 
illus_simdata_500 <- train_and_simulate(data=fig4data, n_train=500, n_sim=500, n_datasets=1000, seed = 10) 
illus_simdata_100 <- train_and_simulate(data=fig4data, n_train=100, n_sim=100, n_datasets=1000, seed = 10) 
illus_simdata_50 <- train_and_simulate(data=fig4data, n_train=100, n_sim=50, n_datasets=1000, seed = 10) 
illus_simdata_30 <- train_and_simulate(data=fig4data, n_train=30, n_sim=30, n_datasets=1000, seed = 10) 
illus_simdata_15 <- train_and_simulate(data=fig4data, n_train=15, n_sim=15, n_datasets=1000, seed = 10) 

# for (j in 1:1000) {
#   illus_simdata_50[[j]] <- as.data.frame(illus_simdata_50[[j]][,-1])
# }

#save results
saveRDS(illus_simdata_1000, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/BN_data_generation/data2/illus_simdata_1000.RData")
saveRDS(illus_simdata_500, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/BN_data_generation/data2/illus_simdata_500.RData")
saveRDS(illus_simdata_100, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/BN_data_generation/data2/illus_simdata_100.RData")
saveRDS(illus_simdata_50, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/BN_data_generation/data2/illus_simdata_50.RData")
saveRDS(illus_simdata_30, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/BN_data_generation/data2/illus_simdata_30.RData")
saveRDS(illus_simdata_15, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/BN_data_generation/data2/illus_simdata_15.RData")
