library(tidyverse)

#IGFdata = readRDS("data/observational_igf.RData")

origdata = read_csv("data/obs_data.csv")

origdata$U = rnorm(nrow(origdata))


source("../ContinuousPGM.R")

ggplot(sampledata,aes(x=PI3K,y=AKT)) + geom_point() + geom_smooth(method="gam")

fig_5_DAG = c("U~1",
              "SOS~U",
              "Ras ~ SOS",
              "PI3K~ U + s(Ras)",
              "AKT ~ s(PI3K)",
              "Raf ~ AKT",
              "Mek ~ Raf",
              "Erk ~ Mek")

fig_5_methods = rep("glm",length(fig_5_DAG))
fig_5_methods[[4]] = "gam"
fig_5_methods[[5]] = "gam"


## Train and Simulate function 

train_and_simulate = function(data,n_train=nrow(data),n_sim=n_train,n_datasets=1000){
  n = nrow(data)
  stopifnot(n_train <= n)
  
  BN = BayesianNetwork$new(formvec=fig_5_DAG,methods=fig_5_methods) 
  
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

## Training and Simulating Data 

start = proc.time()
simdata_full = train_and_simulate(data=origdata)
end = proc.time()
print(end-start) #took 15 secs 

simdata_500 = train_and_simulate(data=origdata,n_train = 500)

simdata_100 = train_and_simulate(data=origdata,n_train = 100)

simdata_20 = train_and_simulate(data=origdata,n_train = 20)

#Saving

saveRDS(simdata_full,"BN_simdata/IGF_simdata_1000.rdata")
saveRDS(simdata_500,"BN_simdata/IGF_simdata_500.rdata")
saveRDS(simdata_100,"BN_simdata/IGF_simdata_100.rdata")
saveRDS(simdata_20,"BN_simdata/IGF_simdata_20.rdata")

#Checking pairs plots 

library(psych)

pairs.panels(origdata[,-c(1:3,11)])
pairs.panels(simdata_full[[1]][,-c(1:2)])
