library(tidyverse)
library(psych)
library(dagitty)

source("../ContinuousPGM.R")

ecoli = read_csv("real_data/obs_data.csv")
n=nrow(ecoli)

set.seed(1)
#ecoli = ecoli %>% filter(oxyR>2.5,fur>6,cra>4)
ecoli = ecoli %>% filter(oxyR>2.5,cra>4)
#ecoli = ecoli %>% filter(oxyR>2.5)

#write.csv(ecoli,"real_data/obs_data_filter.csv")

EColi_dagitty_DAG <- dagitty("
dag {
crp -> dcuR
crp -> dpiA
crp -> fur
crp -> oxyR
dcuR -> dpiA
fnr -> dcuR
fnr -> dpiA
fnr -> narL
fur -> dpiA
fur -> fnr
narL -> dcuR
narL -> dpiA
oxyR -> fur
phoB -> cra
rpoD -> crp
rpoD -> dcuR
rpoD -> dpiA
rpoD -> fnr
rpoD -> fur
rpoD -> narL
rpoD -> oxyR
rpoD -> phoB
rpoH -> cra
}
")

cond_tests = localTests(EColi_dagitty_DAG,data=ecoli)
failed_tests = cond_tests %>% filter(p.value<1e-3)

#Set up initial DAG

ecoli_DAG = c("rpoD ~ 1",
              "rpoH ~ 1",
              "phoB ~ rpoD",
              "crp ~ rpoD",
              "cra ~ phoB + rpoH",
              "oxyR ~ rpoD + crp",
              "fur ~ crp + rpoD + oxyR",
              "fnr ~ fur + rpoD",
              "narL ~ fnr + rpoD",
              "dcuR ~ fnr + crp + rpoD + narL",
              "dpiA ~ fur + crp + fnr + rpoD + dcuR + narL")

nodenames = c("rpoD","rpoH","phoB", "crp", "cra",
              "oxyR","fur","fnr","narL",
              "dcuR","dpiA")
names(ecoli_DAG) = nodenames


#Initialize families

ecoli_families = rep(list(gaussian(link=identity)),length(ecoli_DAG))
names(ecoli_families) = nodenames

#Initialize methods
ecoli_methods = rep("glm",length(ecoli_DAG))
names(ecoli_methods) = nodenames

# Modify BN parameterizaton and distributions 

ecoli_families[["rpoD"]] = Gamma(link=log)
ecoli_families[["rpoH"]] = Gamma(link=log)

ecoli_DAG["phoB"] = "phoB ~ s(rpoD)"
ecoli_methods["phoB"] = "gam"

ecoli_families[["crp"]] = Gamma(link=log)
ecoli_DAG["crp"] = "crp ~ s(rpoD)"
ecoli_methods["crp"] = "gam"


ecoli_families[["oxyR"]] = Gamma(link=log)
ecoli_DAG["oxyR"] = "oxyR ~ s(rpoD) + crp"
ecoli_methods["oxyR"] = "gam"

ecoli_families[["fur"]] = Gamma(link=log)
ecoli_DAG["fur"] = "fur ~ crp + rpoD + s(oxyR)"
ecoli_methods["fur"] = "gam"

ecoli_DAG["fnr"] = "fnr ~ s(fur) + s(rpoD)"
ecoli_methods["fnr"] = "gam"

ecoli_families[["narL"]] = Gamma(link=log)
ecoli_DAG["narL"] = "narL ~ fnr + s(rpoD) + fnr:rpoD"
ecoli_methods["narL"] = "gam"

ecoli_DAG["dcuR"] = "dcuR ~ s(fnr) + s(crp) + rpoD + narL + crp:narL + crp:rpoD"
ecoli_methods["dcuR"] = "gam"

ecoli_DAG["dpiA"] = "dpiA ~ fur + crp + s(fnr) + rpoD + dcuR + narL + rpoD:dcuR"
ecoli_methods["dpiA"] = "gam"


## Train and Simulate function 

train_and_simulate = function(data,n_train=nrow(data),n_sim=n_train,n_datasets=1000, seed = 10){
  n = nrow(data)
  stopifnot(n_train <= n)
  set.seed(seed)
  BN = BayesianNetwork$new(formvec=ecoli_DAG,families=ecoli_families,methods=ecoli_methods) 
  
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


#Usage: train_and_simulate(data=ecoli,n_train=1000,n_sim=1000,n_datasets=1000) 
EColi_BN_sim <- train_and_simulate(data=ecoli,n_train=nrow(ecoli),n_sim=nrow(ecoli),n_datasets=1000)

for (i in 1:1000) {
  EColi_BN_sim[[i]] <- as.data.frame(EColi_BN_sim[[i]])
  EColi_BN_sim[[i]] <- subset(EColi_BN_sim[[i]], select = -c(rownum) )
}

saveRDS(EColi_BN_sim_N264, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/EColi/BN_data/BN_data.RData")
