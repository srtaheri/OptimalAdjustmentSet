source("/Users/sarataheri/GitHub/OptimalAdjustmentSet/main_functions.R")
#This is EColi medium that is the same as EColi_large, but all the descendants of the effect and the single nodes (no edge is connected to them) are removed!
# It takes 1.004796 hours to get all the valid adjustment sets. They are stored in the all_valid_adj folder.
EColi_medium_dag <- dagitty("
dag {
gcvB [latent]
arcA -> aceE
arcA -> cydD
arcA -> dpiA
arcA -> fnr
arcA -> gcvB
arcA -> rpoS
cra -> cyoA
crp -> aceE
crp -> cirA
crp -> cyoA
crp -> dcuR
crp -> dpiA
crp -> fis
crp -> fur
crp -> gadX
crp -> oxyR
crp -> srIR
cspA -> hns
dcuR -> dpiA
dsrA -> hns
dsrA -> lrp
dsrA -> rpoS
fis -> cyoA
fis -> gadX
fis -> hns
fnr -> aceE
fnr -> amtB
fnr -> aspC
fnr -> cydD
fnr -> cyoA
fnr -> dcuR
fnr -> dpiA
fnr -> gadX
fnr -> hcp
fnr -> narL
fur -> amtB
fur -> aspC
fur -> cirA
fur -> cyoA
fur -> fnr
gadX -> amtB
gadX -> hns
gcvB -> lrp
gcvB -> oxyR
gcvB -> ydeO
hns -> srIR
hns -> ydeO
hns -> yjjQ
ihfA -> crp
ihfA -> fnr
ihfA -> ihfB
ihfB -> fnr
lrp -> aspC
lrp -> soxS
modE -> narL
narL -> cydD
narL -> dcuR
narL -> dpiA
narL -> hcp
oxyR -> fur
oxyR -> hcp
phoB -> cra
rpoD -> aceE
rpoD -> arcA
rpoD -> cirA
rpoD -> crp
rpoD -> cydD
rpoD -> dcuR
rpoD -> dsrA
rpoD -> fis
rpoD -> fnr
rpoD -> fur
rpoD -> gcvB
rpoD -> hns
rpoD -> ihfB
rpoD -> narL
rpoD -> oxyR
rpoD -> phoB
rpoD -> soxS
rpoD -> srIR
rpoD -> ydeO
rpoD -> yjjQ
rpoH -> cra
rpoS -> aceE
rpoS -> ihfA
rpoS -> ihfB
rpoS -> oxyR
soxS -> fur
srIR -> gutM
}
")

#covid_med_all_adj_set <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/EColi/all_valid_adj/mydag_adj_all_medium.RData")

#find the list of all the covariates
#The treatment, effect, and the covariates that are not latent are c("cra", "phoB", "dpiA", "fur", "modE", "lrp", "ihfB", "ihfA", "rpoS", "rpoD", "fis", "crp", "rpoH", "cspA", "oxyR", "soxS", "arcA")
forbidden_vars <- get_forbidden_vars(g = EColi_medium_dag, from = "fur", to = "dpiA")
covariates <- setdiff(names(EColi_medium_dag), forbidden_vars)
covariates

## read the data
whole_data <- read.csv("/Users/sarataheri/GitHub/OptimalAdjustmentSet/EColi/data/Expression_data.csv")

#Extract the related rows:
selected_rows = which(whole_data$log.TPM %in% c("b4401" ,"b0080","b3357", "b0620", "b3261", "b0683", "b1712", "b0912", "b0889",  "b0761", "b3961", "b0399", "b3067", "b3461", "b2741", "b4062"))
data <- whole_data[selected_rows,]
n_df = data.frame("names" = c("cra", "phoB", "dpiA", "fur", "modE", "lrp", "ihfB", "ihfA", "rpoS", "rpoD", "fis", "crp", "rpoH", "oxyR", "soxS", "arcA"))
data <- cbind(n_df,data)
#Create observational data
remove = c("fur__delfur_dpd__1",
           "fur__delfur_dpd__2",
           "fur__delfur_fe2__1",
           "fur__delfur_fe2__2",
           "crp__delcrp_glyc__3",
           "ica__bw_delpurR_cytd__2",
           "pal__tartr_ale28__1", 
           "ica__bw_delpurR_cytd__2",
           "ssw__glc_ac_glc1__1",
           "X42c__wt_42c__1",
           "rpoB__rpoBE672K_lb__2",
           "rpoB__rpoBE672K_glc__2",
           "rpoB__rpoBE672K_glc__1",
           "rpoB__rpoBE546V_lb__1")
obs_data = data[ , !(names(data) %in% remove)]
obs_data = t(obs_data[,3:ncol(obs_data)])
colnames(obs_data) = c("cra", "phoB", "dpiA", "fur", "modE", "lrp", "ihfB", "ihfA", "rpoS", "rpoD", "fis", "crp", "rpoH", "oxyR", "soxS", "arcA")
rownames(obs_data) = seq(1:nrow(obs_data))
obs_data = as.data.frame(obs_data)
saveRDS(obs_data, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/EColi/data/obs_data.RData")
#Create interventional data
intv_data <- as.data.frame(as.numeric(data[which(data$names == "dpiA"), c("fur__delfur_dpd__1", "fur__delfur_fe2__1", "fur__delfur_fe2__2")]))
mean(intv_data[1,])
