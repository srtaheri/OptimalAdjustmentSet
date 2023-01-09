source("/Users/sarataheri/GitHub/OptimalAdjustmentSet/main_functions.R")
#This is EColi small. A smaller version of EColi_medium. 
#It takes 11 minutes to get all the valid adjustment sets. They are stored in the all_valid_adj folder.
EColi_small_dag <- dagitty("
dag {
arcA -> dpiA
arcA -> fnr
arcA -> oxyR
arcA -> rpoS
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
rpoD -> arcA
rpoD -> crp
rpoD -> dcuR
rpoD -> dpiA
rpoD -> fnr
rpoD -> fur
rpoD -> narL
rpoD -> oxyR
rpoD -> phoB
rpoH -> cra
rpoS -> oxyR
}
")

forbidden_vars <- get_forbidden_vars(g = EColi_small_dag, from = "fur", to = "dpiA")
covariates <- setdiff(names(EColi_small_dag), forbidden_vars)

all_adj_set = all_valid_adj_sets(g = EColi_small_dag, exposure = "fur", outcome = "dpiA")

