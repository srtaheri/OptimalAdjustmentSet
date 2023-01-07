source("/Users/sarataheri/GitHub/OptimalAdjustmentSet/main_functions.R")
#This is EColi small. A smaller version of EColi_medium. 
#It takes 11 minutes to get all the valid adjustment sets. They are stored in the all_valid_adj folder.
EColi_small_dag <- dagitty("
dag {
citX [latent]
chiX [latent]
crp -> fur
crp -> citX
crp -> dpiA
crp -> dpiB
crp -> dcuR
crp -> oxyR
fur -> fnr
fur -> aspC
oxyR -> fur
rpoD -> chiX
rpoD -> crp
rpoD -> fur
rpoD -> oxyR
rpoD -> fnr
rpoD -> dcuR
rpoD -> dpiB
rpoD -> hyaA
rpoD -> appY
rpoD -> hyaB
rpoD -> narL
rpoD -> hyaF
chiX -> dpiA
fnr -> aspC
fnr -> citX
fnr -> dpiA
fnr -> dpiB
fnr -> dcuR
fnr -> narL
narL -> dcuR
narL -> dpiA
narL -> citX
narL -> dpiB
narL -> hyaA
narL -> hyaB
narL -> hyaF
dpiA -> citX
dpiA -> dpiB
dpiA -> appY
appY -> hyaA
appY -> hyaB
appY -> hyaF
citX -> dpiB
}
")

EColi_small_string <- "
dag {
citX [latent]
chiX [latent]
crp -> fur
crp -> citX
crp -> dpiA
crp -> dpiB
crp -> dcuR
crp -> oxyR
fur -> fnr
fur -> aspC
oxyR -> fur
rpoD -> chiX
rpoD -> crp
rpoD -> fur
rpoD -> oxyR
rpoD -> fnr
rpoD -> dcuR
rpoD -> dpiB
rpoD -> hyaA
rpoD -> appY
rpoD -> hyaB
rpoD -> narL
rpoD -> hyaF
chiX -> dpiA
fnr -> aspC
fnr -> citX
fnr -> dpiA
fnr -> dpiB
fnr -> dcuR
fnr -> narL
narL -> dcuR
narL -> dpiA
narL -> citX
narL -> dpiB
narL -> hyaA
narL -> hyaB
narL -> hyaF
dpiA -> citX
dpiA -> dpiB
dpiA -> appY
appY -> hyaA
appY -> hyaB
appY -> hyaF
citX -> dpiB
}
"

simplified_string <- generate_simplified_graph_string(g = EColi_small_dag, g_string = EColi_small_string)
EColi_small_dag_simplified = dagitty(simplified_string)
all_adj_set = all_valid_adj_sets(g = EColi_small_dag_simplified, exposure = "fur", outcome = "dpiA")

