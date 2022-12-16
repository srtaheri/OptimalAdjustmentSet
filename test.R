g_string = "dag {
chiX [latent]
citX [latent]
gcvB [latent]
appY -> appA
appY -> appB
appY -> appX
appY -> hyaA
appY -> hyaB
appY -> hyaF
arcA -> aceE
arcA -> appY
arcA -> citX
arcA -> cydD
arcA -> dpiA
arcA -> dpiB
arcA -> fnr
arcA -> gcvB
arcA -> hyaA
arcA -> hyaB
arcA -> hyaF
arcA -> mdh
arcA -> rpoS
btsR -> mdh
chiX -> dpiA
chiX -> dpiB
citX -> dpiB
cra -> cyoA
crp -> aceE
crp -> cirA
crp -> citX
crp -> cyoA
crp -> dcuR
crp -> dpiA
crp -> dpiB
crp -> exuT
crp -> fur
crp -> gadX
crp -> mdh
crp -> oxyR
crp -> srIR
crp <-> fis
cspA -> hns
dcuR -> dpiA
dcuR -> dpiB
dpiA -> appY
dpiA -> citC
dpiA -> citD
dpiA -> citX
dpiA -> dpiB
dpiA -> exuT
dpiA -> mdh
dsrA -> hns
dsrA -> lrp
dsrA -> rpoS
fis -> cyoA
fis -> gadX
fis -> hns
fis -> hyaA
fis -> hyaB
fis -> hyaF
fnr -> aceE
fnr -> amtB
fnr -> aspC
fnr -> citX
fnr -> cydD
fnr -> cyoA
fnr -> dcuR
fnr -> dpiA
fnr -> dpiB
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
hns -> appY
hns -> srIR
hns -> ydeO
hns -> yjjQ
ihfA -> crp
ihfA -> fnr
ihfA -> ihfB
ihfB -> fnr
iscR -> hyaA
iscR -> hyaB
iscR -> hyaF
lrp -> aspC
lrp -> soxS
modE -> narL
narL -> citX
narL -> cydD
narL -> dcuR
narL -> dpiA
narL -> dpiB
narL -> hcp
narL -> hyaA
narL -> hyaB
narL -> hyaF
narP -> hyaA
narP -> hyaB
narP -> hyaF
oxyR -> fur
oxyR -> hcp
phoB -> cra
rpoD -> aceE
rpoD -> appY
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
rpoD -> hyaA
rpoD -> hyaB
rpoD -> hyaF
rpoD -> ihfB
rpoD -> mdh
rpoD -> narL
rpoD -> oxyR
rpoD -> phoB
rpoD -> soxS
rpoD -> srIR
rpoD -> ydeO
rpoD -> yjjQ
rpoH -> cra
rpoS -> aceE
rpoS -> appY
rpoS -> hyaA
rpoS -> hyaB
rpoS -> hyaF
rpoS -> ihfA
rpoS -> ihfB
rpoS -> oxyR
soxS -> fur
srIR -> gutM
ydeO -> hyaA
ydeO -> hyaB
ydeO -> hyaF
}
"
g <- dagitty(g_string)

simplified_g_string <- generate_simplified_graph_string(g = g, g_string = g_string)

dagitty_input_str <- generate_dagitty_input_string(g_string = simplified_g_string)

mydag = dagitty(dagitty_input_str)

start_time = Sys.time()
mydag_adj_all = adjustmentSets(x = mydag, exposure = "fur", outcome = "dpiA", type = "all")
end_time = Sys.time()
end_time - start_time


library(dagitty)
library(ggdag)
library(ggplot2)
g_string <- "dag {
chiX [latent]
citX [latent]
gcvB [latent]
appY -> appA
appY -> appB
appY -> appX
appY -> hyaA
appY -> hyaB
appY -> hyaF
arcA -> aceE
arcA -> appY
arcA -> citX
arcA -> cydD
arcA -> dpiA
arcA -> dpiB
arcA -> fnr
arcA -> gcvB
arcA -> hyaA
arcA -> hyaB
arcA -> hyaF
arcA -> mdh
arcA -> rpoS
btsR -> mdh
chiX -> dpiA
chiX -> dpiB
citX -> dpiB
cra -> cyoA
crp -> aceE
crp -> cirA
crp -> citX
crp -> cyoA
crp -> dcuR
crp -> dpiA
crp -> dpiB
crp -> exuT
crp -> fur
crp -> gadX
crp -> mdh
crp -> oxyR
crp -> srIR
crp <-> fis
cspA -> hns
dcuR -> dpiA
dcuR -> dpiB
dpiA -> appY
dpiA -> citC
dpiA -> citD
dpiA -> citX
dpiA -> dpiB
dpiA -> exuT
dpiA -> mdh
dsrA -> hns
dsrA -> lrp
dsrA -> rpoS
fis -> cyoA
fis -> gadX
fis -> hns
fis -> hyaA
fis -> hyaB
fis -> hyaF
fnr -> aceE
fnr -> amtB
fnr -> aspC
fnr -> citX
fnr -> cydD
fnr -> cyoA
fnr -> dcuR
fnr -> dpiA
fnr -> dpiB
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
hns -> appY
hns -> srIR
hns -> ydeO
hns -> yjjQ
ihfA -> crp
ihfA -> fnr
ihfA -> ihfB
ihfB -> fnr
iscR -> hyaA
iscR -> hyaB
iscR -> hyaF
lrp -> aspC
lrp -> soxS
modE -> narL
narL -> citX
narL -> cydD
narL -> dcuR
narL -> dpiA
narL -> dpiB
narL -> hcp
narL -> hyaA
narL -> hyaB
narL -> hyaF
narP -> hyaA
narP -> hyaB
narP -> hyaF
oxyR -> fur
oxyR -> hcp
phoB -> cra
rpoD -> aceE
rpoD -> appY
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
rpoD -> hyaA
rpoD -> hyaB
rpoD -> hyaF
rpoD -> ihfB
rpoD -> mdh
rpoD -> narL
rpoD -> oxyR
rpoD -> phoB
rpoD -> soxS
rpoD -> srIR
rpoD -> ydeO
rpoD -> yjjQ
rpoH -> cra
rpoS -> aceE
rpoS -> appY
rpoS -> hyaA
rpoS -> hyaB
rpoS -> hyaF
rpoS -> ihfA
rpoS -> ihfB
rpoS -> oxyR
soxS -> fur
srIR -> gutM
ydeO -> hyaA
ydeO -> hyaB
ydeO -> hyaF
}
"
g <- dagitty(g_string)

simplified_g_string <- generate_simplified_graph_string(g = g, g_string = g_string)

dagitty_input_str <- generate_dagitty_input_string(g_string = simplified_g_string)

mydag = dagitty(dagitty_input_str)








all_valid_adj_sets <- function(g, exposure, outcome) {
  
  #adj_minimal <- adjustmentSets(x = g, exposure = exposure, outcome = outcome, type = "minimal")
  forbidden_vars <- get_forbidden_vars(g = g, from = exposure, to = outcome)
  covariates <- setdiff(names(g), forbidden_vars)
  #adj_canonical <- adjustmentSets(x = g, exposure = exposure, outcome = outcome, type = "canonical")
  my_combi <- unlist(lapply(1:length(covariates),    # Get all combinations
                            combinat::combn, 
                            x = covariates,
                            simplify = FALSE), 
                     recursive = FALSE)
  
  r = c()
  for (my_combi_var_idx in 1:length(my_combi)) {
    if(isAdjustmentSet(x = g, Z = my_combi[[my_combi_var_idx]], exposure = exposure, outcome = outcome) == FALSE) {
      r = c(r,my_combi_var_idx)
    }
  }
  if (length(r) > 0) {
    my_combi <- my_combi[-r]
  }

  return(my_combi)
}