#This is EColi small. A smaller version of EColi_medium. 
#It takes 11 minutes to get all the valid adjustment sets. They are stored in the all_valid_adj folder.
EColi_small_dag <- dagitty("
dag {
dsrA [latent]
gcvB [latent]
arcA -> dpiA
arcA -> lrp
arcA -> oxyR
arcA -> rpoS
crp -> dpiA
crp -> fis
crp -> fur
crp -> oxyR
dsrA -> lrp
dsrA -> rpoS
fur -> dpiA
gcvB -> lrp
gcvB -> oxyR
ihfA -> crp
ihfA -> dpiA
ihfA -> ihfB
ihfB -> dpiA
lrp -> soxS
oxyR -> fur
rpoD -> arcA
rpoD -> crp
rpoD -> dpiA
rpoD -> fis
rpoD -> fur
rpoD -> ihfB
rpoD -> lrp
rpoD -> oxyR
rpoD -> rpoS
rpoD -> soxS
rpoS -> ihfA
rpoS -> ihfB
rpoS -> oxyR
soxS -> fur
}
")
