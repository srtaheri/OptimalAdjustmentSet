source("/Users/sarataheri/GitHub/OptimalAdjustmentSet/main_functions.R")
#This is EColi medium that is the same as EColi_large, but all the descendants of the effect and the single nodes (no edge is connected to them) are removed!
# It takes 1.004796 hours to get all the valid adjustment sets. They are stored in the all_valid_adj folder.
g_string <- "
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
modE -> dpiA
oxyR -> fur
phoB -> cra
rpoD -> arcA
rpoD -> crp
rpoD -> dpiA
rpoD -> fis
rpoD -> fur
rpoD -> ihfB
rpoD -> lrp
rpoD -> oxyR
rpoD -> phoB
rpoD -> rpoS
rpoD -> soxS
rpoH -> cra
rpoS -> ihfA
rpoS -> ihfB
rpoS -> oxyR
soxS -> fur
"

dagitty_input_str <- generate_dagitty_input_string(g_string)

EColi_medium_dag = dagitty(dagitty_input_str)

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
#names are covariates plus exposure and outcome
n_df = data.frame("names" = c("cra", "phoB", "dpiA", "fur", "modE", "lrp", "ihfB", "ihfA", "rpoS", "rpoD", "fis", "crp", "rpoH", "oxyR", "soxS", "arcA"))
data <- cbind(n_df,data)
#Create observational data
remove = c("fur__delfur_dpd__1",
           "fur__delfur_fe2__1",
           "fur__delfur_fe2__2",
           "crp__delcrp_fru__1",
           "crp__delcrp_fru__2",
           "crp__delcrp_fru__3",
           "crp__delcrp_glc__1",
           "crp__delcrp_glc__2",
           "crp__delcrp_glc__3",
           "crp__delcrp_glyc__1",
           "crp__delcrp_glyc__2",
           "crp__delcrp_glyc__3"
           )
obs_data = data[ , !(names(data) %in% remove)]
obs_data = t(obs_data[,3:ncol(obs_data)])
colnames(obs_data) = c("cra", "phoB", "dpiA", "fur", "modE", "lrp", "ihfB", "ihfA", "rpoS", "rpoD", "fis", "crp", "rpoH", "oxyR", "soxS", "arcA")
rownames(obs_data) = seq(1:nrow(obs_data))
obs_data = as.data.frame(obs_data)
#saveRDS(obs_data, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/EColi/data/obs_data.RData")
#Create interventional data
intv_data <- as.numeric(data[which(data$names == "dpiA"), c("fur__delfur_dpd__1", "fur__delfur_fe2__1", "fur__delfur_fe2__2")])
mean(intv_data)

#test
lm <- lm(dpiA ~ fur + arcA + crp + ihfA + ihfB + rpoD, obs_data)

dpiA_Given_fur <- coef(lm) ['(Intercept)'] + 
  coef(lm) ['arcA'] * obs_data$arcA +
  coef(lm) ['crp'] * obs_data$crp +
  coef(lm) ['ihfA'] * obs_data$ihfA +
  coef(lm) ['ihfB'] * obs_data$ihfB +
  coef(lm) ['rpoD'] * obs_data$rpoD
summary(dpiA_Given_fur)
var(dpiA_Given_fur)
