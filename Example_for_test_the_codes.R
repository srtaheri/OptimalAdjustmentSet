source("/Users/sarataheri/GitHub/OptimalAdjustmentSet/exhaustive_backdoor_search.R")
# Simple case without latent variables
g_simple <- dagitty( "dag { Z1 -> Z2;
                            Z2 -> Z3;
                            Z3 -> Y;
                            Z1 -> X;
                            X -> M1;
                            M1 -> M2;
                            M2 -> Y}" )

gen_data <- function(nds = 10000, #number of data sets
                     ndp = 10000, # number of data points
                     bXM1 = 1,
                     bM1M2 = 1,
                     bM2Y = 1, 
                     bZ1X = 1, 
                     bZ3Y = 1, 
                     bZ1Z2 = 1, 
                     bZ2Z3 = 1 
                     ) {
  data = list()
  for (i in 1:nds) {
    Z1 <- rnorm(ndp)
    Z2 <- rnorm(ndp, bZ1Z2*Z1)
    Z3 <- rnorm(ndp, bZ2Z3*Z2)
    X <- rnorm(ndp, bZ1X*Z1)
    M1 <- rnorm(ndp, bXM1*X)
    M2 <- rnorm(ndp, bM1M2*M1)
    Y <- rnorm(ndp, bM2Y*M2 + bZ3Y*Z3)
    data[[i]] = data.frame("Z1"=Z1, "Z2"=Z2, "Z3"=Z3, "Y"=Y, "X"=X, "M1"=M1, "M2"=M2)
  }
  return(data)
}

simple_case_lm_N20_K100 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                 exposure = "X",
                                                                                 exposure_intv_value = 0,
                                                                                 outcome = "Y",
                                                                                 query = "ATE",
                                                                                 method = "lm",
                                                                                 synthetic_data = gen_data(),
                                                                                 num_dp = 20,
                                                                                 num_synthetic_data_sets = 100)
simple_case_lm_N20_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                exposure = "X",
                                                                                exposure_intv_value = 0,
                                                                                outcome = "Y",
                                                                                query = "ATE",
                                                                                method = "lm",
                                                                                synthetic_data = gen_data(),
                                                                                num_dp = 20,
                                                                                num_synthetic_data_sets = 1000)
simple_case_lm_N20_K10000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                exposure = "X",
                                                                                exposure_intv_value = 0,
                                                                                outcome = "Y",
                                                                                query = "ATE",
                                                                                method = "lm",
                                                                                synthetic_data = gen_data(),
                                                                                num_dp = 20,
                                                                                num_synthetic_data_sets = 10000)

simple_case_lm_N100_K100 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                 exposure = "X",
                                                                                 exposure_intv_value = 0,
                                                                                 outcome = "Y",
                                                                                 query = "ATE",
                                                                                 method = "lm",
                                                                                 synthetic_data = gen_data(),
                                                                                 num_dp = 100,
                                                                                 num_synthetic_data_sets = 100)
simple_case_lm_N100_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                 exposure = "X",
                                                                                 exposure_intv_value = 0,
                                                                                 outcome = "Y",
                                                                                 query = "ATE",
                                                                                 method = "lm",
                                                                                 synthetic_data = gen_data(),
                                                                                 num_dp = 1000,
                                                                                 num_synthetic_data_sets = 100)
simple_case_lm_N100_K10000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                 exposure = "X",
                                                                                 exposure_intv_value = 0,
                                                                                 outcome = "Y",
                                                                                 query = "ATE",
                                                                                 method = "lm",
                                                                                 synthetic_data = gen_data(),
                                                                                 num_dp = 100,
                                                                                 num_synthetic_data_sets = 10000)

simple_case_lm_N1000_K100 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                   exposure = "X",
                                                                                   exposure_intv_value = 0,
                                                                                   outcome = "Y",
                                                                                   query = "ATE",
                                                                                   method = "lm",
                                                                                   synthetic_data = gen_data(),
                                                                                   num_dp = 1000,
                                                                                   num_synthetic_data_sets = 100)
simple_case_lm_N1000_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                  exposure = "X",
                                                                                  exposure_intv_value = 0,
                                                                                  outcome = "Y",
                                                                                  query = "ATE",
                                                                                  method = "lm",
                                                                                  synthetic_data = gen_data(),
                                                                                  num_dp = 1000,
                                                                                  num_synthetic_data_sets = 1000)
simple_case_lm_N1000_K10000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                   exposure = "X",
                                                                                   exposure_intv_value = 0,
                                                                                   outcome = "Y",
                                                                                   query = "ATE",
                                                                                   method = "lm",
                                                                                   synthetic_data = gen_data(),
                                                                                   num_dp = 1000,
                                                                                   num_synthetic_data_sets = 10000)
##################################
# test for the motivating example
g <- dagitty( "dag { Z1 -> Z2;
                     Z2 -> Z3;
                     Z3 -> Z4;
                     Z4 -> Z5;
                     Z5 -> Y;
                     Z1 -> X;
                     X -> M1;
                     M1 -> M2;
                     M2 -> M3;
                     M3 -> Y;
                     Z2 -> M1;
                     Z1 <-> X;
                     Z2 <-> M1}" )


gen_data <- function(nds = 1000, #number of data sets
                     ndp = 1000, # number of data points
                     bXZ6 = 1,
                     bYZ6 = 1,
                     bM3Z7 = 1,
                     bXM1 = 1,
                     bM1M2 = 1,
                     bM2M3 = 1,
                     bM3Y = 1, 
                     bZ1X = 1, 
                     bZ5Y = 1, 
                     bZ1Z2 = 1, 
                     bZ2Z3 = 1, 
                     bZ3Z4 = 1,
                     bZ4Z5 = 1,
                     bZ2M1 = 1,
                     bU1Z1 = 1, 
                     bU1X = 1, 
                     bU2Z2 = 1,
                     bU2M1 = 1) {
  data = list()
  for (i in 1:nds) {
    U1 <- rnorm(ndp)
    U2 <- rnorm(ndp)
    Z1 <- rnorm(ndp, bU1Z1*U1)
    Z2 <- rnorm(ndp, bU2Z2*U2 + bZ1Z2*Z1)
    Z3 <- rnorm(ndp, bZ2Z3*Z2)
    Z4 <- rnorm(ndp, bZ3Z4*Z3)
    Z5 <- rnorm(ndp, bZ4Z5*Z4)
    X <- rnorm(ndp, bU1X*U1 + bZ1X*Z1)
    M1 <- rnorm(ndp, bU2M1*U2 + bXM1*X + bZ2M1*Z2)
    M2 <- rnorm(ndp, bM1M2*M1)
    M3 <- rnorm(ndp, bM2M3*M2)
    Y <- rnorm(ndp, bM3Y*M3 + bZ5Y*Z5)
    data[[i]] = data.frame("Z1"=Z1, "Z2"=Z2, "Z3"=Z3, "Z4"=Z4, "Z5"=Z5, "Y"=Y, "X"=X, "M1"=M1, "M2"=M2, "M3"=M3)
  }
  return(data)
}

find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g, exposure = "X",
                                                     exposure_intv_value = 0,
                                                     outcome = "Y",
                                                     query = "ATE",
                                                     method = "lm",
                                                     synthetic_data = gen_data(),
                                                     num_dp = 100,
                                                     num_synthetic_data_sets = 100)
##################################################################################
##################################################################################
##################################################################################
# test for the covid case study
g_covid <- dagitty( "dag {
                    SARS_COV2 -> ACE2;
                    ACE2 -> Ang;
                    Ang -> AGTR1;
                    AGTR1 -> ADAM17;
                    ADAM17 -> EGF;
                    ADAM17 -> TNF;
                    ADAM17 -> Sil6r;
                    SARS_COV2 -> PRR;
                    PRR -> NFKB;
                    EGFR -> NFKB;
                    TNF -> NFKB;
                    Sil6r -> IL6STAT3;
                    Toci -> Sil6r;
                    NFKB -> IL6AMP;
                    IL6AMP -> cytok;
                    IL6STAT3 -> IL6AMP;
                    EGF -> EGFR;
                    Gefi -> EGFR;
                    SARS_COV2 <-> Ang;
                    EGF <-> EGFR;
                    TNF <-> EGFR;
                    IL6STAT3 <-> EGFR;
                    ADAM17 <-> Sil6r;
                    PRR <-> NFKB}" )

#all_valid_adjustment_sets = adjustmentSets( x = g_covid, exposure = "EGFR", outcome = "cytok" , type = "all")

covid_data = readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/data/obs_data_list.RData")

g = g_covid
exposure = "EGFR"
exposure_intv_value = 0
outcome = "cytok"
query = "ATE"
method = "IPW"
synthetic_data = covid_data

start_time <- Sys.time()
covid_ATE_lm = find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_covid,
                                                                    exposure = "EGFR",
                                                                    exposure_intv_value = 0,
                                                                    outcome = "cytok",
                                                                    query = "ATE",
                                                                    method = "IPW",
                                                                    synthetic_data = covid_data)
end_time <- Sys.time()
end_time - start_time

###############################################################################################
###############################################################################################
# test for the IGF case study
g_IGF <- dagitty( "dag {
                    SOS -> Ras;
                    Ras -> PI3K;
                    Ras -> Raf;
                    PI3K -> AKT;
                    AKT -> Raf;
                    Raf -> Mek;
                    Mek -> Erk;
                    SOS <-> PI3K}" )

all_valid_adjustment_sets = adjustmentSets( x = g_IGF, exposure = "AKT", outcome = "Erk" , type = "all")

IGF_data = readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/IGF_case_study/data/observational_igf.RData")

IGF_exp_lm_N20_K20 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                   exposure = "AKT",
                                                                   exposure_intv_value = 80,
                                                                   outcome = "Erk",
                                                                   query = "expectation",
                                                                   method = "lm",
                                                                   synthetic_data = IGF_data,
                                                                   num_dp = 20,
                                                                   num_synthetic_data_sets = 20)

IGF_exp_lm_N100_K20 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                      exposure = "AKT",
                                                                      exposure_intv_value = 80,
                                                                      outcome = "Erk",
                                                                      query = "expectation",
                                                                      method = "lm",
                                                                      synthetic_data = IGF_data,
                                                                      num_dp = 100,
                                                                      num_synthetic_data_sets = 20)

IGF_exp_lm_N1000_K20 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                       exposure = "AKT",
                                                                       exposure_intv_value = 80,
                                                                       outcome = "Erk",
                                                                       query = "expectation",
                                                                       method = "lm",
                                                                       synthetic_data = IGF_data,
                                                                       num_dp = 1000,
                                                                       num_synthetic_data_sets = 20)

IGF_exp_lm_N20_K100 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                               exposure = "AKT",
                                                                               exposure_intv_value = 80,
                                                                               outcome = "Erk",
                                                                               query = "expectation",
                                                                               method = "lm",
                                                                               synthetic_data = IGF_data,
                                                                               num_dp = 20,
                                                                               num_synthetic_data_sets = 100)

IGF_exp_lm_N100_K100 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                                exposure = "AKT",
                                                                                exposure_intv_value = 80,
                                                                                outcome = "Erk",
                                                                                query = "expectation",
                                                                                method = "lm",
                                                                                synthetic_data = IGF_data,
                                                                                num_dp = 100,
                                                                                num_synthetic_data_sets = 100)

IGF_exp_lm_N1000_K100 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                                 exposure = "AKT",
                                                                                 exposure_intv_value = 80,
                                                                                 outcome = "Erk",
                                                                                 query = "expectation",
                                                                                 method = "lm",
                                                                                 synthetic_data = IGF_data,
                                                                                 num_dp = 1000,
                                                                                 num_synthetic_data_sets = 100)


