source("/Users/sarataheri/GitHub/OptimalAdjustmentSet/main_functions.R")
g_IGF <- dagitty( "dag {SOS -> Ras;
                        Ras -> PI3K;
                        PI3K -> AKT;
                        Ras -> Raf;
                        AKT -> Raf;
                        Raf -> Mek;
                        Mek -> Erk;
                        PI3K <-> SOS}" )
adjustmentSets(g_IGF, exposure = "AKT", outcome = "Erk", type = "all")

obs_data <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/data/observational_igf.RData")

# Get the results
IGF_lm_N1000_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                           exposure = "AKT",
                                                                           exposure_intv_value = 0,
                                                                           outcome = "Erk",
                                                                           query = "ATE",
                                                                           method = "lm",
                                                                           synthetic_data = obs_data,
                                                                           num_dp = 1000,
                                                                           num_synthetic_data_sets = 1000)
IGF_lm_N500_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                           exposure = "AKT",
                                                                           exposure_intv_value = 0,
                                                                           outcome = "Erk",
                                                                           query = "ATE",
                                                                           method = "lm",
                                                                           synthetic_data = obs_data,
                                                                           num_dp = 500,
                                                                           num_synthetic_data_sets = 1000)
IGF_lm_N100_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                          exposure = "AKT",
                                                                          exposure_intv_value = 0,
                                                                          outcome = "Erk",
                                                                          query = "ATE",
                                                                          method = "lm",
                                                                          synthetic_data = obs_data,
                                                                          num_dp = 100,
                                                                          num_synthetic_data_sets = 1000)
IGF_lm_N50_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                          exposure = "AKT",
                                                                          exposure_intv_value = 0,
                                                                          outcome = "Erk",
                                                                          query = "ATE",
                                                                          method = "lm",
                                                                          synthetic_data = obs_data,
                                                                          num_dp = 50,
                                                                          num_synthetic_data_sets = 1000)
IGF_lm_N20_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                          exposure = "AKT",
                                                                          exposure_intv_value = 0,
                                                                          outcome = "Erk",
                                                                          query = "ATE",
                                                                          method = "lm",
                                                                          synthetic_data = obs_data,
                                                                          num_dp = 20,
                                                                          num_synthetic_data_sets = 1000)
