source("/Users/sarataheri/GitHub/OptimalAdjustmentSet/exhaustive_backdoor_search.R")
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

covid_data = readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/data/obs_data_list.RData")

#covid_all_adj <- adjustmentSets(g_covid, exposure = "EGFR", outcome = "cytok", type = "all")
#saveRDS(covid_all_adj, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/data/covid_all_adj.RData")
covid_all_adj <- readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/data/covid_all_adj.RData")
# Get the results
covid_ATE_lm_N50_K100 = find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_covid,
                                                                    exposure = "EGFR",
                                                                    exposure_intv_value = 0,
                                                                    outcome = "cytok",
                                                                    query = "ATE",
                                                                    method = "lm",
                                                                    synthetic_data = covid_data,
                                                                    num_dp = 50,
                                                                    num_synthetic_data_sets = 100)

covid_ATE_lm_N50_K1000 = find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_covid,
                                                                             exposure = "EGFR",
                                                                             exposure_intv_value = 0,
                                                                             outcome = "cytok",
                                                                             query = "ATE",
                                                                             method = "lm",
                                                                             synthetic_data = covid_data,
                                                                             num_dp = 50,
                                                                             num_synthetic_data_sets = 1000)

covid_ATE_lm_N50_K10000 = find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_covid,
                                                                             exposure = "EGFR",
                                                                             exposure_intv_value = 0,
                                                                             outcome = "cytok",
                                                                             query = "ATE",
                                                                             method = "lm",
                                                                             synthetic_data = covid_data,
                                                                             num_dp = 50,
                                                                             num_synthetic_data_sets = 10000)

covid_ATE_lm_N500_K100 = find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_covid,
                                                                             exposure = "EGFR",
                                                                             exposure_intv_value = 0,
                                                                             outcome = "cytok",
                                                                             query = "ATE",
                                                                             method = "lm",
                                                                             synthetic_data = covid_data,
                                                                             num_dp = 500,
                                                                             num_synthetic_data_sets = 100)

covid_ATE_lm_N500_K1000 = find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_covid,
                                                                              exposure = "EGFR",
                                                                              exposure_intv_value = 0,
                                                                              outcome = "cytok",
                                                                              query = "ATE",
                                                                              method = "lm",
                                                                              synthetic_data = covid_data,
                                                                              num_dp = 500,
                                                                              num_synthetic_data_sets = 1000)

covid_ATE_lm_N500_K10000 = find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_covid,
                                                                               exposure = "EGFR",
                                                                               exposure_intv_value = 0,
                                                                               outcome = "cytok",
                                                                               query = "ATE",
                                                                               method = "lm",
                                                                               synthetic_data = covid_data,
                                                                               num_dp = 500,
                                                                               num_synthetic_data_sets = 10000)

covid_ATE_lm_N1000_K100 = find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_covid,
                                                                              exposure = "EGFR",
                                                                              exposure_intv_value = 0,
                                                                              outcome = "cytok",
                                                                              query = "ATE",
                                                                              method = "lm",
                                                                              synthetic_data = covid_data,
                                                                              num_dp = 1000,
                                                                              num_synthetic_data_sets = 100)

covid_ATE_lm_N1000_K1000 = find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_covid,
                                                                               exposure = "EGFR",
                                                                               exposure_intv_value = 0,
                                                                               outcome = "cytok",
                                                                               query = "ATE",
                                                                               method = "lm",
                                                                               synthetic_data = covid_data,
                                                                               num_dp = 1000,
                                                                               num_synthetic_data_sets = 1000)

covid_ATE_lm_N1000_K10000 = find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_covid,
                                                                                exposure = "EGFR",
                                                                                exposure_intv_value = 0,
                                                                                outcome = "cytok",
                                                                                query = "ATE",
                                                                                method = "lm",
                                                                                synthetic_data = covid_data,
                                                                                num_dp = 1000,
                                                                                num_synthetic_data_sets = 10000)

#save the results
saveRDS(covid_ATE_lm_N50_K100, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N50_K100.RData")
saveRDS(covid_ATE_lm_N50_K1000, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N50_K1000.RData")
saveRDS(covid_ATE_lm_N50_K10000, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N50_K10000.RData")
saveRDS(covid_ATE_lm_N500_K100, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N500_K100.RData")
saveRDS(covid_ATE_lm_N500_K1000, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N500_K1000.RData")
saveRDS(covid_ATE_lm_N500_K10000, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N500_K10000.RData")
saveRDS(covid_ATE_lm_N1000_K100, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N1000_K100.RData")
saveRDS(covid_ATE_lm_N1000_K1000, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N1000_K1000.RData")
saveRDS(covid_ATE_lm_N1000_K10000, "/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N1000_K10000.RData")

#read the results
covid_ATE_lm_N50_K100 =  readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N50_K100")
covid_ATE_lm_N50_K1000 = readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N50_K1000")
covid_ATE_lm_N50_K10000 = readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N50_K10000")
covid_ATE_lm_N500_K100 = readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N500_K100")
covid_ATE_lm_N500_K1000 = readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N500_K1000")
covid_ATE_lm_N500_K10000 = readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N500_K10000")
covid_ATE_lm_N1000_K100 = readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N1000_K100")
covid_ATE_lm_N1000_K1000 = readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N1000_K1000")
covid_ATE_lm_N1000_K10000 = readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/Covid_case_study/results/covid_ATE_lm_N1000_K10000")