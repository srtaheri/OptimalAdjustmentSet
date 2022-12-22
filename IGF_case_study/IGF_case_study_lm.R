source("/Users/sarataheri/GitHub/OptimalAdjustmentSet/main_functions.R")
g_IGF <- dagitty( "dag {SOS -> Ras;
                        Ras -> PI3K;
                        PI3K -> AKT;
                        Ras -> Raf;
                        AKT -> Raf;
                        Raf -> Mek;
                        Mek -> Erk;
                        PI3K <-> SOS}" )
all_valid_adj = adjustmentSets(g_IGF, exposure = "AKT", outcome = "Erk", type = "all")

obs_data <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/real_data/observational_igf.RData")
intv_data <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/real_data/interventional_igf_AKT80.RData")

# real value of the query: Q = E[ Erk | do(AKT = 80)]
mean(intv_data[[1]]$Erk)


#read the BN data
BNdata1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/IGF_simdata_1000.RData")
BNdata500 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/IGF_simdata_500.RData")
BNdata100 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/IGF_simdata_100.RData")
BNdata20 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/IGF_simdata_20.RData")

# Get the results for N=1000, 500, 100, 20
IGF_lm_ATE_BN_N20_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                           exposure = "AKT",
                                                                           exposure_intv_value = 80,
                                                                           outcome = "Erk",
                                                                           all_valid_adj = all_valid_adj,
                                                                           query = "ATE",
                                                                           method = "lm",
                                                                           synthetic_data = obs_data,
                                                                           num_dp = 20,
                                                                           num_synthetic_data_sets = 1000)

# save the results
saveRDS(IGF_lm_exp_BN_N20_K1000, "/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/result/IGF_lm_exp_BN_N20_K1000.RData")

# read the BN results
IGF_lm_exp_BN_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/result/IGF_lm_exp_BN_N1000_K1000.RData")
IGF_lm_exp_BN_N500_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/result/IGF_lm_exp_BN_N500_K1000.RData")
IGF_lm_exp_BN_N100_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/result/IGF_lm_exp_BN_N100_K1000.RData")
IGF_lm_exp_BN_N20_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/result/IGF_lm_exp_BN_N20_K1000.RData")
# real the real data results
IGF_lm_exp_real_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/real_data/result/IGF_lm_exp_N1000_K1000.RData")
IGF_lm_exp_real_N500_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/real_data/result/IGF_lm_exp_N500_K1000.RData")
IGF_lm_exp_real_N100_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/real_data/result/IGF_lm_exp_N100_K1000.RData")
IGF_lm_exp_real_N20_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/real_data/result/IGF_lm_exp_N20_K1000.RData")



GAN_data_list <- list()
for (i in 1:1000) {
  GAN_data <- read.csv(paste("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/GAN_data/GAN_IGF_",i,".csv", sep=""))
  GAN_data <- GAN_data[,-1]
  GAN_data_list[[i]] <- GAN_data
}
saveRDS(GAN_data_list, "/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/GAN_data/GAN_data_list_IGF.RData")

a <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/GAN_data/GAN_data_list_IGF.RData")
