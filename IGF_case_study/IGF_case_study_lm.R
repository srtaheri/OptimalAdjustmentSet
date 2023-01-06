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
BNdata1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/data/IGF_simdata_1000.RData")
BNdata500 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/data/IGF_simdata_500.RData")
BNdata100 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/data/IGF_simdata_100.RData")
BNdata50 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/data/IGF_simdata_50.RData")
BNdata30 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/data/IGF_simdata_30.RData")

#read the GAN data
GANdata1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/GAN_data/GAN_data_IGF_N1000.RData")
GANdata500 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/GAN_data/GAN_data_IGF_N500.RData")
GANdata100 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/GAN_data/GAN_data_IGF_N100.RData")
GANdata30 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/GAN_data/GAN_data_IGF_N30.RData")
#generate GANdata30 again with more data points
#get the results for BN 30

# Get the results for N=1000, 500, 100, 30
IGF_lm_exp <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_IGF,
                                                                   exposure = "AKT",
                                                                   exposure_intv_value = 80,
                                                                   outcome = "Erk",
                                                                   all_valid_adj = all_valid_adj,
                                                                   query = "expectation",
                                                                   method = "lm",
                                                                   synthetic_data = BNdata50,
                                                                   num_dp = 50,
                                                                   num_synthetic_data_sets = 1000)
# save the results
saveRDS(IGF_lm_exp, "/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/result/IGF_lm_exp_BN_N50_K1000.RData")

# read the real data results
IGF_lm_exp_real_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/real_data/result_lm/IGF_lm_exp_N1000_K1000.RData")
IGF_lm_exp_real_N500_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/real_data/result_lm/IGF_lm_exp_N500_K1000.RData")
IGF_lm_exp_real_N100_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/real_data/result_lm/IGF_lm_exp_N100_K1000.RData")
IGF_lm_exp_real_N30_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/real_data/result_lm/IGF_lm_exp_N30_K1000.RData")
# read the BN results
IGF_lm_exp_BN_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/result/IGF_lm_exp_BN_N1000_K1000.RData")
IGF_lm_exp_BN_N500_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/result/IGF_lm_exp_BN_N500_K1000.RData")
IGF_lm_exp_BN_N100_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/result/IGF_lm_exp_BN_N100_K1000.RData")
IGF_lm_exp_BN_N30_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/BN_simdata/result/IGF_lm_exp_BN_N30_K1000.RData")
# read GAN results
IGF_lm_exp_GAN_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/result/IGF_lm_exp_GAN_N1000_K1000.RData")
IGF_lm_exp_GAN_N500_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/result/IGF_lm_exp_GAN_N500_K1000.RData")
IGF_lm_exp_GAN_N100_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/result/IGF_lm_exp_GAN_N100_K1000.RData")
#IGF_lm_exp_GAN_N30_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/result/IGF_lm_exp_GAN_N30_K1000.RData")


GAN_data_list <- list()
for (i in 1:1000) {
  GAN_data <- read.csv(paste("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/GAN_data/GAN_data_N30/GAN_IGF_",i,".csv", sep=""))
  GAN_data <- GAN_data[,-1]
  GAN_data_list[[i]] <- GAN_data
}
saveRDS(GAN_data_list, "/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/GAN_data/GAN_data_list_IGF_N30_for_training.RData")

a <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/GAN_simdata/GAN_data/GAN_data_list_IGF_N100_for_training.RData")

library(UsingR)
par(mfrow=c(1,2)) 
hist(obs_data[[1]][1:30,]$Erk)
hist(BNdata30[[1]]$Erk)
mean(obs_data[[1]]$Erk)
mean(BNdata30[[1]]$Erk)

library(infotheo)
condinformation(X = obs_data[[1]]$Ras, Y = obs_data[[1]]$Erk, S = obs_data[[1]]$AKT ) - condinformation(X = obs_data[[1]]$AKT, Y = obs_data[[1]]$Ras, NULL )
condinformation(X = data.frame("Ras" = obs_data[[1]]$Ras, "SOS" = obs_data[[1]]$SOS), Y = obs_data[[1]]$Erk, S = obs_data[[1]]$AKT ) - condinformation(X = obs_data[[1]]$AKT, Y = data.frame("Ras" = obs_data[[1]]$Ras, "SOS" = obs_data[[1]]$SOS), NULL )
