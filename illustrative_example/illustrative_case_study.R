source("/Users/sarataheri/GitHub/OptimalAdjustmentSet/main_functions.R")

# g_simple <- dagitty( "dag { Z3 [latent]
#                             Z1 -> Z2
#                             Z3 -> Y
#                             Z3 -> Z2
#                             Z1 -> X
#                             X -> Y
#                             Z4 -> X
#                             Z5 -> X;
#                             Z5 -> Y}" )

g_simple <- dagitty( "dag { Z1 -> Z2;
                            Y <-> Z2;
                            Z1 -> X;
                            X -> Y;
                            Z4 -> X;
                            Z5 -> X;
                            Z5 -> Y}" )

# True data generation process
gen_data <- function(nds = 1000, #number of data sets
                     ndp = 1000, # number of data points
                     seed = 10
) {
  data = list()
  set.seed(seed)
  bXY = sample(seq(-1,1,0.01), size = 1)
  bZ4X = sample(seq(-1,1,0.01), size = 1)
  bZ1X = sample(seq(-1,1,0.01), size = 1)
  bZ3Y = sample(seq(-1,1,0.01), size = 1)
  bZ1Z2 = sample(seq(-1,1,0.01), size = 1) 
  bZ3Z2 = sample(seq(-1,1,0.01), size = 1)
  bZ5X = sample(seq(-1,1,0.01), size = 1)
  bZ5Y = sample(seq(-1,1,0.01), size = 1)
  print(bXY)
  for (i in 1:nds) {
    set.seed(i)
    Z1 <- rnorm(ndp)
    Z3 <- rnorm(ndp)
    Z4 <- rnorm(ndp)
    Z5 <- rnorm(ndp)
    Z2 <- rnorm(ndp, bZ1Z2*Z1 + bZ3Z2 * Z3)
    #X <- rnorm(ndp, bZ1X*Z1 + bZ4X * Z4)
    pa <- exp(bZ1X*Z1 + bZ4X * Z4 + bZ5X * Z5)/(1 + exp(bZ1X*Z1 + bZ4X * Z4 + bZ5X * Z5))
    X = rbinom(ndp, size = 1, prob = pa)
    Y <- rnorm(ndp, bXY*X + bZ3Y*Z3 + bZ5Y * Z5)
    data[[i]] = data.frame("Z1"=Z1, "Z2"=Z2, "Z3"=Z3, "Z4"=Z4, "Z5" = Z5  ,"Y"=Y, "X"=X)
  }
  return(data)
}

#obs_data_list = gen_data(nds = 1000, ndp = 1000, seed = 10)
all_adj_set = adjustmentSets(x = g_simple, exposure = "X", outcome = "Y", type = "all")
#read BN data
BN_1000 <- readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/BN_data_generation/data/illus_simdata_1000.RData")
BN_500 <- readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/BN_data_generation/data/illus_simdata_500.RData")
BN_100 <- readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/BN_data_generation/data/illus_simdata_100.RData")
BN_50 <- readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/BN_data_generation/data/illus_simdata_50.RData")
BN_30 <- readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/BN_data_generation/data/illus_simdata_30.RData")
#read real data
real_data <- readRDS("/Users/sarataheri/GitHub/OptimalAdjustmentSet/illustrative_example/real_data/obs_data_list.RData")

# Get the results

simple_case_lm_ATE_real_N50_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                        exposure = "X",
                                                                                        exposure_intv_value = 0,
                                                                                        outcome = "Y",
                                                                                        all_valid_adj = all_adj_set,
                                                                                        query = "ATE",
                                                                                        method = "lm",
                                                                                        synthetic_data = real_data,
                                                                                        num_dp = 50,
                                                                                        num_synthetic_data_sets = 1000)

simple_case_IPW_ATE_real_N50_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                         exposure = "X",
                                                                                         exposure_intv_value = 0,
                                                                                         outcome = "Y",
                                                                                         all_valid_adj = all_adj_set,
                                                                                         query = "ATE",
                                                                                         method = "IPW",
                                                                                         synthetic_data = real_data,
                                                                                         num_dp = 50,
                                                                                         num_synthetic_data_sets = 1000)

simple_case_AIPW_ATE_real_N50_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                           exposure = "X",
                                                                                           exposure_intv_value = 0,
                                                                                           outcome = "Y",
                                                                                           all_valid_adj = all_adj_set,
                                                                                           query = "ATE",
                                                                                           method = "AIPW",
                                                                                           synthetic_data = real_data,
                                                                                           num_dp = 50,
                                                                                           num_synthetic_data_sets = 1000)
#Save the results
saveRDS(simple_case_IPW_ATE_real_N50_K1000, "/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_IPW_ATE_real_N50_K1000.RData")
saveRDS(simple_case_lm_ATE_real_N50_K1000, "/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_lm_ATE_real_N50_K1000.RData")
saveRDS(simple_case_AIPW_ATE_real_N50_K1000, "/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_AIPW_ATE_real_N50_K1000.RData")

#Read the real data results
simple_case_IPW_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_IPW_ATE_real_N1000_K1000.RData")
simple_case_AIPW_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_AIPW_ATE_real_N1000_K1000.RData")
simple_case_lm_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_lm_ATE_real_N1000_K1000.RData")

simple_case_IPW_N500_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_IPW_ATE_real_N500_K1000.RData")
simple_case_AIPW_N500_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_AIPW_ATE_real_N500_K1000.RData")
simple_case_lm_N500_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_lm_ATE_real_N500_K1000.RData")

simple_case_IPW_N100_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_IPW_ATE_real_N100_K1000.RData")
simple_case_AIPW_N100_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_AIPW_ATE_real_N100_K1000.RData")
simple_case_lm_N100_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_lm_ATE_real_N100_K1000.RData")

simple_case_IPW_N50_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_IPW_ATE_real_N50_K1000.RData")
simple_case_AIPW_N50_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_AIPW_ATE_real_N50_K1000.RData")
simple_case_lm_N50_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_lm_ATE_real_N50_K1000.RData")

simple_case_IPW_N30_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_IPW_ATE_real_N30_K1000.RData")
simple_case_AIPW_N30_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_AIPW_ATE_real_N30_K1000.RData")
simple_case_lm_N30_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/real_data/result/simple_case_lm_ATE_real_N30_K1000.RData")

# Read the BN data results
simple_case_IPW_BN_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_IPW_ATE_BN_N1000_K1000.RData")
simple_case_AIPW_BN_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_AIPW_ATE_BN_N1000_K1000.RData")
simple_case_lm_BN_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_lm_ATE_BN_N1000_K1000.RData")

simple_case_IPW_BN_N500_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_IPW_ATE_BN_N500_K1000.RData")
simple_case_AIPW_BN_N500_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_AIPW_ATE_BN_N500_K1000.RData")
simple_case_lm_BN_N500_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_lm_ATE_BN_N500_K1000.RData")

simple_case_IPW_BN_N100_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_IPW_ATE_BN_N100_K1000.RData")
simple_case_AIPW_BN_N100_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_AIPW_ATE_BN_N100_K1000.RData")
simple_case_lm_BN_N100_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_lm_ATE_BN_N100_K1000.RData")

simple_case_IPW_BN_N50_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_IPW_ATE_BN_N50_K1000.RData")
simple_case_AIPW_BN_N50_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_AIPW_ATE_BN_N50_K1000.RData")
simple_case_lm_BN_N50_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_lm_ATE_BN_N50_K1000.RData")


simple_case_IPW_BN_N30_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_IPW_ATE_BN_N30_K1000.RData")
#simple_case_AIPW_BN_N30_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_AIPW_ATE_BN_N30_K1000.RData")
simple_case_lm_BN_N30_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/BN_data_generation/result/simple_case_lm_ATE_BN_N30_K1000.RData")

#Plot the results
K=1000 #number of synthetic data sets
df_N1000 <- data.frame("CE" = c(simple_case_IPW_BN_N1000_K1000[[2]][[1]],
                                simple_case_IPW_BN_N1000_K1000[[2]][[2]],
                                simple_case_IPW_BN_N1000_K1000[[2]][[3]],
                                simple_case_IPW_BN_N1000_K1000[[2]][[4]],
                                simple_case_IPW_BN_N1000_K1000[[2]][[5]],
                                simple_case_IPW_BN_N1000_K1000[[2]][[6]],
                                simple_case_AIPW_BN_N1000_K1000[[2]][[1]],
                                simple_case_AIPW_BN_N1000_K1000[[2]][[2]],
                                simple_case_AIPW_BN_N1000_K1000[[2]][[3]],
                                simple_case_AIPW_BN_N1000_K1000[[2]][[4]],
                                simple_case_AIPW_BN_N1000_K1000[[2]][[5]],
                                simple_case_AIPW_BN_N1000_K1000[[2]][[6]],
                                simple_case_lm_BN_N1000_K1000[[2]][[1]],
                                simple_case_lm_BN_N1000_K1000[[2]][[2]],
                                simple_case_lm_BN_N1000_K1000[[2]][[3]],
                                simple_case_lm_BN_N1000_K1000[[2]][[4]],
                                simple_case_lm_BN_N1000_K1000[[2]][[5]],
                                simple_case_lm_BN_N1000_K1000[[2]][[6]]
),
"N" = c(rep("A1",K),
        rep("A2",K),
        rep("A3",K),
        rep("A4",K),
        rep("A5",K),
        rep("A6",K),
        rep("A1",K),
        rep("A2",K),
        rep("A3",K),
        rep("A4",K),
        rep("A5",K),
        rep("A6",K),
        rep("A1",K),
        rep("A2",K),
        rep("A3",K),
        rep("A4",K),
        rep("A5",K),
        rep("A6",K)),
"model" = c(rep("IPW", 6*K),
            rep("AIPW", 6*K),
            rep("Linear regression", 6*K)
)
)
L=996 #number of synthetic data sets
df_N50 <- data.frame("CE" =   c(simple_case_IPW_BN_N50_K1000[[2]][[1]][1:996],
                                simple_case_IPW_BN_N50_K1000[[2]][[2]][1:996],
                                simple_case_IPW_BN_N50_K1000[[2]][[3]][1:996],
                                simple_case_IPW_BN_N50_K1000[[2]][[4]][1:996],
                                simple_case_IPW_BN_N50_K1000[[2]][[5]][1:996],
                                simple_case_IPW_BN_N50_K1000[[2]][[6]][1:996],
                                simple_case_AIPW_BN_N50_K1000[[2]][[1]][1:996],
                                simple_case_AIPW_BN_N50_K1000[[2]][[2]][1:996],
                                simple_case_AIPW_BN_N50_K1000[[2]][[3]][1:996],
                                simple_case_AIPW_BN_N50_K1000[[2]][[4]][1:996],
                                simple_case_AIPW_BN_N50_K1000[[2]][[5]][1:996],
                                simple_case_AIPW_BN_N50_K1000[[2]][[6]][1:996],
                                simple_case_lm_BN_N50_K1000[[2]][[1]][1:996],
                                simple_case_lm_BN_N50_K1000[[2]][[2]][1:996],
                                simple_case_lm_BN_N50_K1000[[2]][[3]][1:996],
                                simple_case_lm_BN_N50_K1000[[2]][[4]][1:996],
                                simple_case_lm_BN_N50_K1000[[2]][[5]][1:996],
                                simple_case_lm_BN_N50_K1000[[2]][[6]][1:996]
),
"N" = c(rep("A1",L),
        rep("A2",L),
        rep("A3",L),
        rep("A4",L),
        rep("A5",L),
        rep("A6",L),
        rep("A1",L),
        rep("A2",L),
        rep("A3",L),
        rep("A4",L),
        rep("A5",L),
        rep("A6",L),
        rep("A1",L),
        rep("A2",L),
        rep("A3",L),
        rep("A4",L),
        rep("A5",L),
        rep("A6",L)),
"model" = c(rep("IPW", 6*L),
            rep("AIPW", 6*L),
            rep("Linear regression", 6*L)
)
)


gg_N1000 <- df_N1000 %>% ggplot(aes(x = as.factor(N), y = CE, fill=model)) +
  geom_boxplot(width = 0.6) +
  geom_abline(intercept=0.36, slope=0) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 35,face="bold"),
        legend.key.size = unit(2, "cm"),
        axis.text.y=element_text(size=35),
        axis.title.y=element_text(size=35,face="bold"),
        axis.title.x = element_text(size=35,face="bold"),
        axis.text.x = element_text(#angle = 90, vjust = 0.5, hjust=1,
          size = 35, face="bold"),
        legend.position = "top",
        plot.title = element_text(size = 35, hjust = 0.5, face = "bold")
  ) +
  xlab("Adjustment sets") +
  ylim(0.2,0.5) +
  #ggtitle("IGF Signalling model") +
  ylab("ATE, N = 1000")
gg_N1000
ggsave("/Users/sarataheri/GitHub/OptimalAdjustmentSet/img/case1_N1000_boxplots.pdf", plot = gg_N1000, width = 12, height = 8, dpi = 300, units = "in")
gg_N50 <- df_N50 %>% ggplot(aes(x = as.factor(N), y = CE, fill=model)) +
  geom_boxplot(width = 0.6) +
  geom_abline(intercept=0.36, slope=0) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 35,face="bold"),
        legend.key.size = unit(2, "cm"),
        axis.text.y=element_text(size=35),
        axis.title.y=element_text(size=35,face="bold"),
        axis.title.x = element_text(size=35,face="bold"),
        axis.text.x = element_text(#angle = 90, vjust = 0.5, hjust=1,
          size = 35, face="bold"),
        legend.position = "top",
        plot.title = element_text(size = 35, hjust = 0.5, face = "bold")
  ) +
  xlab("Adjustment sets") +
  ylim(0.2,0.5) +
  #ggtitle("IGF Signalling model") +
  ylab("ATE, N = 50")
gg_N50
ggsave("/Users/sarataheri/GitHub/OptimalAdjustmentSet/img/case1_N50_boxplots.pdf", plot = gg_N50, width = 12, height = 8, dpi = 300, units = "in")
