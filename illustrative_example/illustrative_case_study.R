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

obs_data_list = gen_data(nds = 1000, ndp = 1000, seed = 10)
#GAN_data = read.csv("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/GAN_illustrative.csv")

# Get the results
simple_case_IPW_N500_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                         exposure = "X",
                                                                                         exposure_intv_value = 0,
                                                                                         outcome = "Y",
                                                                                         query = "ATE",
                                                                                         method = "IPW",
                                                                                         synthetic_data = obs_data_list,
                                                                                         num_dp = 500,
                                                                                         num_synthetic_data_sets = 1000)

simple_case_AIPW_N500_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                    exposure = "X",
                                                                                    exposure_intv_value = 0,
                                                                                    outcome = "Y",
                                                                                    query = "ATE",
                                                                                    method = "AIPW",
                                                                                    synthetic_data = obs_data_list,
                                                                                    num_dp = 500,
                                                                                    num_synthetic_data_sets = 1000)

simple_case_lm_N500_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                    exposure = "X",
                                                                                    exposure_intv_value = 0,
                                                                                    outcome = "Y",
                                                                                    query = "ATE",
                                                                                    method = "lm",
                                                                                    synthetic_data = obs_data_list,
                                                                                    num_dp = 500,
                                                                                    num_synthetic_data_sets = 1000)
saveRDS(simple_case_IPW_N500_K1000, "/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/simple_case_IPW_N1000_K1000.RData")

simple_case_IPW_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/simple_case_IPW_N1000_K1000.RData")
simple_case_AIPW_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/simple_case_AIPW_N1000_K1000.RData")
simple_case_lm_N1000_K1000 <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/simple_case_lm_N1000_K1000.RData")

