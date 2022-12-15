source("/Users/sarataheri/GitHub/OptimalAdjustmentSet/exhaustive_backdoor_search.R")
library(ggplot2)

# g_simple <- dagitty( "dag { Z3 [latent]
#                             Z1 -> Z2
#                             Z3 -> Y
#                             Z3 -> Z2
#                             Z1 -> X
#                             X -> Y
#                             Z4 -> X}" )

g_simple <- dagitty( "dag { Z1 -> Z2;
                            Y <-> Z2;
                            Z1 -> X;
                            X -> Y;
                            Z4 -> X}" )

# True data generation process
gen_data <- function(nds = 10000, #number of data sets
                     ndp = 10000, # number of data points
                     bXY = 1,
                     bZ4X = 1, 
                     bZ1X = 1, 
                     bZ3Y = 1, 
                     bZ1Z2 = 1, 
                     bZ3Z2 = 1 
) {
  data = list()
  for (i in 1:nds) {
    #set.seed(i)
    Z1 <- rnorm(ndp)
    Z3 <- rnorm(ndp)
    Z4 <- rnorm(ndp)
    Z2 <- rnorm(ndp, bZ1Z2*Z1 + bZ3Z2 * Z3)
    X <- rnorm(ndp, bZ1X*Z1 + bZ4X * Z4)
    Y <- rnorm(ndp, bXY*X + bZ3Y*Z3)
    data[[i]] = data.frame("Z1"=Z1, "Z2"=Z2, "Z3"=Z3, "Z4"=Z4, "Y"=Y, "X"=X)
  }
  return(data)
}

obs_data_list = gen_data(nds = 1000, ndp = 1000)
GAN_data = read.csv("/Users/sarataheri/Github/OptimalAdjustmentSet/illustrative_example/GAN_illustrative.csv")

# Get the results
simple_case_lm_N1000_K1000 <- find_ranked_var_and_query_est_for_all_valid_adj_sets(g = g_simple,
                                                                                exposure = "X",
                                                                                exposure_intv_value = 0,
                                                                                outcome = "Y",
                                                                                query = "ATE",
                                                                                method = "lm",
                                                                                synthetic_data = obs_data_list,
                                                                                num_dp = 1000,
                                                                                num_synthetic_data_sets = 1000)
