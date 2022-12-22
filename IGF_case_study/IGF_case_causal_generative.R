library(rstan)
## IGF case study : the model where {PI3K, Ras, SOS} is the adjustment set

model_str_IGF_6 <- "
    data {
        int D;
        vector[D] SOS_train;
        vector[D] Ras_train;
        vector[D] PI3K_train;
        vector[D] AKT_train;
        vector[D] Erk_train;
    }
  parameters {
       real<lower=0> mu_U; 
       real<lower=0> sigma_U;
       real<lower=-2, upper=2> beta0_SOS; 
       real<lower=-2, upper=2> beta_UToSOS;
       real<lower=-2, upper=2> beta0_Ras;
       real<lower=-2, upper=2> beta_SOSToRas;
       real<lower=-2, upper=2> beta0_PI3K;
       real<lower=-2, upper=2> beta_UToPI3K;
       real<lower=-2, upper=2> beta_RasToPI3K;
       real<lower=-2, upper=2> beta0_Erk;
       real<lower=-2, upper=0> beta_AKTToErk;
       real<lower=-2, upper=2> beta_RasToErk;
       vector[D] U_train;
    }
    transformed parameters {
      vector[D] SOS_loc;
      vector[D] Ras_loc;
      vector[D] PI3K_loc;
      vector[D] Erk_loc;
      for (i in 1:D){
        SOS_loc[i] = 100  / (1 + exp(-beta0_SOS  - U_train[i]   * beta_UToSOS));
        Ras_loc[i] = 100  / (1 + exp(-beta0_Ras  - SOS_train[i] * beta_SOSToRas));
        PI3K_loc[i] = 100 / (1 + exp(-beta0_PI3K - Ras_train[i] * beta_RasToPI3K - U_train[i] * beta_UToPI3K));
        Erk_loc[i] = 100  / (1 + exp(-beta0_Erk  - AKT_train[i] * beta_AKTToErk - Ras_train[i] * beta_RasToErk));
      }
    }
    model {
        mu_U ~ normal(40, 10);
        sigma_U ~ normal(10,1);
        beta0_SOS  ~ normal(0, 10); 
        beta_UToSOS  ~ normal(0, 10);
        beta0_Ras ~ normal(0,10);
        beta_SOSToRas ~ normal(0, 10);
        beta0_PI3K ~ normal(0, 10);
        beta_UToPI3K ~ normal(0, 10);
        beta_RasToPI3K ~ normal(0, 10);
        beta0_Erk ~ normal(0,10);
        beta_AKTToErk ~ normal(0,10);
        beta_RasToErk ~ normal(0,10);
        U_train ~ normal(mu_U, sigma_U);
        SOS_train ~ normal(SOS_loc, 5.4);      // likelihood
        Ras_train ~ normal(Ras_loc, 5.3);
        PI3K_train ~ normal(PI3K_loc, 5);
        Erk_train ~ normal(Erk_loc,3.2);
    }
"

mod_IGF_6 <- rstan::stan_model(model_code = model_str_IGF_6)

#number of data sets
K = 5
#number of data points
N = 100
#read the data
observational_data_list <- readRDS("/Users/sarataheri/Github/OptimalAdjustmentSet/IGF_case_study/real_data/observational_igf.RData")

fit_list <- rep(list(list()), K)

start_time <- Sys.time()
for (num_data_points in N) {
  for (data_count in 1:K) {
    print(data_count)
    data_list <- list(D=num_data_points,
                      SOS_train = observational_data_list[[data_count]]$SOS[1:num_data_points],
                      Ras_train = observational_data_list[[data_count]]$Ras[1:num_data_points],
                      PI3K_train = observational_data_list[[data_count]]$PI3K[1:num_data_points],
                      AKT_train = observational_data_list[[data_count]]$AKT[1:num_data_points],
                      Erk_train = observational_data_list[[data_count]]$Erk[1:num_data_points]
    )
    
    # fit_list[[data_count]][[paste0("num_data_points_",num_data_points)]] <- rstan::vb(mod_IGF_6,
    #                                                                                   data=data_list,
    #                                                                                   seed = 1,
    #                                                                                   tol_rel_obj = 0.005)
    fit_list[[data_count]][[paste0("num_data_points_",num_data_points)]] <- rstan::sampling(mod_IGF_6,
                                                                                                data=data_list,
                                                                                                chains = 2,
                                                                                                iter = 3000,
                                                                                                warmup = 1500,
                                                                                                seed = 1#,
                                                                                                #control = list(max_treedepth = 15)
                                                                                            )
  }
  
}
end_time <- Sys.time()
end_time - start_time

#saveRDS(fit_list,file = "/Users/sarataheri/Desktop/Simplified_LVM/IGF_case_study/data/IGF_hmc_fit/fit_list_IGF_full_N100.RData")


mutilated_model_IGF_6 <- function(mu_U, sigma_U,
                                  beta0_SOS, beta_UToSOS,
                                  beta0_Ras, beta_SOSToRas,
                                  beta0_PI3K, beta_UToPI3K, beta_RasToPI3K,
                                  beta0_Erk, beta_AKTToErk, beta_RasToErk,
                                  akt, num_gen_samples, seed
) {
  set.seed(seed)
  U = rnorm(num_gen_samples, mu_U, sigma_U)
  SOS = 100 / (1 + exp(-beta0_SOS - U * beta_UToSOS)) + rnorm(num_gen_samples,0,10);
  Ras = 100 / (1 + exp(-beta0_Ras - SOS * beta_SOSToRas)) + rnorm(num_gen_samples,0,10)
  PI3K = 100 / (1 + exp(-beta0_PI3K - U * beta_UToPI3K - Ras * beta_RasToPI3K)) + rnorm(num_gen_samples,0,10)
  AKT = rep(akt, num_gen_samples)
  Erk = 100 / (1 + exp(-beta0_Erk - AKT * beta_AKTToErk - Ras * beta_RasToErk)) + rnorm(n = num_gen_samples,0,10)
  return(mean(Erk))
}

parameters <- c("mu_U", "sigma_U",
                "beta0_SOS","beta_UToSOS",
                "beta0_Ras","beta_SOSToRas",
                "beta0_PI3K", "beta_UToPI3K", "beta_RasToPI3K",
                "beta0_Erk", "beta_AKTToErk", "beta_RasToErk")



# fit_list <- readRDS("/Users/sarataheri/Desktop/Simplified_LVM/IGF_case_study/data/IGF_hmc_fit/fit_list_IGF_full_N100.RData")

CE_list <- list()
means_hmc <- c()
J80 <- c()
start_time <- Sys.time()

for (num_data_points in N) {
  for (data_count in 1:K) {
    samples_hmc <- rstan::extract(fit_list[[data_count]][[paste0("num_data_points_",num_data_points)]], parameters)
    for (j in 1:length(samples_hmc$mu_U)) {
      J80 <-  c(J80,mutilated_model_IGF_6(mu_U = (samples_hmc$mu_U)[j],
                                          sigma_U = (samples_hmc$sigma_U)[j],
                                          beta0_SOS = (samples_hmc$beta0_SOS)[j],
                                          beta_UToSOS = (samples_hmc$beta_UToSOS)[j],
                                          beta0_Ras = (samples_hmc$beta0_Ras)[j],
                                          beta_SOSToRas = (samples_hmc$beta_SOSToRas)[j],
                                          beta0_PI3K = (samples_hmc$beta0_PI3K)[j],
                                          beta_UToPI3K = (samples_hmc$beta_UToPI3K)[j],
                                          beta_RasToPI3K = (samples_hmc$beta_RasToPI3K)[j],
                                          beta0_Erk = (samples_hmc$beta0_Erk)[j],
                                          beta_AKTToErk = (samples_hmc$beta_AKTToErk)[j],
                                          beta_RasToErk = (samples_hmc$beta_RasToErk)[j],
                                          akt = 80,
                                          num_gen_samples = 1000,
                                          seed=1))
      
      
    }
    
    means_hmc <- c(means_hmc,mean(J80))
    J80 <- c()
  }
  
}
end_time <- Sys.time()
end_time - start_time
means_hmc
#saveRDS(means_hmc,file = "means_hmc_IGF_full_N100.RData")
