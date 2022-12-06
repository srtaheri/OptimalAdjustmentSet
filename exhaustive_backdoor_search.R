#required libraries
library(remotes)
remotes::install_github("yqzhong7/AIPW")
remotes::install_github("tlverse/sl3")
library(Rsolnp)
library(SuperLearner)
library(ggplot2)
library(AIPW)
library(dagitty)
library(parallel)
library(doParallel)
#install.packages("~/Downloads/ipw_1.0-11.tar.gz", repos = NULL, type = "source")
library(WeightIt)
library(dplyr)

n.cores = parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()
#how many workers are available? (optional)
foreach::getDoParWorkers()
# find the variance for a given adjustment set
# Inputs:
## exposure : The exposure/cause/treatment variable (target of intervention)
## exposure_intv_value : The intervened value that the exposure takes. The default value is 0.
## outcome : The effect/outcome variable
## query : The query of interest in the form of "ATE" (Average treatment effect) or "expectation" (E[outcome|do(exposure = exposure_intv_value)]) or "density" (P(outcome|do(exposure = exposure_intv_value))). Default is "ATE".
## valid_adj_set : A valid adjustment set
## method: The method to estimate the causal query in the form of "lm" (linear model). Default is "lm"
## synthetic_data : A list of data sets. If not mentioned, by default linearly associated data is created
## num_dp : number of data points from the synthetic_data used to estimate the query and the variance. Default is 100.
find_query_est_for_given_adj_set <- function(exposure,  exposure_intv_value = 0, outcome, query = "ATE", valid_adj_set, method = "lm", synthetic_data, num_dp = 100, num_synthetic_data_sets = 100) {
  adjSet = valid_adj_set
  data_nds = synthetic_data[seq(1:num_synthetic_data_sets)]
  set.seed(20)
  data = lapply(data_nds, function(x) x[sample(1:nrow(x),num_dp),])
  
  if (method == "lm") {
    formula = paste(outcome,
                    paste(c(exposure,adjSet), collapse = " + "),
                    sep = " ~ ")
    models = lapply(data, function(x) lm(formula,data=x))
    if(query == "ATE") {
      estimate_query = as.vector(unlist(lapply(models, function(x) coef(x) [exposure]))) #get the coefficient
    }
    else if (query == "expectation") {
      newdata = data
      newdata = lapply(newdata, function(x) {
        x[,exposure] = exposure_intv_value
        return(x)
      })
      estimate_query <- sapply(1:length(models), function(index) mean(predict(models[[index]], newdata = newdata[[index]])))
    }
    else{
      print("The query should be ATE or expectation.")
    }
  }
  if (method == "AIPW") {
    if(query == "ATE") {
      start_time = Sys.time() #
      estimate_query = unlist(lapply(data, function(x) {
        AIPW_SL <- AIPW$new(Y = x[,outcome],
                            A = x[,exposure],
                            W = x[,adjSet],
                            Q.SL.library = c("SL.mean","SL.glm"),
                            g.SL.library = c("SL.mean","SL.glm"),
                            k_split = 3,
                            verbose=FALSE)
        suppressWarnings({
          AIPW_SL$stratified_fit()$summary()
        })
        return(AIPW_SL$result["ATC Risk Difference","Estimate"])
      }))
      end_time <- Sys.time() #
      end_time - start_time #
    }
  }
  
  if (method == "gformula") {
    # The idea of codes are from : https://vincentarelbundock.github.io/marginaleffects/articles/gformula.html
    # create a dataset with 3 copies of each subject
    estimate_query = unlist(lapply(data, function(x) {
      x$interv <- -1 # 1st copy: equal to original one
      
      interv0 <- x # 2nd copy: treatment set to 0, outcome to missing
      interv0$interv <- 0
      interv0$EGFR <- 0
      interv0$cytok <- NA
      
      interv1 <- x # 3rd copy: treatment set to 1, outcome to missing
      interv1$interv <- 1
      interv1$EGFR <- 1
      interv1$cytok <- NA
      
      onesample <- rbind(x, interv0, interv1) # combining datasets
      # This formula can change. It can be quadratic
      formula = paste(outcome,
                      paste(c(exposure,adjSet), collapse = " + "),
                      sep = " ~ ")
      std <- glm(formula, data = onesample)
      onesample$predicted_meanY <- predict(std, onesample)
      # estimate mean outcome in each of the groups interv=0, and interv=1
      #ATE
      return(mean(onesample[which(onesample$interv == 1), ]$predicted_meanY) - mean(onesample[which(onesample$interv == 0), ]$predicted_meanY))
    }))
    
  }
  
  if(method == "IPW") {
    if(query == "ATE") {
      estimate_query = unlist(lapply(data, function(x) {
        formula1 = paste(exposure,
                         paste(adjSet, collapse = " + "),
                         sep = " ~ ")
        parsed_formula1 = eval(parse(text = formula1))
        weights_weightit <- weightit(parsed_formula1,  # Model exposure with adjustment set
                                     data = x, 
                                     estimand = query,  # Find the ATE
                                     method = "ps")  # Build weights with propensity scores
        data_weightit <- x %>% 
          mutate(ipw = weights_weightit$weights)
        formula2 = paste(outcome,
                         paste(exposure, collapse = " + "),
                         sep = " ~ ")
        model_data_weightit <- lm(formula2, 
                                            data = data_weightit, weights = ipw)
        return(coef(model_data_weightit) [exposure])
      }))
      
      
    }
  }
  
  return(estimate_query)
}

# find the optimal adjustment set
# Inputs:
## g: A DAG or ADMG in the dagitty format
## exposure : The exposure/cause/treatment variable (target of intervention)
## exposure_intv_value : The intervened value that the exposure takes. The default value is 0.
## outcome : The effect/outcome variable
## query : The query of interest in the form of "ATE" (Average treatment effect) or "expectation" (E[outcome|do(exposure = exposure_intv_value)]) or "density" (P(outcome|do(exposure = exposure_intv_value))). Default is "ATE".
## method: The method to estimate the causal query in the form of "lm" (linear model). Default is "lm"
## synthetic_data : A list of data sets. If not mentioned, by default linearly associated data is created
## num_dp : number of data points from the synthetic_data used to estimate the query and the variance. Default is 100.
find_ranked_var_and_query_est_for_all_valid_adj_sets = function(g, exposure, exposure_intv_value = 0, outcome, query = "ATE", method = "lm", synthetic_data, num_dp = 100, num_synthetic_data_sets = 100) {
  
  all_valid_adjustment_sets = adjustmentSets( x = g, exposure = exposure, outcome = outcome , type = "all")
  valid_adjustment_sets_names = sapply(all_valid_adjustment_sets, paste, collapse=",")
  
  start_time = Sys.time() #
  query_est = list()
  var_est = c()
  for (adjSetIdx in 1:length(all_valid_adjustment_sets)) {
    print(adjSetIdx) #
    query_est[[adjSetIdx]] = find_query_est_for_given_adj_set(exposure = exposure,
                                                                        exposure_intv_value = exposure_intv_value,
                                                                        outcome = outcome,
                                                                        query = query,
                                                                        valid_adj_set = all_valid_adjustment_sets[[adjSetIdx]],
                                                                        method = method,
                                                                        synthetic_data = synthetic_data,
                                                                        num_dp = num_dp,
                                                                        num_synthetic_data_sets = num_synthetic_data_sets)
    var_est = c(var_est, round(var(query_est[[adjSetIdx]]), digits=3))
  }
  
  
  # query_est <- foreach(adjSetIdx=1:length(all_valid_adjustment_sets), .combine = 'c') %do% {
  #   print(adjSetIdx)
  #   find_query_est_for_given_adj_set(exposure = exposure,
  #                                   exposure_intv_value = exposure_intv_value,
  #                                   outcome = outcome,
  #                                   query = query,
  #                                   valid_adj_set = all_valid_adjustment_sets[[adjSetIdx]],
  #                                   method = method,
  #                                   synthetic_data = synthetic_data)
  # }
  #var_est = unlist(lapply(query_est, function(x) round(var(x), digits=3)))
  end_time <- Sys.time() #
  end_time - start_time #
  

  names(var_est) = valid_adjustment_sets_names
  names(query_est) = valid_adjustment_sets_names
  
  sorted_var = sort(var_est)
  #order the query estimates in ascending order base on variance of estimation. 
  sorted_query_est = query_est[(names(sorted_var))]
  
  output = list("sorted_adj_set_based_on_var" = sorted_var, "sorted_query_est" = sorted_query_est)
  return(output)
}
