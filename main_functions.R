#required libraries
library(remotes)
remotes::install_github("yqzhong7/AIPW")
remotes::install_github("tlverse/sl3")
library(Rsolnp)
library(SuperLearner)
library(ggplot2)
library(AIPW)
library(dagitty)
#install.packages("~/Downloads/ipw_1.0-11.tar.gz", repos = NULL, type = "source")
library(WeightIt)
library(dplyr)
library(combinat)
library(ggplot2)



# find the variance for a given adjustment set
# Inputs:
## exposure : The exposure/cause/treatment variable (target of intervention)
## exposure_intv_value : The intervened value that the exposure takes. The default value is 0.
## outcome : The effect/outcome variable
## query : The query of interest in the form of "ATE" (Average treatment effect) or "expectation" (E[outcome|do(exposure = exposure_intv_value)]). Default is "ATE".
## method: The method to estimate the causal query in the form of "lm" (linear model). Default is "lm"
## synthetic_data : A list of data sets. If not mentioned, by default linearly associated data is created
## num_dp : number of data points from the synthetic_data used to estimate the query and the variance. Default is 100.
find_query_est_for_given_adj_set <- function(exposure,  exposure_intv_value = 0, outcome, query = "ATE", valid_adj_set, method = "lm", synthetic_data, num_dp = 100, num_synthetic_data_sets = 100) {
  adjSet = valid_adj_set
  if(length(adjSet) < 1) {
    method = "lm"
  }
  data_nds = synthetic_data[seq(1:num_synthetic_data_sets)]
  set.seed(20) #20
  data = lapply(data_nds, function(x) x[sample(1:nrow(x),num_dp),])
  
  if (method == "lm") {
    print("lm")
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
      estimate_query = c()
      for (i in 1:num_synthetic_data_sets) {
        # if(i == 167 || i == 240 || i == 323 || i == 470) {
        #   next
        # }
        print(i)
        AIPW_SL <- AIPW$new(Y = data[[i]][,outcome],
                            A = data[[i]][,exposure],
                            W = data[[i]][,adjSet],
                            Q.SL.library = c("SL.mean","SL.glm"),
                            g.SL.library = c("SL.mean","SL.glm"),
                            k_split = 3,
                            verbose=FALSE)
        suppressWarnings({
          AIPW_SL$stratified_fit()$summary()
        })
        estimate_query = c(estimate_query, AIPW_SL$result["ATC Risk Difference","Estimate"])
        
      }
      # estimate_query = unlist(lapply(data, function(x) {
      #   AIPW_SL <- AIPW$new(Y = x[,outcome],
      #                       A = x[,exposure],
      #                       W = x[,adjSet],
      #                       Q.SL.library = c("SL.mean","SL.glm"),
      #                       g.SL.library = c("SL.mean","SL.glm"),
      #                       k_split = 3,
      #                       verbose=FALSE)
      #   suppressWarnings({
      #     AIPW_SL$stratified_fit()$summary()
      #   })
      #   return(AIPW_SL$result["ATC Risk Difference","Estimate"])
      # }))
    }
  }
  
  if (method == "gformula") {
    # The idea of codes are from : https://vincentarelbundock.github.io/marginaleffects/articles/gformula.html
    # create a dataset with 3 copies of each subject
    estimate_query = unlist(lapply(data, function(x) {
      x$interv <- -1 # 1st copy: equal to original one
      
      interv0 <- x # 2nd copy: treatment set to 0, outcome to missing
      interv0$interv <- 0
      interv0[,exposure] <- 0
      interv0[,outcome] <- NA
      
      interv1 <- x # 3rd copy: treatment set to 1, outcome to missing
      interv1$interv <- 1
      interv1[,exposure] <- 1
      interv1[,outcome] <- NA
      
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
        data_weightit <- mutate(x, ipw = weights_weightit$weights)
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
## query : The query of interest in the form of "ATE" (Average treatment effect) or "expectation" (E[outcome|do(exposure = exposure_intv_value)]). Default is "ATE".
## method: The method to estimate the causal query in the form of "lm" (linear model). Default is "lm"
## synthetic_data : A list of data sets. If not mentioned, by default linearly associated data is created
## num_dp : number of data points from the synthetic_data used to estimate the query and the variance. Default is 100.
## num_synthetic_data_sets : number of synthetic data sets used to estimate the query and the variance. Default is 100.
## round_var_decimal_place : inyeger value. The number of the decimal placd to round the variance. Default is 7.
find_ranked_var_and_query_est_for_all_valid_adj_sets = function(g,
                                                                exposure,
                                                                exposure_intv_value = 0,
                                                                outcome,
                                                                all_valid_adj,
                                                                query = "ATE",
                                                                method = "lm",
                                                                synthetic_data,
                                                                num_dp = 100,
                                                                num_synthetic_data_sets = 100,
                                                                round_var_decimal_place = 7) {
  
  #all_valid_adjustment_sets = adjustmentSets( x = g, exposure = exposure, outcome = outcome , type = "all")
  all_valid_adjustment_sets = all_valid_adj
  valid_adjustment_sets_names = sapply(all_valid_adjustment_sets, paste, collapse=",")
  
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
    var_est = c(var_est, round(var(query_est[[adjSetIdx]]), round_var_decimal_place ))
  }

  names(var_est) = valid_adjustment_sets_names
  names(query_est) = valid_adjustment_sets_names
  
  sorted_var = sort(var_est)
  #order the query estimates in ascending order base on variance of estimation. 
  sorted_query_est = query_est[(names(sorted_var))]
  
  output = list("sorted_adj_set_based_on_var" = sorted_var, "sorted_query_est" = sorted_query_est)
  return(output)
}
# Get all the forbidden variables for the adjustment set
get_forbidden_vars <- function(g, from, to) {
  proper_paths <- dagitty::paths(x = g, from = from, to = to, directed = TRUE)
  vars = c()
  for (path in proper_paths$paths) {
    vars = c(vars, unlist(strsplit(path, split = " -> ")))
  }
  vars = unique(vars)
  forbidden_vars = vars
  for (var in vars) {
    forbidden_vars = c(forbidden_vars, dagitty::descendants(x = g, v = var))
  }
  forbidden_vars = unique(forbidden_vars)
  return(forbidden_vars)
}

# Create all valid adjustment sets excluding latent variables
## g: A DAG or ADMG in the dagitty format
## exposure : The exposure/cause/treatment variable (target of intervention)
## outcome : The effect/outcome variable

# covariates <- setdiff(names(x), c(exposure, outcome))
# subsets <- (expand.grid(rep(list(0:1), length(covariates))))
# r <- lapply(1:nrow(subsets), function(i) {
#   Z <- covariates[as.logical(subsets[i, ])]
#   if (isAdjustmentSet(x, Z, exposure = exposure, outcome = outcome)) {
#     Z
#   }
#   else {
#     NA
#   }
# })
# non.r <- which(sapply(r, function(x) isTRUE(is.na(x))))
# if (length(non.r) > 0) {
#   r <- r[-non.r]
# }
# r <- structure(r, class = "dagitty.sets")

# all_valid_adj_sets <- function(g, exposure, outcome) {
#   
#   adj_minimal <- adjustmentSets(x = g, exposure = exposure, outcome = outcome, type = "minimal")
#   forbidden_vars <- get_forbidden_vars(g = g, from = exposure, to = outcome)
#   covariates <- setdiff(names(g), forbidden_vars)
#   #adj_canonical <- adjustmentSets(x = g, exposure = exposure, outcome = outcome, type = "canonical")
#   
#   result <- list()
#   for (i in 1:length(adj_minimal)) {
#     difference <- setdiff(covariates, adj_minimal[[i]])
#     if(i>1 && sum(difference %in% adj_minimal[[i-1]]) > 0) {
#       difference <- difference[-which(difference %in% adj_minimal[[1]])]
#     }
#     my_combi <- unlist(lapply(1:length(difference),    # Get all combinations
#                               combinat::combn, 
#                               x = difference,
#                               simplify = FALSE), 
#                               recursive = FALSE)
#     my_combi_vars = lapply(my_combi,function(x) sort(c(adj_minimal[[i]],x)))
#     r = c()
#     for (my_combi_var_idx in 1:length(my_combi_vars)) {
#       if(isAdjustmentSet(x = g, Z = my_combi_vars[[my_combi_var_idx]], exposure = exposure, outcome = outcome) == FALSE) {
#         r = c(r,my_combi_var_idx)
#       }
#     }
#     if (length(r) > 0) {
#       my_combi_vars <- my_combi_vars[-r]
#     }
#     my_combi_vars[[length(my_combi_vars)+1]] <- adj_minimal[[i]]
#     result[[i]] <- my_combi_vars
#   }
#   result = unlist(result, recursive = F)
#   r = c()
#   for (result_element_idx1 in 1:(length(result)-1)) {
#     for (result_element_idx2 in (result_element_idx1 + 1):length(result)) {
#       if(length(result[[result_element_idx1]]) == length(result[[result_element_idx2]]) && sum(result[[result_element_idx1]] %in% result[[result_element_idx2]]) == length(result[[result_element_idx1]])) {
#         r = c(r,result_element_idx1)
#       }
#     }
#   }
#   
#   return(result)
# }
all_valid_adj_sets <- function(g, exposure, outcome) {
  
  #adj_minimal <- adjustmentSets(x = g, exposure = exposure, outcome = outcome, type = "minimal")
  forbidden_vars <- get_forbidden_vars(g = g, from = exposure, to = outcome)
  covariates <- setdiff(names(g), forbidden_vars)
  #adj_canonical <- adjustmentSets(x = g, exposure = exposure, outcome = outcome, type = "canonical")
  my_combi <- unlist(lapply(1:length(covariates),    # Get all combinations
                            combinat::combn, 
                            x = covariates,
                            simplify = FALSE), 
                     recursive = FALSE)
  
  r = c()
  for (my_combi_var_idx in 1:length(my_combi)) {
    if(isAdjustmentSet(x = g, Z = my_combi[[my_combi_var_idx]], exposure = exposure, outcome = outcome) == FALSE) {
      r = c(r,my_combi_var_idx)
    }
  }
  if (length(r) > 0) {
    my_combi <- my_combi[-r]
  }
  
  return(my_combi)
}
#Simplifies the graph according to Evan's simplification rules.
#The simplification rules are as follows:
#1) Remove latent variable with no children from the graph
#2) Remove an exogenous latent variable that has at most one child
#3) Transform a latent variable with parents to an exogenous variable where all its parents are connected to its children
#4) If U and W are latent variables where children of W are a subset of children of U, then, W can be removed.
# add this too: remove all descendants of the effect, if effect is specified
#Input:
#g: A dagitty graph that contains latent nodes and does not have any bidirected edges
#g_string: The string format of the g
#Output:
# A dagitty format string of a simpified dag.
#example input:
# g <- dagitty( "dag {U1 [latent]
#                     U2 [latent]
#                     U3 [latent]
#                     U4 [latent]
#                     U5 [latent]
#                     Z1 -> Z2
#                     Z2 -> Z3
#                     Z3 -> Y
#                     Z1 -> X
#                     X -> Y;
#                     Z4 -> Z3
#                     Z4 -> Y
#                     X -> U1
#                     U2 -> Z1
#                     U2 -> Z2
#                     U3 -> Z1
#                     U3 -> Z2
#                     U3 -> Z3
#                     U5 -> Z3
#                     U5 -> Y
#                     U4 -> Y
#                     Z4 -> U5
#                     Z4 -> U5
#                     Z5 ->U5}")
# g_string <- " U1 [latent]
#               U2 [latent]
#               U3 [latent]
#               U4 [latent]
#               U5 [latent]
#               Z1 -> Z2
#               Z2 -> Z3
#               Z3 -> Y
#               Z1 -> X
#               X -> Y
#               Z4 -> Z3
#               Z4 -> Y
#               X -> U1
#               U2 -> Z1
#               U2 -> Z2
#               U3 -> Z1
#               U3 -> Z2
#               U3 -> Z3
#               U5 -> Z3
#               U5 -> Y
#               U4 -> Y
#               Z4 -> U5
#               Z5 ->U5"
#Example output:
# g_string <- " U3 [latent]
#               U5 [latent]
#               Z1 -> Z2
#               Z2 -> Z3
#               Z3 -> Y
#               Z1 -> X
#               X -> Y
#               Z4 -> Z3
#               Z4 -> Y
#               U3 -> Z1
#               U3 -> Z2
#               U3 -> Z3
#               U5 -> Z3
#               U5 -> Y
#               Z5 -> Z3
#               Z5 -> Y
#               Z4 -> Z3
#               Z4 -> Y"
generate_simplified_graph_string <- function(g, g_string) {
  glines = lapply(strsplit(g_string, "\n"), trimws)
  #find the latent nodes
  e_from = c()
  e_to = c()
  latent_nodes = c()
  for (line in glines[[1]]) {
    if (grepl("->", line)) {
      edge_splitted = lapply(strsplit(line, "->"), trimws)
      e_from = c(e_from, edge_splitted[[1]][1])
      e_to = c(e_to, edge_splitted[[1]][2])
    }
    if (grepl("latent", line)) {
      latent_nodes = c(latent_nodes, strsplit(line, " ")[[1]][1])
    }
  }
  #create a datafram for edges
  edge_df = data.frame("from" = e_from, "to" = e_to)
  remove_rows <- c()
  add_edge_df = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(add_edge_df) = c("from", "to")
  lt_children_list <- list()
  #Apply rule 1, 2, 3
  for (lt in latent_nodes) {
    pars = dagitty::parents(g, lt)
    chils = dagitty::children(g, lt)
    if(length(chils)>0) {
      lt_children_list[[lt]] = chils
    }
    for (rowIdx in 1:nrow(edge_df)) {
      # Rule 1 & 2
      if ((edge_df[rowIdx, "from"] == lt && length(chils) == 1) || (edge_df[rowIdx, "to"] == lt && length(chils) == 0)) {
        remove_rows <- c(remove_rows, rowIdx)
      }
      if (length(pars) > 0 && edge_df[rowIdx, "to"] == lt && length(chils) >0) {
        remove_rows <- c(remove_rows, rowIdx)
        df = data.frame("from" = edge_df[rowIdx, "from"], "to" = chils)
        add_edge_df = rbind(add_edge_df, df)
      }
    }
  }
  edge_df = edge_df[-remove_rows,]
  edge_df = rbind(edge_df, add_edge_df)
  rownames(edge_df) = seq(1:nrow(edge_df))
  remove_rows = c()
  #Apply rule 4
  for (rowIdx in 1:nrow(edge_df)) {
    for (lt1_idx in 1:(length(lt_children_list)-1)){
      #print(lt1_idx)
      for (lt2_idx in (lt1_idx+1):length(lt_children_list)) {
        #print(lt2_idx)
        if(edge_df[rowIdx, "from"] == names(lt_children_list)[lt1_idx]  && sum(lt_children_list[[lt1_idx]] %in% lt_children_list[[lt2_idx]]) == length(lt_children_list[[lt1_idx]])) {
          remove_rows = c(remove_rows, rowIdx)
        }
      }
  }
  }  
  if(length(remove_rows) > 0) {
    edge_df = edge_df[-remove_rows,]
  }
  rownames(edge_df) = seq(1:nrow(edge_df))
  remaining_lt_nodes = c()
  for (lt in latent_nodes) {
    if(length(which(edge_df["from"] == lt) > 0)) {
      remaining_lt_nodes = c(remaining_lt_nodes,lt)
    }
  }
  edge_df_str = paste(apply(edge_df, 1, function(x) paste0(x[1],"->",x[2])), collapse=";\n")
  if(length(remaining_lt_nodes) > 0) {
    latent_nodes_string_str = paste(remaining_lt_nodes, " [latent]" , collapse=";\n")
    dagitty_input_str = paste("dag {", "\n" , latent_nodes_string_str, edge_df_str, ";\n}", sep="")
  }
  
  dagitty_input_str = paste("dag {", "\n", edge_df_str, ";\n}", sep="")
  
  return(dagitty_input_str)
}

#removes latent variables and show their presence by bidirected edges. Outputs a dagitty string
#Inputs:
#g_string: A dagitty format string of the dag that contains latent nodes and no bidirected edges
#Output: A dagitty format string of an admg that doesn't contain any latent nodes and has bidirected edges.
#Example input:
# g_string = "Z3 [latent]
#             Z1 -> Z2
#             Z3 -> Y
#             Z3 -> Z2
#             Z1 -> X
#             X -> Y
#             Z4 -> X"
#Example output:
#            "Z1 -> Z2
#             Z2 <-> Y
#             Z1 -> X
#             X -> Y
#             Z4 -> X"
generate_dagitty_input_string <- function(g_string) {
  glines = lapply(strsplit(g_string, "\n"), trimws)
  
  e_from = c()
  e_to = c()
  latent_nodes = c()
  for (line in glines[[1]]) {
    if (grepl("->", line)) {
      edge_splitted = lapply(strsplit(line, "->"), trimws)
      e_from = c(e_from, edge_splitted[[1]][1])
      e_to = c(e_to, edge_splitted[[1]][2])
    }
    if (grepl("latent", line)) {
      latent_nodes = c(latent_nodes, strsplit(line, " ")[[1]][1])
    }
  }
  
  reg_edge_df = data.frame("from"=e_from, "to"=e_to)
  
  lt_edge_df = data.frame(matrix(nrow = 0, ncol = 3))
  colnames(lt_edge_df) = c("from", "to")
  
  for (lt in latent_nodes) {
    lt_indices = which(reg_edge_df[,"from"] == lt)
    lt_edges = reg_edge_df[lt_indices,]
    lt_children = lt_edges[,"to"]
    new_edges = t(as.data.frame(combn(lt_children, 2)))
    colnames(new_edges) = c("from", "to")
    lt_edge_df = rbind(lt_edge_df, new_edges)
    reg_edge_df = reg_edge_df[-lt_indices,]
  }
  rownames(reg_edge_df) = seq(1:nrow(reg_edge_df))
  rownames(lt_edge_df) = seq(1:nrow(lt_edge_df))
  
  lt_edges_str = paste(apply(lt_edge_df, 1, function(x) paste0(x[1],"<->",x[2])), collapse=";\n")
  
  reg_edges_str = paste(apply(reg_edge_df, 1, function(x) paste0(x[1],"->",x[2])), collapse = ";\n")
  
  dagitty_input_str = paste("dag {", reg_edges_str, ";\n", lt_edges_str, ";\n}", sep="")
  return(dagitty_input_str)
}

# I want to write a function that takes the dagitty string, the cause, effect. It outputs a simplified graph where all the descendants
# of the effect are removed and all the remaining single nodes are removed.


