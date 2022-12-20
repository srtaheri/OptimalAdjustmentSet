library(tidyverse)
library(mgcv)
library(R6)

cpd = R6Class("cpd",list(
  form = NA,
  parents = NA,
  child = NA,
  edges = NA,
  cpd = NULL,
  family = NA,
  method = NA,
  initialize = function(form,family=gaussian(link=identity),method="glm"){
    stopifnot(method %in% c("glm","gam"))
    self$form = formula(form)
    allvars = all.vars(self$form)
    #set parent to NA if it is a character(0)
    self$parents = allvars[-1]
    self$child = allvars[1]
    self$family = family
    self$method = method
    self$cpd = NULL
    self$edges = tibble(from=rep(NA,length(self$parents)),to=self$child)
    for(i in 1:length(self$parents)){
      self$edges$from[i] = self$parents[i]
    }
  },
  fit = function(data){
    if(self$method=="glm"){
      model_call = expr(glm(!!self$form,family=self$family,data=data))
      self$cpd = eval(model_call)
      
    }else if(self$method=="gam"){
      model_call = expr(gam(!!self$form,family=self$family,data=data))
      self$cpd = eval(model_call)
    }
    invisible(self)
  },
  predict = function(newdata){
    if(is.null(self$cpd)){
      stop("No model has been trained in order to predict from")
    }
    return(predict(self$cpd,newdata=newdata,type="response"))
  },
  
  sample = function(xdata){
    if(is.null(self$cpd)){
      stop("No model has been trained in order to simulate new samples")
    }
    distribution = self$cpd$family$family
    invlinkfn = self$cpd$family$linkinv
    
    if(!(distribution %in% c("gaussian","Gamma"))){
      stop("Sampling only implemented for Normal and Gamma families")
    }
    
    if(sum(is.na(xdata)) > 0){
      stop("xdata must have no missing data")
    }
    
    if(distribution %in% c("gaussian")){
      sigma = sqrt(summary(self$cpd)$dispersion) #extract sd 
      
      mu = self$predict(xdata)
      n = length(mu)
      y = rnorm(n,mean=mu,sd=sigma)
    }else if(distribution %in% c("Gamma")){
      alpha = MASS::gamma.shape(self$cpd)$alpha
      
      
      mu = self$predict(xdata)
      
      n = length(mu)
      
      sigma = mu/alpha 
      
      y = rgamma(n,shape=alpha,scale=sigma)
    }
    
    xdata[[self$child]] = y
    return(xdata)
  }
  
))

BayesianNetwork = R6Class("BayesianNetwork",list(
  formvec = NA,
  families = NA,
  cpds = NA,
  roots = c(),
  graph = NA,
  topological_sort = NA,
  initialize = function(formvec,families=rep(list(gaussian(link=identity)),length(formvec)),methods=rep("glm",length(formvec))){
    stopifnot(length(formvec)==length(families))
    fam_class = sapply(families,class)
    if(!all(fam_class=="family")){
      stop("families MUST be a list of family objects. Do NOT use quotes in the families list. Example with no quotes) families=c(gaussian(link=identity),Gamma(link=log)) ")
    }
    len_cpd = length(formvec)
    
    stopifnot(len_cpd==length(families))
    stopifnot(len_cpd==length(methods))
    self$formvec = formvec
    self$families = families
    
    self$cpds = vector(mode="list",length=len_cpd)
    
    for(i in 1:len_cpd){
      self$cpds[[i]] = cpd$new(form=formvec[i],family=self$families[[i]],method=methods[i])
      if(identical(self$cpds[[i]]$parents,character(0))){
        self$roots = c(self$roots,self$cpds[[i]]$child)
      }
      names(self$cpds)[i] = self$cpds[[i]]$child #set name of list element to child 
      names(self$families)[i] = self$cpds[[i]]$child
      names(self$formvec)[i] = self$cpds[[i]]$child
    }
    
    if(length(unique(names(self$cpds))) != length(names(self$cpds))){
      stop("Invalid Network: There are multiple cpds corresponding to a child node")
    }
    
    self$graph = bind_rows(lapply(self$cpds,function(x) x$edges)) |> mutate(edge = paste0(from,"->",to))
    self$topological_sort = self$get_topological_sort()
  },
  
  fit = function(data){
    for(i in 1:length(self$cpds)){
      #rint(self$families[[i]])
      self$cpds[[i]]$fit(data)
    }
    invisible(self)
  },
  
  get_topological_sort = function(){
    L = c() #empty list that will contain the sorted elements
    S = self$roots #Set of all nodes with no incoming edge
    remaining_edges = self$graph #will be used to remove the edges while sorting
    
    while(TRUE){
      n = S[1]
      S = S[-1] #removing node from S, adding to L above
      L = c(L,n)
      #print(paste0("n is currently: ", n))
      n_to_m_edge_df = self$graph |> filter(from==n)
      #print(n_to_m_edge_df)
      if(nrow(n_to_m_edge_df)>0){
        for(i in 1:nrow(n_to_m_edge_df)){
          m = n_to_m_edge_df$to[i]
          curr_edge = n_to_m_edge_df$edge[i]
          remaining_edges = remaining_edges |> filter(edge != curr_edge)
          
          other_incoming_to_m_df = remaining_edges |> filter(to==m)
          if(nrow(other_incoming_to_m_df)==0){
            S = c(S,m)
          }
        }
      }
      #print(paste0("S is currently: ", S))
      if(is.na(n)){
        break
      }
    }
    
    if(nrow(remaining_edges) != 0){
      stop("Invalid Network: No Valid Topological Sort could be found")
    }
    L = subset(L,!is.na(L))
    return(L)
  },
  
  sample = function(n){
    simdata = tibble(rownum = seq(1,n,1))
    
    for(node in self$topological_sort){
      simdata = self$cpds[[node]]$sample(simdata)
    }
    
    return(simdata)
  }
))
