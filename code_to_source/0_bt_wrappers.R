# BATCHTOOLS
# wrapper functions

#### problems ####

multiness_sim_wrapper <- function(data,job,
                                  n,m,d1,d2,model,dens,rho){
    temp <- multiness_sim(n,m,d1,d2,model,
                          self_loops=FALSE,
                          opts=list(density_shift=dens,
                                    dependence_type='all',
                                    return_P=TRUE,
                                    rho=rho))
    out <- list(A=temp$A,P=temp$P,V=temp$V,U=temp$U,
                d1=d1,d2=d2,dens=dens,model=model,rho=rho)
    # zero matrices for null dimensions
    if(d1==0){
      out$V = matrix(0,n,1)
    }
    if(d2==0){
      out$U = array(0,c(n,1,m))
    }
    return(out)
}

#### baseline methods ####

alt_svd_wrapper <- function(data,job,instance){
  # function call
  method <- joint_embed_alt(instance$A,instance$d1,instance$d2,k_max=100)
  # get errors
  out <- list()
  m <- dim(instance$A)[3]
  # overall errors
  # F matrix errors
  out$Ferr  <- ffnorm(method$V,instance$V,normalize=T,hollow=hollowize)
  # G matrix errors
  out$Gerr <- mean(sapply(1:m,function(ii){ffnorm(method$U[,,ii],instance$U[,,ii],normalize=T,hollow=hollowize)}))
  # P matrix errors
  out$Perr <- mean(sapply(1:m,function(ii){fmnorm(cbind(method$V,method$U[,,ii]),instance$P[,,ii],normalize2=T,hollow=hollowize)}))
  # correct rank
  out$F_rank <- 1
  out$G_rank <- m
  # number of iterations
  out$K <- method$K
  return(out)
}

grad_factor_mult_wrapper <- function(data,job,instance){
  # set link functions
  linkl=list(expit,logit)
  # check if there is a negative eigenvalue
  method <- grad_factor_mult(instance$A,
                             instance$d1,instance$d2,
                             instance$dens,
                             init='random',
                             K_max=100 + 100*instance$dens,
                             link=linkl,
                             eta=6,
                             return_factors=FALSE)
  # get errors
  out <- list()
  m <- dim(instance$A)[3]
  # overall errors
  # F matrix errors
  out$Ferr <- fmnorm(instance$V,method$F_hat + instance$dens,normalize=T,hollow=hollowize)
  # G matrix errors
  out$Gerr <- mean(sapply(1:m,function(ii){fmnorm(instance$U[,,ii],method$G_hat[[ii]],normalize=T,hollow=hollowize)}))
  # P matrix errors
  out$Perr <- mean(sapply(1:m,function(ii){mmnorm(expit(method$F_hat + method$G_hat[[ii]]),instance$P[,,ii],normalize=T,hollow=hollowize)}))
  # correct rank
  out$F_rank <- 1
  out$G_rank <- m
  # number of iterations
  out$K <- method$K
  return(out)
}

cosie_wrapper <- function(data,job,instance){
  # set dimensions
  d1 <- instance$d1
  d2 <- instance$d2

  # run method
  method <- cosie_prewrapper(instance$A,d1,d2)

  # store errors (only P_error for COSIE method)
  m <- dim(instance$A)[3]
  # P matrix errors
  if(instance$model=="gaussian"){
    Perr <- mean(sapply(1:m,function(ii){mmnorm(method[[ii]],instance$P[,,ii],normalize=T,hollow=hollowize)}))
  }
  else{
    Perr <- mean(sapply(1:m,function(ii){mmnorm(mat_entry_trunc(method[[ii]],5),instance$P[,,ii],normalize=T,hollow=hollowize)}))
  }
  out <- list(Perr=Perr)
  out$Ferr <- out$Gerr <- out$F_rank <- out$G_rank <- out$K <- NA
  return(out)
}

# MGRAF method for logit link only
mgraf_wrapper <- function(data,job,instance){
  # set dimensions
  d1 <- instance$d1
  d2 <- instance$d2
  m <- dim(instance$A)[3]
  # method
  method <- mgraf_prewrapper(instance$A,d2)
  # get errors
  out <- list()
  if(!all(is.na(method$F_hat))){
    # overall errors
    # F matrix errors
    out$Ferr <- fmnorm(instance$V,method$F_hat + instance$dens,normalize=T,hollow=hollowize)
    # G matrix errors
    out$Gerr <- mean(sapply(1:m,function(ii){fmnorm(instance$U[,,ii],method$G_hat[[ii]],normalize=T,hollow=hollowize)}))
    # P matrix errors
    out$Perr <- mean(sapply(1:m,function(ii){mmnorm(expit(method$F_hat + method$G_hat[[ii]]),instance$P[,,ii],normalize=T,hollow=hollowize)}))
  }
  else{
    out$Ferr <- out$Gerr <- out$Perr <- NA
  }
  out$F_rank <- NA
  out$G_rank <- m
  out$K <- NA
  return(out)
}

#### multiness ####

multiness_fit_wrapper <- function(data,job,instance,
                                  refit){
  # fit multiness model with adaptive tuning (except for low density logistic
  # model with refit)
  if((instance$dens > 1) & refit){
    # hard code CV constants for different densities
    n <- dim(instance$A)[1]
    sigma_cv <- c(NA,0.34,0.27,0.20,0.14,0.08)
    C_cv <- ((17 - 2*instance$dens):(20 - 2*instance$dens))/(sigma_cv[instance$dens]*sqrt(n))
    # fit with CV tuning
    method <- multiness_fit(instance$A,
                            model=instance$model,
                            self_loops=FALSE,
                            refit=refit,
                            tuning='cv',
                            tuning_opts=list(layer_wise=FALSE,
                                             penalty_const_vec=C_cv),
                            optim_opts=list(K_max=100+50*instance$dens))
  }
  else{
    method <- multiness_fit(instance$A,
                            model=instance$model,
                            self_loops=FALSE,
                            refit=refit,
                            tuning='adaptive',
                            tuning_opts=list(layer_wise=FALSE,
                                             penalty_const=ifelse(refit,2.309,2.01)),
                            optim_opts=list(K_max=100+50*instance$dens))
    
  }
  # get errors
  out <- list()
  m <- dim(instance$A)[3]
  # overall errors
  # F matrix errors
  out$Ferr <- fmnorm(instance$V,method$F_hat + instance$dens,normalize=(instance$rho==0),hollow=hollowize)
  # G matrix errors
  out$Gerr <- mean(sapply(1:m,function(ii){fmnorm(instance$U[,,ii],method$G_hat[[ii]],normalize=(instance$rho==0),hollow=hollowize)}))
  # P matrix errors
  if(instance$model=='gaussian'){
    out$Perr <- mean(sapply(1:m,function(ii){mmnorm(method$F_hat + method$G_hat[[ii]],instance$P[,,ii],normalize=(instance$rho==0),hollow=hollowize)}))
  }
  if(instance$model== 'logistic'){
    out$Perr <- mean(sapply(1:m,function(ii){mmnorm(expit(method$F_hat + method$G_hat[[ii]]),instance$P[,,ii],normalize=T,hollow=hollowize)}))
  }
  # correct rank
  out$F_rank <- as.integer(method$d1 == (instance$d1 + as.integer(instance$dens > 0)))
  out$G_rank <- sum(as.integer(method$d2 == instance$d2))
  # number of iterations
  out$K <- method$K
  return(out)
}
