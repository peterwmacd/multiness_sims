# Ma and Ma method for direct update of factors (without covariates)
# here for one layer

# gradient update for single matrix factors
Z_update <- function(Z,A,eta,link=identity,alpha=0){
  n <- dim(A)[1]
  Res <- A - link(tcrossprod(Z)) - alpha
  return(Z + eta*(Res %*% Z))
}

grad_factor <- function(A,d,link=list(identity,identity),
                         alpha=0,
                         eta,eps=1e-4,K_max=50,
                         verbose=F){
  # dimensions
  n <- dim(A)[1]
  # initialization
  M <- link[[2]](rank_thresh(A,d))
  Z_old <- ase(M,d)
  # set step size
  eta_Z <- eta / norm(Z_old,type="2")^2
  # start counter
  K_count <- 0
  for(kk in 1:K_max){
    K_count <- K_count+1
    Z_new <- Z_update(Z_old,A,eta_Z,link[[1]],alpha)
    # check convergence
    conv <- mean(abs(Z_old - Z_new))
    if(verbose){
      print(K_count)
      print(conv)
    }
    if(conv < eps){
      break
    }
    else{
      Z_old <- Z_new
    }
  }
  return(list(Z_hat=Z_new,K=K_count))
}

