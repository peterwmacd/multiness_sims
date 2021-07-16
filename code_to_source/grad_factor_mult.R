# baseline gradient descent routine for logistic MultiNeSS model

# residual matrix for multiple matrix factors
Resid <- function(A,V,U,dens=0,link=identity,alpha=0){
  m <- dim(A)[3]
  temp <- array(0,dim(A))
  for(ii in 1:m){
      temp[,,ii] <- A[,,ii] - link(tcrossprod(cbind(V,U[[ii]])) - dens) - alpha
  }
  return(temp)
}

# gradient update for multiple matrix factors
V_update <- function(V,Res,eta){
  V + eta*(apply(Res,c(1,2),sum) %*% V)
}

# extra centering step for sparse networks
U_update <- function(U,Res,eta){
  n <- dim(Res)[1]
  m <- dim(Res)[3]
  #lapply(1:m,function(ii){(diag(n) - matrix(as.integer(neg),n,n)/n) %*% (U[[ii]] + eta*(Res[,,ii] %*% U[[ii]])) })
  lapply(1:m,function(ii){U[[ii]] + eta*(Res[,,ii] %*% U[[ii]])})
}

# Gradient descent for factors (serves as a comparison for the logit case)

grad_factor_mult <- function(A,d1,d2,dens=0,
                             link=list(identity,identity),
                             alpha=0,init='svd',
                             block=F,eta,eps=1e-4,K_max=50,
                             verbose=F,return_factors=T){
  # dimensions
  n <- dim(A)[1]
  m <- dim(A)[3]
  # run a different method if d1 or d2 is zero
  # no individual factors
  # note d2=0 is not compatible with negative eigenvalues
  if(d2==0){
    fit <- grad_factor(apply(A,c(1,2),mean),d=d1,link=link,
                       alpha=alpha,eta=eta,eps=eps,K_max=K_max,
                       verbose=verbose)
    V_hat <- fit$Z_hat
    if(return_factors){
      return(list(V_hat=V_hat,Vneg_hat=NULL,U_hat=NULL,K=fit$K))
    }
    else{
      F_hat <- tcrossprod(V_hat)
      G_hat <- lapply(1:m,function(ii){matrix(0,n,n)})
      return(list(F_hat=F_hat,G_hat=G_hat,K=fit$K))
    }
  }
  # no common factors
  # note d1=0 is not compatible with negative eigenvalues
  if(d1==0){
    U_hat <- lapply(1:m,function(ii){
      grad_factor(A[,,ii],d=d2,link=link,alpha=alpha,
                  eta=eta,eps=eps,K_max=K_max,verbose=verbose)$Z_hat
    })
    if(return_factors){
      return(list(V_hat=NULL,Vneg_hat=NULL,U_hat=U_hat,K=NULL))
    }
    else{
      G_hat <- lapply(1:m,function(ii){tcrossprod(U_hat[[ii]])})
      return(list(F_hat=matrix(0,n,n),G_hat=G_hat,K=NULL))
    }
  }
  # initialization method
  # initialization
  if(init=="svd"){
    M <- link[[2]](rank_thresh(apply(A,c(1,2),mean),d1,pos=TRUE))
    V_old <- ase(M,d1)
    U_old <- lapply(1:m,function(ii){ase(link[[2]](A[,,ii] - link[[1]](M)),d2)})
  }
  if(init=="random"){
    V_old <- matrix(rnorm(n*d1),n,d1)
    U_old <- lapply(1:m,function(ii){matrix(rnorm(n*d2),n,d2)})
  }
  # update steps
  K_count <- 0
  for(kk in 1:K_max){
    K_count <- K_count+1
    # get current residual
    Res <- Resid(A,V_old,U_old,dens,link[[1]],alpha)
    # update F
    V_new <- V_update(V_old,Res,eta/(n*m))
    # update residual if in block updating case
    if(block){
      Res <- Resid(A,V_new,U_old,dens,link[[1]],alpha)
    }
    # update G
    U_new <- U_update(U_old,Res,eta/n)
    # check convergence
    conv <- mean(abs(c(V_old)-c(V_new)))
    if(conv < eps){
      break
    }
    else{
      V_old <- V_new
      U_old <- U_new
    }
    if(verbose){
      print(K_count)
      print(conv)
    }
  }
  if(return_factors){
    return(list(V_hat=V_new,Vneg_hat=Vneg_new,U_hat=U_new,K=K_count))
  }
  else{
    F_hat <- tcrossprod(V_new) - dens
    G_hat <- lapply(1:m,function(ii){tcrossprod(U_new[[ii]])})
    return(list(F_hat=F_hat,G_hat=G_hat,K=K_count))
  }
}
