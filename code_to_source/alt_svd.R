# alternating SVD approach
#library(rARPACK)

# marginal embedding (when there is no common structure)
embed_marginal <- function(A,d){
  n <- dim(A)[1]
  m <- dim(A)[3]
  X <- array(0,c(n,d,m))
  for(ii in 1:m){
    X[,,ii] <- ase(A[,,ii],d)
  }
  return(X)
}

# common embedding (when there is no individual structure)
embed_common <- function(A,d){
  m <- dim(A)[3]
  A_m <- apply(A,c(1,2),mean)
  V <- ase(A_m,d)
  return(V)
}

# alternating svd fitting algorithm
# input the tensor and dimensions, output the embeddings
joint_embed_alt <- function(A,d1,d2,
                            tol=1e-4,k_max=20,
                            verbose=F){
  n <- dim(A)[1]
  m <- dim(A)[3]
  # special cases d1=0 and d2=0
  if(d1==0){
    U <- embed_marginal(A,d2)
    return(list(V=matrix(0,n,1),U=U,k=0))
  }
  if(d2==0){
    V <- embed_common(A,d1)
    return(list(V=V,U=array(0,c(n,1,m)),k=0))
  }
  # calculate mean A matrix
  A_m <- apply(A,c(1,2),mean)
  # helper function to calculate mean of crossproduct matrices
  tcp_mean <- function(U){
    n <- dim(U)[1]
    t <- dim(U)[3]
    tcp <- array(apply(U,3,tcrossprod),c(n,n,m))
    tcp_m <- apply(tcp,c(1,2),mean)
    return(tcp_m)
  }
  # initialize V and U
  U_old <- U_new <- array(0,c(n,d2,m))
  V_old <- ase(A_m,d1)
  # alternating updates
  n_u <- 0
  for(k in 1:k_max){
    for(ii in 1:m){
      U_new[,,ii] <- ase(A[,,ii]-tcrossprod(V_old),d2)
    }
    U_m <- tcp_mean(U_new)
    V_new <- ase(A_m-U_m,d1)
    # update counter
    n_u <- n_u+1
    # check convergence
    # rotation invariant convergence check... only using V,
    # assume that if V doesn't change, U won't have changed...
    delta <- sum(abs(tcrossprod(V_old)-tcrossprod(V_new)))/(n^2)
    if(verbose){
      print(delta)
    }
    if(delta < tol){
      break
    }
    V_old <- V_new
    U_old <- U_new
  }
  return(list(U=U_new,V=V_new,K=n_u))
}

