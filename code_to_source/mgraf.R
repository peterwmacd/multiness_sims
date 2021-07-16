## pre-wrapper function for MGRAF (Wang, Dunson 2019)

#install.packages('CISE')
#library(CISE)

tri_to_mat <- function(t_vec){
  if(is.null(t_vec)){
    return(NA)
  }
  if(all(is.na(t_vec))){
    return(NA)
  }
  m <- length(t_vec)
  n <- (1 + sqrt(1 + 8*m))/2
  M <- matrix(0,n,n)
  M[lower.tri(M)] <- t_vec
  M <- M + t(M)
  return(M)
}

mgraf_prewrapper <- function(A,d2){
  m <- dim(A)[3]
  F_hat <- G_hat <- U_hat <- NA
  try(fit <- CISE::MGRAF1(A,d2),silent=T)
  # convert to matrices for return statements
  try(F_hat <- tri_to_mat(fit$Z),silent=T)
  try(G_hat <- lapply(1:m,function(ii){tri_to_mat(fit$D_LT[,ii])}),silent=T)
  return(list(F_hat=F_hat,G_hat=G_hat))
}

