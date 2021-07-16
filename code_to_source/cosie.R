# wrapper for COSIE/MASE from github

# wrapper function
# for estimated dimension, pass d1,d2 as NA
cosie_prewrapper <- function(A,d1=NA,d2=NA){
  # reshape data
  m <- dim(A)[3]
  A_list <- lapply(1:m,function(ii){A[,,ii]})
  # get total dimension
  d_tot <- d1 + m*d2
  # run multiRDPG with total dimension
  if(m<3){
    temp <- mase(A_list,d_tot,rep(d1+d2,m),elbow_mase=1)
  }
  else{
    temp <- mase(A_list,d_tot,rep(d1+d2,m))
  }
  # return a list of P hat matrices
  P_hat <- lapply(1:m,function(ii){temp$V %*% temp$R[[ii]] %*% t(temp$V)})
  return(P_hat)
}

