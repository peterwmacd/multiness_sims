# auxilliary functions for nuclear norm problem

# load required libraries
#library(rARPACK)
#library(pROC)

# make an nxn matrix 'hollow' (zero diagonal) (from multiness utils)
hollowize <- function(M){
  n <- dim(M)[1]
  M*(1-diag(n))
}

# einfo_to_mat (from multiness utils)
einfo_to_mat <- function(einfo,eig_prec=0){
  eval <- einfo$vals*(abs(einfo$vals) > eig_prec)
  return((einfo$vecs %*% (eval * t(einfo$vecs))))
}

# hard singular value thresholding for symmetric PSD matrices
# for initialization use the psd version (i.e. ignore negative eigenvalues)
# (from multiness utils)
rank_thresh <- function(M,max_rank,pos=FALSE,eig_maxitr=1000){
  # dimensions
  n <- dim(M)[1]
  # PSD version
  if(pos){
    estr <- "LA"
  }
  else{
    estr <- "LM"
  }
  # truncated eigendecomposition
  e <- RSpectra::eigs_sym(M,min(max_rank,n-1),
                          which=estr,opts=list(maxitr=eig_maxitr))
  return(einfo_to_mat(list(vals=e$values,vecs=e$vectors)))
}

# log-odds scale matrix truncation (for COSIE)
mat_entry_trunc <- function(M,c=Inf){
  ub <- expit(c)
  lb <- expit(-c)
  M <- matrix(pmax(pmin(M,ub),lb),nrow=nrow(M),ncol=ncol(M))
  return(M)
}


