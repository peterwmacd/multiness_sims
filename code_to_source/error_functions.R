# error functions

# rmse for latent factors (both passed as factors)
# pass true value as second argument
# give hollow=hollowize for hollow matrices (o/w will set identity fn)
ffnorm <- function(fact1,fact2,normalize=F,hollow=identity){
  e <- norm(hollow(tcrossprod(fact1)-tcrossprod(fact2)),type="F")
  if(normalize){
    normalizer <- norm(hollow(tcrossprod(fact2)),type="F")
    if(normalizer > 0){
      return(e/normalizer)
    }
    else{
      return(NA)
    }
  }
  else{
    return(e)
  }
}

# rmse for latent factors (first as factors, second as matrix)
# pass the true value as the first (factor) argument
# give hollow=hollowize for hollow matrices (o/w will set identity fn)
fmnorm <- function(fact1,mat2,normalize=F,normalize2=F,hollow=identity){
  e <- norm(hollow(tcrossprod(fact1)-mat2),type="F")
  if(normalize){
    normalizer <- norm(hollow(tcrossprod(fact1)),type="F")
    if(normalizer > 0){
      return(e/normalizer)
    }
    else{
      return(NA)
    }
  }
  if(normalize2){
    normalizer <- norm(hollow(mat2),type="F")
    if(normalizer > 0){
      return(e/normalizer)
    }
    else{
      return(NA)
    }
  }
  if(!normalize & !normalize2){
    return(e)
  }
}

# rmse for latent factors (first as matrix, second as matrix)
# pass true value as second argument
# give hollow=hollowize for hollow matrices (o/w will set identity fn)
mmnorm <- function(mat1,mat2,normalize=F,hollow=identity){
  e <- norm(hollow(mat1-mat2),type="F")
  if(normalize){
    normalizer <- norm(hollow(mat2),type="F")
    if(normalizer > 0){
      return(e/normalizer)
    }
    else{
      return(NA)
    }
  }
  else{
    return(e)
  }
}

