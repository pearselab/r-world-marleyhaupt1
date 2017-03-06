# Terrain is Cool!

#' 

#creates a matrix with odd dimensions and fills the matrix with NAs
start.matrix <- function(dim){
  #throws and error message if the dimensions of the matrix aren't odd
  if(dim %% 2 == 0){
    stop("Matrix must have odd dimensions")
  }
  m <- matrix(ncol=dim, nrow=dim)
  #populates the corners of the matrix with random values
  m[1,1] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  m[1,dim] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  m[dim,1] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  m[dim,dim] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  return(m)
}

#identifies and fills in the center cell of the matrix
diamond.step <- function(m){ 
  tl <- m[1,1]
  tr <- m[1, ncol(m)]
  bl <- m[nrow(m), 1]
  br <- m[nrow(m), ncol(m)]
  mid <- ceiling(nrow(m)/2)
  #takes the averages of the corners and populates the center cell of the matrix with some noise
  m[mid, mid] <- jitter(mean(c(tl, tr, bl, br)))
  return(m)
} 

#populates the corners of the new smaller matrices by taking the average of nearby cells with values
square.step <- function(m){
  #assign names to key cells
  tl <- m[1,1]
  tr <- m[1, ncol(m)]
  bl <- m[nrow(m), 1]
  br <- m[nrow(m), ncol(m)]
  mid <- ceiling(nrow(m)/2)
  c <- m[mid, mid]
  #calculate average and populate cells
  ml <- jitter(mean(c(tl, c, bl)))
  m[mid, 1] <- ml
  mr <- jitter(mean(c(tr, c, br))) 
  m[mid, ncol(m)] <- mr
  mt <- jitter(mean(c(tr, c, tl)))
  m[1, mid] <- mt
  mb <- jitter(mean(c(br, c, bl)))
  m[nrow(m), mid] <- mb
  return(m)
}

diamond.square.step <- function(dim){
  m <- start.matrix(dim)
  #determines the maximum power of 2 based on the dimensions of the matrix specified
  n <- (dim-1)/2
  for(s in 2^(n:1)){
    for(i in seq(from=1, to=s+1, by=s)){ #i is for the rows
      for(j in seq(from=1, to=s+1, by=s)){ #j is for the columns
        m[i:1+s, j:1+s] <- diamond.step(m[i:1+s, j:1+s])
        m[i:1+s, j:1+s] <- square.step(m[i:1+s, j:1+s])
      }
    }
  }
  return(m)
}

terrain.func <- function(dim, lakes){ 
  m <- diamond.square.step(dim)
  #fills in the the cells of the matrix with NAs if the value is less than 0
  if(lakes == TRUE){
    for(i in 1:nrow(m)){ 
      for(j in 1:ncol(m)){
        if(m[i,j] < 0){
          m[i,j] <- NA
        }
      }
    }
  }  
  image(m)
}

  
  