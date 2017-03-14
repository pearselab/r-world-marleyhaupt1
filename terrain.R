#' Makes a matrix with cell numbers corresponding to elevations
#' 
#' start.matrix function
#' @param dim dim corresponds to the dimensions of the matrix; columns must equal rows and must be a power of 2 + 1 (i.e., 5 is 2*2+1)
#' @return an odd sided matrix with the corners populated with starting value
#' 
#' diamond.step function
#' @param m m is the matrix that is the output of the start.matrix function
#' @return matrix m with the center cell filled in with the average + noise of the corner values
#' 
#' square.step function
#' @param m m is the matrix that is the output of the diamond.step function
#' @return matrix m with the top center, bottom center, left center, and right center cells filled in with the average + noise of adjacent cells
#' 
#' diamond.square.step function is a wrapper around the start.matrix, diamond.step, and square.step functions
#' @param dim dim corresponds to the dimensions of the matrix; columns must equal rows and must be a power of 2 + 1 (i.e., 5 is 2*2+1)
#' @return a matrix where NAs are water and numbers are elevations
#' 
#' terrain.fun is a wrapper around diamond.square.step and can also incorporate water into the terrain where a number is negative
#' @param dim dim corresponds to the dimensions of the matrix; columns must equal rows and must be a power of 2 + 1 (i.e., 5 is 2*2+1)
#' @param lakes a logical argument that will make all negative numbers in the matrix into water (NAs), default is TRUE
#' @return a terrain matrix and an image of the terrain matrix. Numbers are heights. NAs are water
#' @examples 
#' terrain <- terrain.fun(65)
#' terrain <- terrain.fun(9)
#' @export

#creates a matrix with odd dimensions and fills the matrix with NAs
start.matrix <- function(dim){
  #error message if the dimensions aren't odd
  if(dim %% 2 == 0){
    stop("Matrix must have odd dimensions")
  }
  #error must be a power of 2 (i.e., 64 = 2*2*2*2*2*2 = 2^6)
  i <- dim-1
  while(i >= 1){
    if(i != round(i)){
      stop("Dimension minus 1 must be a power of 2")
    }  
    i <- i/2
  }
  m <- matrix(ncol=dim, nrow=dim)
  #populates the corners with random values
  m[1,1] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  m[1,dim] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  m[dim,1] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  m[dim,dim] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  return(m)
}

#identifies and fills center
diamond.step <- function(m){ 
  tl <- m[1,1]
  tr <- m[1, ncol(m)]
  bl <- m[nrow(m), 1]
  br <- m[nrow(m), ncol(m)]
  mid <- ceiling(nrow(m)/2)
  #averages corners and populates the center cell
  m[mid, mid] <- jitter(mean(c(tl, tr, bl, br)))
  return(m)
} 

#populates corners
square.step <- function(m){
  #assign names to key cells
  tl <- m[1,1]
  tr <- m[1, ncol(m)]
  bl <- m[nrow(m), 1]
  br <- m[nrow(m), ncol(m)]
  mid <- ceiling(nrow(m)/2)
  c <- m[mid, mid]
  #calculate average and populates cells
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
  s <- dim-1
  while(s >= 1){ # s is for size of sub-matrix
    for(i in seq(from=1, to=dim-1, by=s)){ #i is for the rows
      for(j in seq(from=1, to=dim-1, by=s)){ #j is for the columns
        m[i:(i+s), j:(j+s)] <- diamond.step(m[i:(i+s), j:(j+s)])
        m[i:(i+s), j:(j+s)] <- square.step(m[i:(i+s), j:(j+s)])
      }
    }
    s <- s/2
  }
  return(m)
}

terrain.fun <- function(dim, lakes=TRUE){ 
  m <- diamond.square.step(dim)
  #fills cells of matrix with NAs if the value is less than 0
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
  return(m)
}

  
  