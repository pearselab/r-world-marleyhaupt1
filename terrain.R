# Terrain is Cool!

start.matrix <- function(dim){
  if(dim %% 2 == 0){
    stop("Matrix must have odd dimensions. Odd as in 'not even', not odd as in 'weird'")
  }
  terrain.matrix <- matrix(ncol=dim, nrow=dim)
  terrain.matrix[1,1] <- rnorm(1, 0, 5)
  terrain.matrix[1,dim] <- rnorm(1, 0, 5)
  terrain.matrix[dim,1] <- rnorm(1, 0, 5)
  terrain.matrix[dim,dim] <- rnorm(1, 0, 5)
  return(terrain.matrix)
}