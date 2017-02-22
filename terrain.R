# Terrain is Cool!

start.matrix <- function(dim){
  if(dim %% 2 == 0){
    stop("Matrix must have odd dimensions. Odd as in 'not even', not odd as in 'weird'")
  }
  terrain.matrix <- matrix(ncol=dim, nrow=dim)
  terrain.matrix[1,1] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  terrain.matrix[1,dim] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  terrain.matrix[dim,1] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  terrain.matrix[dim,dim] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  return(terrain.matrix)
}
terrain.matrix <- start.matrix(7)

diamond.step <- function(matrix){ 
  top.left <- matrix[1,1]
  top.right <- matrix[1, ncol(matrix)]
  bot.left <- matrix[nrow(matrix), 1]
  bot.right <- matrix[nrow(matrix), ncol(matrix)]
  middle <- ceiling(nrow(matrix)/2)
  matrix [middle, middle] <- jitter(mean(top.left, top.right, bot.left, bot.right))
  return(matrix)
}

square.step <- function(matrix){
  top.left <- matrix[1,1]
  top.right <- matrix[1, ncol(matrix)]
  bot.left <- matrix[nrow(matrix), 1]
  bot.right <- matrix[nrow(matrix), ncol(matrix)]
}