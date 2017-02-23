# Terrain is Cool!

start.matrix <- function(dim){
  if(dim %% 2 == 0){
    stop("Matrix must have odd dimensions. Odd as in 'not even', not odd as in 'weird'")
  }
  matrix <- matrix(ncol=dim, nrow=dim)
  matrix[1,1] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  matrix[1,dim] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  matrix[dim,1] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  matrix[dim,dim] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  return(terrain.matrix)
}
terrain.matrix <- start.matrix(7)

diamond.step <- function(matrix){ 
  top.left <- matrix[1,1]
  top.right <- matrix[1, ncol(matrix)]
  bot.left <- matrix[nrow(matrix), 1]
  bot.right <- matrix[nrow(matrix), ncol(matrix)]
  middle <- ceiling(nrow(matrix)/2)
  matrix [middle, middle] <- jitter(mean(c(top.left, top.right, bot.left, bot.right)))
  return(matrix)
} 
terrain.matrix <- diamond.step(terrain.matrix)

square.step <- function(matrix){
  top.left <- matrix[1,1]
  top.right <- matrix[1, ncol(matrix)]
  bot.left <- matrix[nrow(matrix), 1]
  bot.right <- matrix[nrow(matrix), ncol(matrix)]
  middle <- ceiling(nrow(matrix)/2)
  center <- matrix[middle, middle]
  mid.left <- jitter(mean(c(top.left, center, bot.left)))
  matrix[middle, 1] <- mid.left
  mid.right <- jitter(mean(c(top.right, center, bot.right))) 
  matrix[middle, ncol(matrix)] <- mid.right
  mid.top <- jitter(mean(c(top.right, center, top.left)))
  matrix[1, middle] <- mid.top
  mid.bot <- jitter(mean(c(bot.right, center, bot.left)))
  matrix[nrow(matrix), middle] <- mid.bot
  return(matrix)
}
terrain.matrix <- square.step(terrain.matrix)

diamond.square.step <- function()


