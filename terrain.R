# Terrain is Cool!

#' 

#creates a matrix with odd dimensions and fills the matrix with NAs
start.matrix <- function(dim){
  #throws and error message if the dimensions of the matrix aren't odd
  if(dim %% 2 == 0){
    stop("Matrix must have odd dimensions. Odd as in 'not even', not odd as in 'weird'")
  }
  matrix <- matrix(ncol=dim, nrow=dim)
  #populates the corners of the matrix with random values
  matrix[1,1] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  matrix[1,dim] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  matrix[dim,1] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  matrix[dim,dim] <- rnorm(1, rnorm(1, 0, 1), runif(1, min=0))
  return(terrain.matrix)
}

#identifies and fills in the center cell of the matrix
diamond.step <- function(matrix){ 
  top.left <- matrix[1,1]
  top.right <- matrix[1, ncol(matrix)]
  bot.left <- matrix[nrow(matrix), 1]
  bot.right <- matrix[nrow(matrix), ncol(matrix)]
  middle <- ceiling(nrow(matrix)/2)
  #takes the averages of the corners and populates the center cell of the matrix with some noise
  matrix [middle, middle] <- jitter(mean(c(top.left, top.right, bot.left, bot.right)))
  return(matrix)
} 

#populates the corners of the new smaller matrices by taking the average of nearby cells with values
square.step <- function(matrix){
  top.left <- matrix[1,1]
  top.right <- matrix[1, ncol(matrix)]
  bot.left <- matrix[nrow(matrix), 1]
  bot.right <- matrix[nrow(matrix), ncol(matrix)]
  middle <- ceiling(nrow(matrix)/2)
  center <- matrix[middle, middle]
  #calculates noisy average and populates the middle left cell
  mid.left <- jitter(mean(c(top.left, center, bot.left)))
  matrix[middle, 1] <- mid.left
  #calculates noisy average and populates middle right cell
  mid.right <- jitter(mean(c(top.right, center, bot.right))) 
  matrix[middle, ncol(matrix)] <- mid.right
  #calculates noisy average and populates middle top cell
  mid.top <- jitter(mean(c(top.right, center, top.left)))
  matrix[1, middle] <- mid.top
  #calculates noisy average and populates middle bottom cell
  mid.bot <- jitter(mean(c(bot.right, center, bot.left)))
  matrix[nrow(matrix), middle] <- mid.bot
  return(matrix)
}

diamond.square.step <- function(dim){
  terrain.matrix <- start.matrix(dim)
  terrain.matrix <- diamond.step(terrain.matrix)
  terrain.matrix <- square.step(terrain.matrix)
  half <- ceiling(nrow(terrain.matrix)/2)
  #I need to incorporate a for loop in here somehow, but I'm not exactly sure how to do it
}


