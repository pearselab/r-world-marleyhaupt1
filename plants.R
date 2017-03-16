#' setup.plants is a function that creates a list of plant info
#' @param repro reproduction probability, must be a repro prob for every plant species, a vector with each value between 0 and 1
#' @param survival survival probability, must be a survival prob for every plant species, a vector with each value between 0 and 1
#' @param comp.matrix competition matrix of survival probabilities between species, rows and columns must be equal to the number of plant species, each cell is populated with a value between 0 and 1
#' @param names names of the plants being simulated, if null plants are "a", "b", "c", etc.
#' @return a list with the repro probability, survival probability, comp matrix probabilities, and names

setup.plants <- function(repro, survival, comp.matrix, names=NULL){
  if(is.null(names)){
    names <- letters[seq_along(repro)]
  }
  if(length(repro) != length(survival)){
    stop("Reproduction and Survival parameters must be the same length")
  }
  if(class(comp.matrix) != "matrix"){
    stop("Comp.matrix parameter must be a matrix class")
  }
  if(nrow(comp.matrix) != length(repro) & ncol(comp.matrix) != length(repro)){
    stop("Comp.matrix dimensions must be same length as reproduction and survival")
  }
  repro <- setNames(repro, names)
  survival <- setNames(survival, names)
  rownames(comp.matrix) <- names
  colnames(comp.matrix) <- names
  return(list(repro=repro, survival=survival, comp.matrix=comp.matrix, names=names))
}

#creating some initial parameters to test
#r <- c(0.5,0.5)
#s <- c(0.7, 0.6)
#cm <- matrix(data=c(0.9, 0.7, 0.3, 0.5), 2, 2)
#names <- c("Bill", "Ted")
#info <- setup.plants(repro = r, survival = s, comp.matrix = cm, names = names)
#t <- terrain.fun(65)

#' init.plants is a function that creates an array and initiates plants across a terrain
#' @param terrain a matrix and the output of the terrain.fun function
#' @param timesteps the number of times the simulation will run
#' @param names names of the plants being simulated, if null plants are "a", "b", "c", etc.
#' @return an array where columns and rows equal columns and rows of terrain and the third dimension equals with timestep + 1 (an initial matrix, plus one matrix for each timestep)
init.plants <- function(terrain, timesteps, names){
  if(is.null(names)){
    names <- letters[seq_along(repro)]
  }
  plants <- array("", dim=c(dim(terrain), timesteps+1))
  for(i in 1:nrow(terrain)){
    for(j in 1:ncol(terrain)){
      plants[i,j,1] <- sample(names,1)
    }
  }
  #puts NAs into plants array where there is water in terrain
  for(i in seq_len(dim(plants)[timesteps+1])){
    plants[,,i][is.na(terrain)] <- NA
  }
  return(plants)
}

#creating a plant array to work with
#plants <- init.plants(terrain = t, timesteps = 2, names=names)

#' survival function determins the if a plant in a given cell survives each timestep
#' @param cell the cell (row, col, k) in the array (the output from init.plants) that is being tested
#' @param info a list and the output from setup.plants
#' @return returns "" if the plant died and the name of the plant if it survived
survival <- function(cell, info){
  #checks to see if the cell is underwater
  if(is.na(cell)){
    cell <- NA
  }
  #checks to see that the cell isn't empty
  if(cell != ""){
    #checks to see if the plant in the cell survived
    if(runif(1) > info$survival[cell]){
      #plant died, cell is empty
      cell <- ""
    }
  }
  return(cell)
}

#' reproduce determines if and where a plant will reproduce. Limited to adjacent cells. Also includes a competition function if a cell is already occupied
#' @param timesteps the number of times the simulation will run
#' @param row the number of rows in the matrix. Determined from the number of rows in the Plants array
#' @param column the number of columns in the matrix. Determined from the number of columns in the Plants array
#' @param plants an array and output of init.plants function
#' @param info a list and output of the setup.plants function
#' @return returns updated array (timestep) with plant information depending on outcome of simulated repro and competition
reproduce <- function(time, row, column, plants, info){
  #loops through all rows in plants array
  for(p in 1:nrow(plants)){
    #loops through all the plant species
    for(n in 1:length(info$names)){
      #creates a matrix of possible repro locations
      pos.loc <- as.matrix(expand.grid(row+c(-1,0,1), column+c(-1,0,1)))
      #If pos repro loc is out of bounds, populates pos.loc with NA
      pos.loc[pos.loc < 1] <- NA
      pos.loc[pos.loc > dim(plants)[1]] <- NA
      #Deletes all rows containing NAs from pos.loc
      for(i in 9:1){
        if(is.na(pos.loc[i,1]) | is.na(pos.loc[i,2])){
          pos.loc <- pos.loc[-i,]
        }
      }
      #if a cell has a plant in it
      if(plants[row,column,time] == info$names[n]){
        #if the plant reproduced
        if(runif(1) <= info$repro[plants[row, column, time]]){
          #if the possible repro location does not contain NA (i.e., isn't under water)
          if(!is.na(plants[pos.loc[p,1], pos.loc[p,2], time])){
            #if an adjacent cell is a possible repro location and already has a plant in it
            if(plants[pos.loc[p,1], pos.loc[p,2], time] == info$names[n]){
              #if the propagule outcompetes the original plant
              if(runif(1) <= info$comp.matrix[plants[row,column,time], plants[pos.loc[p,1], pos.loc[p,2], time]]){
                #then the cell gets populated with the new plant
                plants[pos.loc[p,1], pos.loc[p,2],time] <- plants[row,column,time]
              }
            }
          }
        }
      }
    }
  }
  return(plants)
}

#' plant.timestep is a wrapper around the survival and reproduce functions. Loops through every row, column, and timestep in the plants array
#' @param plants an array and output of init.plants function
#' @param terrain a matrix and the output of the terrain.fun function
#' @param info a list and output of the setup.plants function
#' @param timesteps the number of times the simulation will run
#' @return a new matrix with the updated plant location info based on the outcome of reproduction, survival, and competition
plant.timestep <- function(plants, terrain, info, timesteps){
  #loops through the timesteps
  for(t in 1:(timesteps+1)){
    #loops through the rows
    for(i in 1:nrow(plants)){
      #loops through the columns
      for(j in 1:ncol(plants)){
        #applies the survival function to each cell in the matrix
        survival(plants[i,j], info)
        reproduce(time=t, row=plants[i,], column=plants[,j], plants, info)
      }
    }
  }
  return(plants)
}

#' run.plant.eco is a function that wraps around the setup.plants, init.plants, and plant.timestep functions
#' @param timesteps the number of times the simulation will run
#' @param terrain a matrix and the output of the terrain.fun function
#' @param repro reproduction probability, must be a repro prob for every plant species, a vector with each value between 0 and 1
#' @param survival survival probability, must be a survival prob for every plant species, a vector with each value between 0 and 1
#' @param comp.matrix competition matrix of survival probabilities between species, rows and columns must be equal to the number of plant species, each cell is populated with a value between 0 and 1
#' @param names names of the plants being simulated, if null plants are "a", "b", "c", etc.
#' @return returns an array with plant locations overtime
#' @export
run.plant.eco <- function(timesteps, terrain, repro, survival, comp.matrix, names=NULL){
  info <- setup.plants(repro=repro, survival=survival, comp.matrix=comp.matrix, names = names)
  plants <- init.plants(terrain = terrain, timesteps = timesteps, names = names)
  output <- plant.timestep(plants=plants, terrain=terrain, info=info, timesteps=timesteps)
  return(output)
}


