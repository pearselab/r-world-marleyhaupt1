# Plants are Cool!

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
  return(list(repro=repro, survival=survival, comp.matrix=comp.matrix))
}

#creating some initial parameters to test
r <- c(0.5,0.5)
s <- c(0.7, 0.6)
cm <- matrix(data=c(0.9, 0.7, 0.3, 0.5), 2, 2)
names <- c("Bill", "Ted")
info <- setup.plants(repro = r, survival = s, comp.matrix = cm, names = names)
t <- terrain.fun(65)

#initiate plants on the terrain
init.plants <- function(terrain, timesteps, names=NULL){
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
plants <- init.plants(terrain = t, timesteps = 2, names=names)

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

########################################################### NEED TO WORK ON PLANT.TIMESTEP ########
 

#################################################### REPRO AND COMPETITION NEEDS WORK ############
reproduce <- function(row, column, plants, info){
  pos.loc <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  
}

plant.timestep <- function(plants, terrain, info, timesteps){
  #loops through the timesteps
  for(k in 1:(timesteps+1)){
    #loops through the rows
    for(i in 1:nrow(plants)){
      #loops through the columns
      for(j in 1:ncol(plants)){
        #applies the survival function to each cell in the matrix
        survival(plants[i,j], info)
        reproduce(row=plants[i,], column=plants[,j], plants, info)
      }
    }
  }
  return(new.plants.matrix)
}
  
run.plant.eco <- function(timesteps, terrain, repro, survival, comp.matrix, names=NULL){
  info <- setup.plants(repro=repro, survival=survival, comp.matrix=comp.matrix, names = names)
  plants <- init.plants(terrain = terrain, timesteps = timesteps, names = names)
  output <- plant.timestep(plants=plants, terrain=terrain, info=info, timesteps=timesteps)
  return(output)
}
  
  
  
  
