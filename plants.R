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
init.plants <- function(terrain.matrix, timesteps, names){
  plants <- array("", dim=c(dim(terrain.matrix), timesteps+1))
  for(i in nrow(t)){
    for(j in ncol(t)){
      plants[i,j,1] <- sample(names,1)
    }
  }
  #puts NAs into plants array where there is water in terrain
  for(i in seq_len(dim(plants)[timesteps+1])){
    plants[,,i][is.na(terrain.matrix)] <- NA
  }
  return(plants)
}

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
plant.timestep <- function(plants, terrain, info){
  #loops through the rows
  for(i in nrow(plants)){
    #loops through the columns
    for(j in ncol(plants)){
      #applies the survival function to each cell in the matrix
      survival(plants[i,j], info)
      reproduce(row=plants[i,], column=plants[,j], plants, info)
    }
  }
  return(new.plants.matrix)
}

#################################################### REPRO AND COMPETITION NEEDS WORK ############
reproduce <- function(row, column, plants, info){
  pos.loc <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  
}
  
############################################ WORK ON WRAPPER TO SIMULATE PLANTS ON TERRAIN #######
run.plant.eco <- function(terrain, repro, survive, comp.mat, names=NULL){
  # include the plant array, survival, reproduction, competition, and plant.timestep functions
}
  
  
  
  
