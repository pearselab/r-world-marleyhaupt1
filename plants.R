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