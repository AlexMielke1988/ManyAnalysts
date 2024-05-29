# function that z-standardises a variable
z_standardise <- function(variable) {
  return(as.vector(scale(variable, center = TRUE, scale = TRUE)))
}

# function that minmax-standardises a variable
minmax_standardise <- function(variable) {
  return(as.vector((variable - min(variable)) / (max(variable) - min(variable))))
}

# function that centres a variable on 0
centre <- function(variable) {
  return(as.vector(scale(variable, center = TRUE, scale = FALSE)))
}

# function that ranks the values of a variable and then minmax-standardises them
proportional <- function(variable) {
  return(as.vector(rank(variable) / length(variable)))
}

# create a vector with all the function names in this file
# create a switch statement that will run the function that is passed to it
