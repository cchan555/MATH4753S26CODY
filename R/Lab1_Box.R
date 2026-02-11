#' Create a boxplot
#'
#'
#'@param values Create a vector of values
#'
#'@return Boxplot of value given with color blue
#'@export
Lab1_Box <- function(values){

  b <- boxplot(values, col="blue")

  return(b)
}
