#' Create a boxplot
#'
#'
#'@param values Create a vector of values
#'
#'@return Boxplot
#'@export
Lab1_QuickBox <- function(values){

  b <- boxplot(values, col="blue")

  return(b)
}
