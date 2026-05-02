#' Create a boxplot
#'
#'
#'@param values Create a vector of values
#'
#'@return Boxplot of value given with color blue
#'@export
#' @examples
#' x <- c(1, 2, 3, 4, 5, 6, 7)
#' Lab1_Box(x)
Lab1_Box <- function(values){
  boxplot(values, col="blue")
}
