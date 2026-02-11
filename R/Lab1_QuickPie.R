#' Create a Pie Plot
#'
#' @param values A numeric vector of values for Pie Chart
#'
#' @returns Pie Plot
#' @export
Lab1_QuickPie <- function(value) {

  p <- pie(value,  col=rainbow(length(value)))

  return(p)
}
