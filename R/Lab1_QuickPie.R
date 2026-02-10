#' Create a Pie Plot
#'
#' @param value A numeric vector of values for Pie Chart
#'
#' @returns Pie Plot
#' @export
#'
#' @examples \dontrun {# Pie Chart of Fish SPECIES pie(tabFish)}
Lab1_QuickPie <- function(value) {

  p <- pie(value,  col=rainbow(length(value)))

  return(p)
}
