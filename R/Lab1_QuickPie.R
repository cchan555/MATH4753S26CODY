#' Create a Pie Plpt
#'
#' @param value
#'
#' @returns Pie Plot
#' @export
#'
#' @examples
#' \dontrun {# Pie Chart of Fish SPECIES}
#' \dontrun {pie(tabFish)}
Lab1_QuickPie <- function(value) {

  p <- pie(value,  col=rainbow(length(value)))

  return(p)
}
