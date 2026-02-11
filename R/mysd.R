#' Title
#'
#' @param x A quantitative vector
#' @param na.rm A Value
#'
#' @returns A quantitative vector with square
#' @export
#'
mysd <- function(x, na.rm = TRUE) {
    if(na.rm) {
      x<- x[!is.na(x)]
    }
  sqrt(var(x))
}
