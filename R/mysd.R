#' Title
#'
#' @param x A quantitative vector
#' @param na.rm
#'
#' @returns A quantitative vector with squre
#' @export
#'
#' @examples \dontrun mysq(1:10)
mysd <- function(x, na.rm = TRUE) {
    if(na.rm) {
      x<- x[!is.na(x)]
    }
  sqrt(var(x))
}
