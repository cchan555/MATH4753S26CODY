#' Title
#'
#' @param x
#' @param na.rm
#'
#' @returns
#' @export
#'
#' @examples
mysd <- function(x, na.rm = TRUE) {
    if(na.rm) {
      x<- x[!is.na(x)]
    }
  sqrt(var(x))
}
