#' Title
#'
#' @param x A quantitative vector
#' @param na.rm A Value
#'
#' @returns A quantitative vector with square
#' @export
#'
#' @examples \dontrun {mysd(1:10)}
mysd <- function(x, na.rm = TRUE) {
    if(na.rm) {
      x<- x[!is.na(x)]
    }
  sqrt(var(x))
}
