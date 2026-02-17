#' Title My Standard Deviation Function
#'
#' @param x A quantitative vector
#' @param na.rm A Logical indicating whether to remove NA value before computation
#' @importFrom stats var
#'
#' @returns A quantitative vector with square (SD of the numeric vector)
#' @export
#'
#'@examples
#' mysd(c(1, 2, 3, 4, 5))
mysd <- function(x, na.rm = TRUE) {
    if(na.rm) {
      x <- x[!is.na(x)]
    }
  sqrt(var(x))
}
