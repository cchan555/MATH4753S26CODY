#' Find the z-score
#'
#' @param values A list of vectors of values
#'
#' @return z-score vector
#' @export
Lab2_zScore <- function(values) {
  z <- (values - mean(values)) / sd(values)
}
