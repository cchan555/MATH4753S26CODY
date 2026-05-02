#' Find the z-score
#'
#' @param values A list of vectors of values
#'
#' @return z-score vector
#' @export
#' @examples
#' z <- c(1, 2, 3, 4)
#' Lab2_zScore(z)
Lab2_zScore <- function(values) {
  z <- (values - mean(values)) / sd(values)
  return(z);
}
