#' Negative Binomial Distribution
#'
#' @param y Number of fails
#' @param r The target number of successes
#' @param p Probability of Success in trials
#'
#' @returns A decimal number of a probability
#' @export
Lab5_mynbin <- function(y, r, p) {
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}



