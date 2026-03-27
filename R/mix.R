#' Mixture Density Function
#'
#' Density of a 2-component normal mixture
#'
#' @param x numeric vector
#' @param d1,d2 means
#' @param s1,s2 standard deviations
#' @param w mixing weight (0 < w < 1)
#' @param log logical
#'
#' @return numeric vector
#' @export
dmix <- function(x, d1, s1, d2, s2, w, log = FALSE) {
  dmix_cpp(x, d1, s1, d2, s2, w, log)
}

#' Mixture CDF
#' @inheritParams dmix
#' @param q quantiles
#' @export
pmix <- function(q, d1, s1, d2, s2, w) {
  pmix_cpp(q, d1, s1, d2, s2, w)
}

#' Mixture Quantile Function
#' @inheritParams dmix
#' @param p probabilities
#' @export
qmix <- function(p, d1, s1, d2, s2, w) {
  qmix_cpp(p, d1, s1, d2, s2, w)
}

#' Random Generation
#' @inheritParams dmix
#' @param n sample size
#' @export
rmix <- function(n, d1, s1, d2, s2, w) {
  rmix_cpp(n, d1, s1, d2, s2, w)
}
