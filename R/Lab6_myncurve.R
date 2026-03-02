#' Creates density plot using dnorm when given mean and standard deviation
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a cutoff value
#'
#' @returns Returns density plot with area colored in
#' @export

Lab6_myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  # x values corresponding to the x - cords of points on the curve
  xcurve = seq(mu-3*sigma, a, length = 1000)
  # Y values corresponding to the x values
  ycurve = dnorm(xcurve, mu, sigma)

  # Fill in the polygon with the given vertices
  polygon(c(-mu, xcurve, a), c(0, ycurve, 0), col="green")
  # Area
  prob= round(pnorm(a, mu, sigma), 4)
  text(x = a, y = 0.5*dnorm(a, mu, sigma), paste0("Area =", prob))

  list(mu = mu, sigma = sigma, a = a)

}
