#' Plot Residual vs Fitted Value
#'
#' @param y y Variable
#' @param x x Variable
#' @param data Dataset used for plot
#'
#' @returns Residual vs Fitted Values Plot
#' @export
#' @examples
#' Lab4_ResidvsFittedPlot(mtcars$wt, mtcars$mpg, mtcars)
Lab4_ResidvsFittedPlot <- function(x, y, data) {

  # Create linear model
  data <- with(data, lm(y~x))

  residual <- residuals(data)
  fitted <- fitted(data)

  plot(fitted, residual) # Create Residual vs Fitted Plot
}
