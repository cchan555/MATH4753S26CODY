#' Creates a plot with blue dots with limits
#'
#' @param xVariable x-axis variables
#' @param yVariable y-axix variables
#' @param xlabel x label
#' @param ylabel y label
#' @param title title of the plot
#'
#' @returns returns the plot
#' @export
Lab3_Plot <- function(xVariable, yVariable, xlabel, ylabel, title) {
  plot(x = xVariable, y = yVariable, xlab = xlabel, ylab = ylabel, main = title,
       bg = "blue", pch = 21, cex = 1.2, ylim = c(0, 1.1*max(yVariable)),
       xlim = c(0, 1.1*max(xVariable)))
}
