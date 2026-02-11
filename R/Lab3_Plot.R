#' Creates a plot with blue dots
#'
#' @param xVariable x-axis variables
#' @param yVariable y-axix variables
#' @param xlab x label
#' @param ylab y label
#' @param title title of the plot
#' @param dataType dataset being called in
#'
#' @returns returns the plot
#' @export
Lab3_Plot <- function(xVariable, yVariable, xlabel, ylabal, title, datatype) {
  p <- plot(x = xVariable, y = yVariable, xlab = xlabel, ylab = ylabel,
        main = "Spruce Height Prediction", data = datatype,
        bg = "blue", pch = 21, cex = 1.2, ylim = c(0, 1.1*max(yVariable)),
        xlim = c(0, 1.1*max(xVariable)))
  return (p)
}

