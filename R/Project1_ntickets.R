#' Overbooking Problem (Number of tickets to sell a flight)
#'
#' @param N N - Number of seats on the plane
#' @param gamma Gamma - acceptable probability for overbooking
#' @param p p - probability that a ticket of a person will show up
#'
#' @returns returns Discrete and Continuous Plots of the overbooking problem
#' @export
#' @examples
#' ntickets(100, 0.05, 0.9)
ntickets <- function(N, gamma, p) {
  ticket_range = seq(N, floor(N + N/10), by = 1)

  # Discrete Distribution
  discrete = 1 - gamma - pbinom(N, ticket_range, p)
  optimal_discrete <- which.min(abs(discrete)) # Optimal number of tickets for discrete

  # Continuous Approximation
  continuous <- function(x) { # x - number of tickets
    mean_show <- x * p
    sd_show <- sqrt(x * p * (1 - p))
    1 - gamma - pnorm(q = N + 0.5, mean = mean_show, sd = sd_show)
  }

  root_result = stats::uniroot(continuous, c(N, floor(N + N/10)))


  # Plotting format
  layout(matrix(1:2, nrow = 2, ncol = 1))

  # Plot Discrete Distribution
  plot(x = ticket_range, y = discrete, xlab = "Number of Tickets (n)", ylab = "Objective Function Value", main = paste("Objective vs n to find optimal tickets sold (Discrete Distrubution)"), cex.main = 0.8, col = "green", pch = 19)

  # Add values and parameters
  mtext(paste("Optimal n = ", ticket_range[optimal_discrete],
              ", gamma = ", gamma,
              ", Method: Discrete"),
        cex = 0.8)

  # Create reference line for optimal point
  abline(h = discrete[optimal_discrete], v = ticket_range[optimal_discrete], col = "blue")

  # Plot Continuous
  curve(continuous, xlim = c(N, N + floor(N/10)), xlab = "Number of tickets (n)", ylab = "Objective Function Value", main = paste("Objective vs n to find optimal tickets sold (Continuous Approximation)"), cex.main = 0.8)

  # Show optimal values and parameters
  mtext(paste("Optimal n = ", round(root_result$root, 4),
              ", gamma = ", gamma,
              ", N = ", N,
              ", Method: Continuous"),
        cex = 0.8)


  # Add reference line for optimal point
  abline(h = continuous(root_result$root), v = root_result$root, col = "purple")

  # Return results
  list(nd = ticket_range[optimal_discrete], # Discrete
       nc = round(root_result$root, 4), # Continuous
       N = N, gamma = gamma, p = p) # Parameters
}
