#' Central Limit Theorem for the Poisson Distribution
#'
#' This function demonstrates the Central Limit Theorem (CLT) using samples
#' from a Poisson distribution. It generates repeated random samples, computes
#' their means, and displays how the distribution of these means approaches a
#' normal distribution as the sample size increases.
#'
#' @param n Integer. Sample size per iteration.
#' @param iter Integer. Number of iterations (simulations) to run.
#' @param lambda Numeric. The Poisson rate parameter (mean and variance of the Poisson).
#' @param ... Additional graphical parameters passed to the plotting functions.
#'
#' @return Produces three plots:
#'   \itemize{
#'     \item Histogram of sample means with theoretical normal curve overlay
#'     \item Bar plot of sampled values
#'     \item Poisson probability function
#'   }
#'   Returns (invisibly) a vector of sample means.
#'
#' @examples
#' mycltp(n = 10, iter = 10000, lambda = 4)
#'
#' @export
mycltp = function(n, iter, lambda = 10, ...)
{
  ## Generate n × iter Poisson samples
  y = rpois(n * iter, lambda = lambda)

  ## Reshape into a matrix with n rows and iter columns
  data = matrix(y, nr = n, nc = iter, byrow = TRUE)

  ## Compute sample means for each column
  w = apply(data, 2, mean)

  ## Set up histogram parameters
  param = hist(w, plot = FALSE)
  ymax = 1.1 * max(param$density)

  ## Define color-blind-safe palette (Okabe–Ito)
  safe_cols = c("#0072B2", "#E69F00", "#56B4E9", "#CC79A7", "#F0E442", "#009E73")

  ## Layout for multiple plots
  layout(matrix(c(1, 1, 2, 3), nr = 2, nc = 2, byrow = TRUE))

  ## Plot sampling distribution of means
  hist(w, freq = FALSE, ylim = c(0, ymax),
       col = "#56B4E9", border = "white",
       main = paste("Sampling Distribution of Sample Mean\nn=", n,
                    ", iter=", iter, ", lamdba", lambda, sep=""),
       xlab = "Sample Mean", ...)

  ## Add theoretical normal curve (CLT approximation)
  curve(dnorm(x, mean = lambda, sd = sqrt(lambda / n)),
        add = TRUE, col = "#E69F00", lty = 2, lwd = 3)

  ## Add bar plot of sampled y
  barplot(table(y)/(n * iter), col = safe_cols, border = NA,
          main = "Relative Frequency of Sampled y",
          ylab = "Rel. Freq", xlab = "y")

  ## Add Poisson probability function
  x = 0:max(y)
  plot(x, dpois(x, lambda = lambda), type = "h", lwd = 4,
       col = "#0072B2",
       main = "Poisson Probability Function",
       ylab = "Probability", xlab = "y")

  invisible(w)
}
