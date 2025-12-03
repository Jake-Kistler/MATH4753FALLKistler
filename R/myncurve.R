#' Plot a normal curve and shade P(X ≤ a)
#'
#' Draws the N(mu, sigma^2) density, shades the area from -Inf to `a`,
#' and returns the probability P(X ≤ a).
#'
#' @param mu    Numeric scalar, the mean.
#' @param sigma Positive numeric scalar, the standard deviation.
#' @param a     Numeric scalar cutoff where the shaded region ends.
#'
#' @return A named list with components:
#'   \item{mu}{the mean used}
#'   \item{sigma}{the standard deviation used}
#'   \item{prob}{P(X ≤ a) for N(mu, sigma^2)}
#'
#' @examples
#' myncurve(10, 5, 6)
#' @export
myncurve <- function(mu, sigma, a)
{
  # probability to return
  prob <- pnorm(a, mean = mu, sd = sigma)

  # x-range for plotting
  xlim <- c(mu - 4 * sigma, mu + 4 * sigma)

  # base curve
  curve(dnorm(x, mean = mu, sd = sigma),
        from = xlim[1], to = xlim[2],
        xlab = "x", ylab = "Density",
        main = sprintf("N(%g, %g^2): shaded P(X \u2264 %g)", mu, sigma, a),
        lwd = 2)

  # shade from left edge up to a (clamped to panel)
  a_clamped <- max(xlim[1], min(a, xlim[2]))

  if (a_clamped > xlim[1])
  {
    xs <- seq(xlim[1], a_clamped, length.out = 500)
    ys <- dnorm(xs, mean = mu, sd = sigma)

    polygon(c(xlim[1], xs, a_clamped), c(0, ys, 0), col = "grey85", border = NA)
    curve(dnorm(x, mean = mu, sd = sigma),
          from = xlim[1], to = xlim[2], add = TRUE, lwd = 2)
  }

  abline(v = a, lty = 3)
  mtext(sprintf("P = %.4f", prob), side = 3, adj = 1, line = 0.5)

  # return exactly the three fields
  list(mu = mu, sigma = sigma, prob = prob)
}
