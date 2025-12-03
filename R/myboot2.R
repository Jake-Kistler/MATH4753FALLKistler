#' Bootstrap Confidence Interval
#'
#' This function uses the bootstrap to estimate a confidence interval for a
#' statistic of interest. It resamples the data, computes the statistic for each
#' resample, and plots the bootstrap distribution with the CI and point estimate.
#'
#' @param iter Number of bootstrap iterations. Default is 10000.
#' @param x Numeric sample to bootstrap.
#' @param fun Statistic to compute (e.g. "mean", "sd", median, IQR).
#'            Default is "mean".
#' @param alpha Significance level. Default is 0.05 for a 95\% CI.
#' @param cx Text size for plot labels. Default is 1.5.
#' @param ... Extra arguments passed to \code{hist()}.
#'
#' @return Invisibly returns a list with the confidence interval, the statistic used,
#'         and the original data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   set.seed(1)
#'   sam <- rnorm(20, mean = 10, sd = 4)
#'   myboot2(x = sam, fun = "mean")
#' }

myboot2 <- function(iter=10000, x, fun="mean", alpha=0.05, cx=1.5, ...)
{
  n = length(x)

  y = sample(x, n * iter, replace = TRUE)
  rs.mat = matrix(y, nr = n, nc = iter, byrow = TRUE)

  xstat = apply(rs.mat, 2, fun)

  ci = quantile(xstat, c(alpha/2, 1 - alpha/2))

  para = hist(xstat, freq = FALSE, las = 1,
              main=paste("Histogram of Bootstrap sample statistics",
                         "\n", "alpha=",alpha," iter=",iter,sep=""),
              ...)

  mat = matrix(x, nr=n, nc=1, byrow=TRUE)

  pte = apply(mat, 2, fun)

  abline(v=pte, lwd=3, col="Black")

  segments(ci[1],0,ci[2],0,lwd=4)

  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  text(pte, max(para$density)/2, round(pte,2), cex=cx)

  invisible(list(ci=ci, fun=fun, x=x))
}
