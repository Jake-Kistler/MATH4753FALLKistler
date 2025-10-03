#' Title
#'
#' @param x an integer that is the size of the group to sample
#'
#' @returns a number which is the probability of at least one shared birthday
#' @export
#' @examples
#' birthday(10)
#' birthday(40)
birthday <- function(x)
{
  # log P(all birthdays distinct) = log(365! / ((365-x)! * 365^x))
  L <- lchoose(365, x) + lfactorial(x) - x * log(365)
  1 - exp(L)  # complement => P(at least one shared birthday)
}

