#'@title Geometric mean
#'
#'@description get a geometric mean
#'@param x is a numeric value, could be a vector or data.frame
#'@param na.rm remove missing values before computing
#' @param add_1 should 1 be added to \code{x} before taking the log?
#' If so, the exponentiated mean will have 1 subtracted afterwards
#'
#'@export g_mean
#'@keywords g_mean
#'@return a geometric mean
#'@export g_mean
#'@examples
#'x<-rnorm(25,2,3)
#'g_mean(x)
#'
g_mean <- function(x, na.rm = TRUE, add_1 = FALSE)
{
  if (add_1) {
    x = x + 1
  }
  if (is.null(nrow(x))) {
    res = exp(mean(log(x), na.rm = na.rm))
  }
  else {
    res = exp(colMeans(log(x),na.rm = na.rm))
  }
  if (add_1) {
    res = res - 1
  }
  return(res)
}
