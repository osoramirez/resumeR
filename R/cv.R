#'@title coefficient of variation
#'
#'@description Compute the coefficient of variation
#'@param x is a numeric value, could be a  a vector or data.frame
#'@export cv
#'@keywords cv
#'@return A single number, a scalar
#'@export cv
#'
#'@examples
#'set.seed(12345)
#'x<-rnorm(25,2,3)
#'cv(x)
#'

cv <- function(x)
{
  dig = getOption("digits")
  options(digits = 3)
  on.exit({
    options(digits = dig)
  })
  # don't need na.omit if na.rm = TRUE
  sqrt(var(x,na.rm = TRUE))/mean(x,na.rm=TRUE)
  }


