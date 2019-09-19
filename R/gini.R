#' GINI index
#'
#' This function computes the GINI index.
#'
#' @param x Factor
#'
#' @return Numeric variable.
#'
#' @examples
#' gini(iris$Species)
#'
#' @export
gini <- function(x) {

  y <- table(x)

  z <- (y / sum(y)) ^ 2

  gini <- 1 - sum(z)
  
  return(gini)

}
