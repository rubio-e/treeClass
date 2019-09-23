#' Gini index
#'
#' This function computes the Gini index.
#'
#' @param x Factor
#'
#' @return Numeric variable
#'
#' @examples
#' gini(iris$Species)
#'
#' @export
gini <- function(x) {
  # This function implements the Gini index formula
  y <- table(x)

  z <- (y / sum(y)) ^ 2

  gini <- 1 - sum(z)
  
  return(gini)

}
