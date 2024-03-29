#' Entropy index
#'
#' This function computes the entropy.
#'
#' @param x Factor vector
#'
#' @return Numeric variable.
#'
#' @examples
#' entropy(iris$Species)
#'
#' @export
entropy <- function(x) {
  # This uses directly the formula of entropy
  
  y <- table(x)
  
  yy  <-  -(y / sum(y)) * log2(y / sum(y)) - (1 - (y / sum(y))) * log2(1 -(y / sum(y)))
  
  entropy <- sum((y / sum(y)) * yy)
  
  return(entropy)

}
