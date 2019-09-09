#' Entropy index
#'
#' This function computes the entropy.
#'
#' @param x Factor
#'
#' @return Numeric variable.
#'
#' @examples
#' entropy(iris$Species)
#'
#' @export
entropy <- function(x) {
  y = table(x)

  yy = -(y / sum(y)) *
    log2(y / sum(y)) -
    (1 - (y / sum(y))) *
    log2(1 -(y / sum(y)))

  sum((y / sum(y)) * yy)

}
