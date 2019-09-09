#' purity function
#'
#' This function defines if a vector is pure
#'
#' @param x Factor vector
#'
#' @return string
#'
#' @examples
#' purity(iris$Species)
#'
#' @export
purity <- function(x) {
  y = ifelse(length(unique(x)) == 1, "pure", "impure")
  y
}
