#' @keywords internal
purity <- function(x) {
  # to define if a node is pure or not
  y <- ifelse(length(unique(x)) == 1, "pure", "impure")
  
  return(y)
  
}
