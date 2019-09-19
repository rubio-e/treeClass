#' @keywords internal
purity <- function(x) {
  
  y <- ifelse(length(unique(x)) == 1, "pure", "impure")
  
  return(y)
  
}
