#' A function to demonstrate how an R function is defined.
#' 
#' @param x  Numeric.
#' @param y  Numeric.
#' @return The sum of \code{x} and \code{y}.
#' @author Mark Heckmann
#' @export    
#'
foo <- function(x, y){
  res <- x + y
  return(res)
}