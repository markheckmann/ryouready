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


#' Form row means taking into account a minimum number of values required
#' 
#' In the construction of psychometric scales the calculation of a value is
#' sometimes only desired if a minimum number of items contain values. In SPSS
#' it is possible to calculate a mean value only if a minimum number of values
#' are supplied by using the syntax MEAN.MIN with MIN being a numeric value. The
#' function \code{rowMeans2} does the same.
#' 
#' \code{rowMeans2} is very similary to \code{rowMeans}. The differences are
#' that \code{rowMeans2} allows to indicate the minimum number of values that
#' have to be supplied and to weight the columns.
#' 
#' @seealso \code{\link{rowMeans}}
#' @param x A matrix of dataframe whose columns should be averaged.
#' @param w A numerical vector of weights the same length as number of columns 
#'   in \code{x}.
#' @param min The minimum number of values required to calculate the mean value.
#'   Otherwise return \code{NA}.
#' @param na.rm A logical value indicating whether \code{NA} values in \code{x} 
#'   should be stripped before the computation proceeds.
#' @return A vector of means.
#' @export
#' @author Mark Heckmann
#' @examples 
#'  x <- replicate(3, runif(5))
#'  x[1:3, 1] <- NA       # add NAs to data
#'  x[1:2, 2] <- NA
#'  x[1, 3] <- NA
#'  x
#'  rowMeans2(x)          # the same as rowMeans, except that NAs are allowed
#'  rowMeans2(x, min=2)   # minimum two values to calculate mean
#'  rowMeans2(x, min=3)   # minimum three values to calculate mean
#'  
#'  # returns numeric(0) if x has zero rows 
#'  d <- x[NULL, ]
#'  rowMeans2(d)
#'  
#'  # weights for each column
#'  rowMeans2(x, w=c(1,1,2))
#' 
rowMeans2 <- function(x, w, min=0, na.rm=TRUE)
{ 
  if (missing(w))
    w <- rep(1, ncol(x))        # if no weights are supplied give equal weights to all columns
  if (min > ncol(x))
    warning("min smaller than columns in x. All values are NA", call.=FALSE)
  r <- apply(x, 1, function(x) {
    n <- sum(!is.na(x))
    if (n < min)                  # if number of values is smaller min, NA is returned
      return(NA)
    weighted.mean(x=x, w=w, na.rm=na.rm)
  }) 
  is.na(r) <- is.na(r)            # prevent NaN in case alle entries are NA
  r
}



