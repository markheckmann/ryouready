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


#' Use standard mathematical interval notation in \code{recode} from \code{car} 
#' package
#' 
#' The \code{recode} function from the \code{car} package is an excellent 
#' function for recoding data. When defining open intervals though, the recoding
#' definitions will quickly become hard to read. The \code{intervals} function 
#' allows to use standard mathematical interval notation, e.g. like 
#' \code{[1,4)}, to define (open) intervals. It will convert the intervals 
#' definition into a format required by the \code{recode} function from 
#' \code{car}. The standard intervals can simply be used additionally to the 
#' standard recoding definitions as required by \code{recode}.
#' 
#' @param rec recoding definition as required by the \code{recode} function from
#'   the \code{car} package, additionally allowing for standard mathematical
#'   interval notation. An interval notation consists of two brackets containing
#'   the interval values seperated by a comma. Open and closed intervals may be
#'   defined, e.g. \code{(1,2), [1,2], (1,2], [1,2)}. The tags \code{lo} and
#'   \code{hi} for the highest and lowest value in the dataset may also be used,
#'   e.g. \code{[lo,4], [0,hi)}.
#' @param e Deviation from given interval values when an open interval is used 
#'   (i.e. excluding the given value). The default deviation is \code{10e-8}. 
#'   This means that e.g. the interval \code{(1,2)} is converted into the 
#'   definition \code{1+10e-8:2-10e-8} to be used in the \code{recode} function.
#' @return A string with recoding definitions for intervals as required by \code{recode} from 
#'   \code{car}.
#' @export
#' @author Mark Heckmann
#' @examples \dontrun{
#'  library(car)
#'  
#'  # the standard way if we want to recode [1,2) to the value 3
#'  recode(c(1, 1.999, 2, 2.001), "1:2-1e-4=3")
#'  
#'  # the same using interval notation
#'  intervals("[1,2)=3")
#'  recode(c(1, 1.999, 2, 2.001), intervals("[1,2)=3"))
#'  
#'  # another example: the car way
#'  e <- 10^-8
#'  recode(1:9/3.01, "lo:1-e=0; 1:2-e=1; 2:3-e=2")
#'  # using intervals
#'  recode(1:9/3.01, intervals("[lo,1)=0; [1,2)=1; [2,3)=2"))
#' }
#' 
intervals <- function(rec, e=10^-8)
{
  e <- deparse(e)
  rec.pcs <- str_trim(str_split(rec, ";")[[1]])   # split recode def into pieces
  rec.pcs <- str_replace_all(rec.pcs, " ", "") 
  
  # determine positions of valid interval defs (to really make sure that nothing else gets changed)
  # bracket [high, lo, number] Comma [high, lo, number] bracket 
  i <- str_detect(rec.pcs, "^(\\]|\\(|\\[)(lo|[0-9]*(\\.[0-9]*)?),(hi|[0-9]*(\\.[0-9]*)?)(\\]|\\)|\\[)")
  
  s <- rec.pcs[i]
  # replace open brackets for lower interval
  s <- str_replace(s, "^(\\]|\\()([0-9]*(\\.[0-9]*)?|lo),", paste0("(\\2+", e, ")@"))
  # replace closed brackets for lower interval
  s <- str_replace(s, "^(\\[)([0-9]*(\\.[0-9]*)?|lo),", "\\2@")
  
  # replace open brackets for upper interval
  s <- str_replace(s, "@([0-9]*(\\.[0-9]*)?|hi)(\\[|\\))", paste0(":(\\1-", e, ")"))
  # replace closed brackets for upper interval
  s <- str_replace(s, "@([0-9]*(\\.[0-9]*)?|hi)(\\])", ":\\1")
  
  rec.pcs[i] <- s
  paste(rec.pcs, collapse="; ")
}

