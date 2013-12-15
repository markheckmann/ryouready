\name{intervals}
\alias{intervals}
\title{Use standard mathematical interval notation in \code{recode} from \code{car}
package}
\usage{
  intervals(rec, e = 10^-8)
}
\arguments{
  \item{rec}{recoding definition as required by the
  \code{recode} function from the \code{car} package,
  additionally allowing for standard mathematical interval
  notation. An interval notation consists of two brackets
  containing the interval values seperated by a comma. Open
  and closed intervals may be defined, e.g. \code{(1,2),
  [1,2], (1,2], [1,2)}. The tags \code{lo} and \code{hi}
  for the highest and lowest value in the dataset may also
  be used, e.g. \code{[lo,4], [0,hi)}.}

  \item{e}{Deviation from given interval values when an
  open interval is used (i.e. excluding the given value).
  The default deviation is \code{10e-8}.  This means that
  e.g. the interval \code{(1,2)} is converted into the
  definition \code{1+10e-8:2-10e-8} to be used in the
  \code{recode} function.}
}
\value{
  A string with recoding definitions for intervals as
  required by \code{recode} from \code{car}.
}
\description{
  The \code{recode} function from the \code{car} package is
  an excellent function for recoding data. When defining
  open intervals though, the recoding definitions will
  quickly become hard to read. The \code{intervals}
  function allows to use standard mathematical interval
  notation, e.g. like \code{[1,4)}, to define (open)
  intervals. It will convert the intervals definition into
  a format required by the \code{recode} function from
  \code{car}. The standard intervals can simply be used
  additionally to the standard recoding definitions as
  required by \code{recode}.
}
\examples{
\dontrun{
 library(car)

 # the standard way if we want to recode [1,2) to the value 3
 recode(c(1, 1.999, 2, 2.001), "1:2-1e-4=3")

 # the same using interval notation
 intervals("[1,2)=3")
 recode(c(1, 1.999, 2, 2.001), intervals("[1,2)=3"))

 # another example: the car way
 e <- 10^-8
 recode(1:9/3.01, "lo:1-e=0; 1:2-e=1; 2:3-e=2")
 # using intervals
 recode(1:9/3.01, intervals("[lo,1)=0; [1,2)=1; [2,3)=2"))
}
}
\author{
  Mark Heckmann
}

