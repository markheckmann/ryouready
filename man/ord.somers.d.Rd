\name{ord.somers.d}
\alias{ord.somers.d}
\title{Calculate Somers' d for ordinal data tables.}
\usage{
  ord.somers.d(x)
}
\arguments{
  \item{x}{A table object.}
}
\value{
  Kendall's Tau-b value.

  A named list with the three values: \item{sd.cr}{The row
  variable is used as independent, the column variable as
  dependent variable.} \item{sd.rc}{The column variable is
  used as independent, the row variable as dependent
  variable.} \item{sd.symmetric}{Symmetric Somers' d.}
}
\description{
  Calculate Somers' d for ordinal data tables.
}
\note{
  The code for the calculation was supplied by Marc
  Schwartz (under GPL 2)
}
\examples{
{
 # TODO
}
}
\author{
  Marc Schwartz, Mark Heckmann
}

