\name{nom.uncertainty}
\alias{nom.uncertainty}
\title{Calculate the Uncertainty Coefficient (Theil's U)}
\usage{
  nom.uncertainty(x)
}
\arguments{
  \item{x}{A table object.}
}
\value{
  A named list with the three values: \item{ucc.cr}{The row
  variable is used as independent, the column variable as
  dependent variable.} \item{uc.rc}{The column variable is
  used as independent, the row variable as dependent
  variable.} \item{uc.symmetric}{Symmetric uncertainty
  coefficient.}
}
\description{
  Calculate the Uncertainty Coefficient (Theil's U)
}
\note{
  The code for the calculation was supplied by Marc
  Schwartz (under GPL 2). Note: Asymmetric formulae
  denomiators corrected on May 4, 2007 thanks to Antti
  Arppe. Checked against SPSS results.
}
\examples{
{

}
}
\author{
  Marc Schwartz, Mark Heckmann
}

