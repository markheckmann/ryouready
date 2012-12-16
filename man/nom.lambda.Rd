\name{nom.lambda}
\alias{nom.lambda}
\title{Calculate Lambda for nominal data tables.}
\usage{
  nom.lambda(x)
}
\arguments{
  \item{x}{A table object.}
}
\value{
  A named list with the three values: \item{lambda.cr}{The
  row variable is used as independent, the column variable
  as dependent variable.} \item{lambda.rc}{The column
  variable is used as independent, the row variable as
  dependent variable.} \item{lambda.symmetric}{Symmetric
  Lambda (the mean of both above).}
}
\description{
  Calculate Lambda for nominal data tables.
}
\note{
  The code for the calculation was supplied by Marc
  Schwartz (under GPL 2). Checked against SPSS results.
}
\examples{
{

}
}
\author{
  Marc Schwartz, Mark Heckmann
}

