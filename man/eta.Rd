\name{eta}
\alias{eta}
\title{Eta coefficient for nominal/interval data.}
\usage{
  eta(x, y, breaks = NULL, ...)
}
\arguments{
  \item{x}{Independent nominal variable (factor or
  numeric).}

  \item{y}{Dependent interval variable (numeric).}

  \item{breaks}{If \code{x} is interval data the
  \code{breaks} argument can be specified to classify the
  data. \code{breaks} is passed on to the function
  \code{\link{cut}}.}
}
\value{
  Eta coefficient
}
\description{
  Eta coefficient for nominal/interval data.
}
\examples{
attach(eta2)     # using eta2 dataset
eta(x1, y)

# classify interval data x
eta(x, y, breaks=c(1, 4, 7,10))
# visualize classication
plot(x, y)
abline(v=c(1, 4, 7,10))

# setting number of breaks for classification
eta(x, y, breaks=7)
}
\author{
  Mark Heckmann
}

