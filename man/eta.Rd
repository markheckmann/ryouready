\name{eta}
\alias{eta}
\title{Eta and Eta-squared for nominal/interval data.}
\usage{
  eta(x, y, breaks = NULL, ...)
}
\arguments{
  \item{x}{Independent nominal variable (factor or
  numeric).}

  \item{x}{Independent nominal variable (factor or
  numeric).}
}
\value{
  Eta coefficient
}
\description{
  Eta and Eta-squared for nominal/interval data.
}
\examples{
{
 attach(eta2)
 eta(x1, y)

 # classify interval data x
 eta(x, y, breaks=c(1, 4, 7,10))
 # visualize classication
 plot(x, y)
 abline(v=c(1, 4, 7,10))
}
}
\author{
  Mark Heckmann
}

