\name{hyp.sine}
\alias{hyp.sine}
\title{hyperbolic sine scaling towards zero}
\usage{
  hyp.sine(b, n, plotit)
}
\arguments{
  \item{b}{numeric vector of length 2. bounds.}

  \item{n}{number of desired points}

  \item{plotit}{boolean TRUE if want a plot of result}
}
\value{
  numeric vector of gridpoints
}
\description{
  increase concentration of points symmetrically around
  zero.
}
\examples{
gridR(bounds=c(-10,10),num.points=20,spacing="hyp.sine",plotit=TRUE)
}
\seealso{
  Other grid.makers: \code{\link{beta.grid}},
  \code{\link{expo}}, \code{\link{expo2}},
  \code{\link{gumbel.grid}}, \code{\link{log.grid}},
  \code{\link{lognorm.grid}}
}

