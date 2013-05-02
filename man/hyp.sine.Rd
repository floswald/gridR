\name{hyp.sine}
\alias{hyp.sine}
\title{hyperbolic sine scaling towards zero: internal function}
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
grid.maker(bounds=c(-10,10),num.points=20,spacing="hyp.sine",plotit=TRUE)
}
\seealso{
  Other grid.makers: \code{\link{beta.grid}},
  \code{\link{exp.grid}}, \code{\link{gumbel.grid}},
  \code{\link{log.g}}, \code{\link{log.g2}},
  \code{\link{lognorm.grid}}
}

