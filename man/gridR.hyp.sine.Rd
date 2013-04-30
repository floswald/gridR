\name{gridR.hyp.sine}
\alias{gridR.hyp.sine}
\title{hyperbolic sine scaling towards zero}
\usage{
  gridR.hyp.sine(b, n, plotit)
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
\seealso{
  Other grid.makers: \code{\link{gridR.beta}},
  \code{\link{gridR.expo}}, \code{\link{gridR.expo2}},
  \code{\link{gridR.gumbel}}, \code{\link{gridR.log.grid}},
  \code{\link{gridR.lognorm}}
}

