\name{gridR.log.grid}
\alias{gridR.log.grid}
\title{log scaled grid: more points at upper bound}
\usage{
  gridR.log.grid(b, n, plotit)
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
  concentrate points towards the upper bound.
}
\seealso{
  Other grid.makers: \code{\link{gridR.beta}},
  \code{\link{gridR.expo}}, \code{\link{gridR.expo2}},
  \code{\link{gridR.gumbel}}, \code{\link{gridR.hyp.sine}},
  \code{\link{gridR.lognorm}}
}

