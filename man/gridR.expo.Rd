\name{gridR.expo}
\alias{gridR.expo}
\title{exponentially scaled grid}
\usage{
  gridR.expo(b, n, plotit)
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
  make a grid with more points towards the lower bound.
  adjusts to negative lower bound as well.
}
\seealso{
  Other grid.makers: \code{\link{gridR.beta}},
  \code{\link{gridR.expo2}}, \code{\link{gridR.gumbel}},
  \code{\link{gridR.hyp.sine}},
  \code{\link{gridR.log.grid}}, \code{\link{gridR.lognorm}}
}

