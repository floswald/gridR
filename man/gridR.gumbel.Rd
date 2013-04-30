\name{gridR.gumbel}
\alias{gridR.gumbel}
\title{points distributed according to gumbel pdf}
\usage{
  gridR.gumbel(b, n, plotit, ...)
}
\arguments{
  \item{b}{numeric vector of length 2. bounds.}

  \item{n}{number of desired points}

  \item{plotit}{boolean TRUE if want a plot of result}

  \item{loc}{location parameter}

  \item{scale}{parameter}
}
\value{
  numeric vector of gridpoints
}
\description{
  distribute points according to the gumbel density. use
  location and scale parameter to change shape of
  distribution.
}
\seealso{
  \link[evd]{pgumbel}

  Other grid.makers: \code{\link{gridR.beta}},
  \code{\link{gridR.expo}}, \code{\link{gridR.expo2}},
  \code{\link{gridR.hyp.sine}},
  \code{\link{gridR.log.grid}}, \code{\link{gridR.lognorm}}
}

