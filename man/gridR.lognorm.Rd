\name{gridR.lognorm}
\alias{gridR.lognorm}
\title{points distributed according to log normal}
\usage{
  gridR.lognorm(b, n, plotit, ...)
}
\arguments{
  \item{b}{numeric vector of length 2. bounds.}

  \item{n}{number of desired points}

  \item{plotit}{boolean TRUE if want a plot of result}

  \item{meanlog}{mean of log normal}

  \item{sdlog}{standard deviation of log normal}
}
\value{
  numeric vector of gridpoints
}
\description{
  distribute points according to the log normal density.
  only for points in positive range.
}
\seealso{
  \link[stats]{dlnorm}

  Other grid.makers: \code{\link{gridR.beta}},
  \code{\link{gridR.expo}}, \code{\link{gridR.expo2}},
  \code{\link{gridR.gumbel}}, \code{\link{gridR.hyp.sine}},
  \code{\link{gridR.log.grid}}
}

