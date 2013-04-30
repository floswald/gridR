\name{log.grid}
\alias{log.grid}
\title{log scaled grid: more points at upper bound}
\usage{
  log.grid(b, n, plotit)
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
\examples{
gridR(bounds=c(-1,5),num.points=10,spacing="log.grid",plotit=TRUE)
}
\seealso{
  Other grid.makers: \code{\link{beta.grid}},
  \code{\link{expo}}, \code{\link{expo2}},
  \code{\link{gumbel.grid}}, \code{\link{hyp.sine}},
  \code{\link{lognorm.grid}}
}

