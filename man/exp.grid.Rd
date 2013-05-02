\name{exp.grid}
\alias{exp.grid}
\title{exponentially scaled grid: more points at upper bound: internal function}
\usage{
  exp.grid(b, n, plotit)
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
grid.maker(bounds=c(-1,5),num.points=10,spacing="exp.grid",plotit=TRUE)
}
\seealso{
  Other grid.makers: \code{\link{beta.grid}},
  \code{\link{gumbel.grid}}, \code{\link{hyp.sine}},
  \code{\link{log.g}}, \code{\link{log.g2}},
  \code{\link{lognorm.grid}}
}

