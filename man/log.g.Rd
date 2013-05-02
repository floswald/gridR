\name{log.g}
\alias{log.g}
\title{log scaled grid: internal function}
\usage{
  log.g(b, n, plotit)
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
\examples{
grid.maker(bounds=c(-1,5),num.points=10,spacing="log",plotit=TRUE)
}
\seealso{
  Other grid.makers: \code{\link{beta.grid}},
  \code{\link{exp.grid}}, \code{\link{gumbel.grid}},
  \code{\link{hyp.sine}}, \code{\link{log.g2}},
  \code{\link{lognorm.grid}}
}

