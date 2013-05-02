\name{log.g2}
\alias{log.g2}
\title{twice log scaled grid: internal function}
\usage{
  log.g2(b, n, plotit)
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
  make a grid with concentrated even more towards the lower
  bound. adjusts to negative lower bound as well.
}
\examples{
grid.maker(bounds=c(-1,5),num.points=10,spacing="log2",plotit=TRUE)
}
\seealso{
  Other grid.makers: \code{\link{beta.grid}},
  \code{\link{exp.grid}}, \code{\link{gumbel.grid}},
  \code{\link{hyp.sine}}, \code{\link{log.g}},
  \code{\link{lognorm.grid}}
}

