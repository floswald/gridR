\name{expo2}
\alias{expo2}
\title{twice exponentially scaled grid}
\usage{
  expo2(b, n, plotit)
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
gridR(bounds=c(-1,5),num.points=10,spacing="expo2",plotit=TRUE)
}
\seealso{
  Other grid.makers: \code{\link{beta.grid}},
  \code{\link{expo}}, \code{\link{gumbel.grid}},
  \code{\link{hyp.sine}}, \code{\link{log.grid}},
  \code{\link{lognorm.grid}}
}

