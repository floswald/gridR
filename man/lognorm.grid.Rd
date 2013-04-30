\name{lognorm.grid}
\alias{lognorm.grid}
\title{points distributed according to log normal}
\usage{
  lognorm.grid(b, n, plotit, ...)
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
\examples{
gridR(bounds=c(1,5),num.points=10,spacing="lognorm.grid",plotit=TRUE,meanlog=3,sdlog=0.5)
}
\seealso{
  \link[stats]{dlnorm}

  Other grid.makers: \code{\link{beta.grid}},
  \code{\link{expo}}, \code{\link{expo2}},
  \code{\link{gumbel.grid}}, \code{\link{hyp.sine}},
  \code{\link{log.grid}}
}

