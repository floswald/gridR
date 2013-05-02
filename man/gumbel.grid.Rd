\name{gumbel.grid}
\alias{gumbel.grid}
\title{points distributed according to gumbel pdf: internal function}
\usage{
  gumbel.grid(b, n, plotit, ...)
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
\examples{
grid.maker(bounds=c(-1,5),num.points=10,spacing="gumbel.grid",plotit=TRUE,loc=1,scale=1.1)
}
\seealso{
  \link[evd]{pgumbel}

  Other grid.makers: \code{\link{beta.grid}},
  \code{\link{exp.grid}}, \code{\link{hyp.sine}},
  \code{\link{log.g}}, \code{\link{log.g2}},
  \code{\link{lognorm.grid}}
}

