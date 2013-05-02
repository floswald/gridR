\name{beta.grid}
\alias{beta.grid}
\title{points distributed according to beta dist: internal function}
\usage{
  beta.grid(b, n, plotit, ...)
}
\arguments{
  \item{b}{numeric vector of length 2. bounds.}

  \item{n}{number of desired points}

  \item{plotit}{boolean TRUE if want a plot of result}

  \item{shape1}{shape param 1 ("alpha")}

  \item{shape2}{shape param 2 ("beta")}

  \item{ncp}{non-centrality paramter}
}
\value{
  numeric vector of gridpoints
}
\description{
  distribute points according to the beta density. use
  shape1, shape2 and noncentrality parameter to affect
  shape.
}
\examples{
grid.maker(bounds=c(-1,5),num.points=10,spacing="beta.grid",plotit=TRUE,shape1=2,shape2=5,ncp=1)
}
\seealso{
  \link[stats]{dbeta}

  Other grid.makers: \code{\link{exp.grid}},
  \code{\link{gumbel.grid}}, \code{\link{hyp.sine}},
  \code{\link{log.g}}, \code{\link{log.g2}},
  \code{\link{lognorm.grid}}
}

