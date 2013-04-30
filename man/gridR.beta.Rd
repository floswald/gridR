\name{gridR.beta}
\alias{gridR.beta}
\title{points distributed according to beta dist}
\usage{
  gridR.beta(b, n, plotit, ...)
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
\seealso{
  \link[stats]{dbeta}

  Other grid.makers: \code{\link{gridR.expo}},
  \code{\link{gridR.expo2}}, \code{\link{gridR.gumbel}},
  \code{\link{gridR.hyp.sine}},
  \code{\link{gridR.log.grid}}, \code{\link{gridR.lognorm}}
}

