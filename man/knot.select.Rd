\name{knot.select}
\alias{knot.select}
\title{knot.selector: produce knot vector for splineDesign}
\usage{
  knot.select(degree, x, num.basis = NULL, plotit = FALSE)
}
\arguments{
  \item{degree}{positive integer for spline degree}

  \item{x}{numeric vector of data sites or desired spline
  knots}

  \item{num.basis}{optional.}

  \item{plotit}{logical of whether plot result}
}
\value{
  numeric vector of spline knots of length \code{num.basis}
  + \code{degree} + 1, with knot multiplicity
  \code{degree+1} of class \emph{knotVec} with attribute
  \emph{num.basis}.
}
\description{
  select knots for \code{\link[splines]{splineDesign}} at
  quantiles of data or at user-specified locations.
}
\details{
  user selects \emph{degree} of spline and optionally the
  number of desired basis functions. If \code{num.basis} is
  not \code{NULL}, function produces a knot vector on
  supplied data vector \code{x}, where the remaining
  interior knots are placed at the quantiles of \code{x}.
  \emph{Remaining} means "after beginning/end multiplicity
  was constructed". If \code{num.basis} is \code{NULL}, the
  interior knots are given by \code{x[-c(1,length(x))]}
  (i.e. all but first and last element of x) and
  \code{num.basis} is chosen accordingly. The relationship
  is \code{num.basis} = \code{length(knots)} - degree - 1.
  The minimum number of basis functions to obtain a valid
  knot vector with correct multiplicity is
  \code{min(num.basis) = deg + 1}. The function overrides
  choices where \code{num.basis} is too small with a
  warning.
}
\examples{
knot.select(degree=4,x=1:10)
knot.select(degree=3,x=1:10,num.basis=8,plotit=TRUE)
knot.select(degree=3,x=1:10,num.basis=3)  # warning
knot.select(degree=1,x=1:10,num.basis=2)
knot.select(degree=4,x=1:10)
knot.select(degree=3,x=1:10,num.basis=6,plotit=TRUE)
#
# make a double exponentially-scaled grid and use as knots
#
log.grid <- grid.maker(c(-10,15),20,spacing="log.g2")
knot.select(degree=4,x=log.grid,num.basis=NULL,TRUE)
#
# choose number of basis funs on same grid: remaining interior (after constructing multiplicities)
# knots will be placed at quantiles of grid
#
knot.select(degree=4,x=log.grid,num.basis=8,TRUE)
#
# make a hyperbolic-sine-scaled grid and use as knots
#
hyp.grid <- grid.maker(c(-10,15),20,spacing="hyp.sine")
knot.select(degree=5,x=hyp.grid,num.basis=NULL,TRUE)
#
# choose number of basis funs on same grid
#
knot.select(degree=5,x=hyp.grid,num.basis=9,TRUE)
}
\references{
  \url{http://www.cs.mtu.edu/~shene/COURSES/cs3621/NOTES/spline/B-spline/bspline-basis.html},
  \url{http://www.uio.no/studier/emner/matnat/ifi/INF-MAT5340/v07/undervisningsmateriale/}
}

