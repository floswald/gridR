\docType{package}
\name{gridR-package}
\alias{gridR-package}
\title{gridR}
\description{
  gridR is a package that helps with a fundamental problem
  in function approximation: where to place the grid points
  at which to measure the function. There are various ways
  to derive differently scaled points from a uniform grid
  in the package. There is also a helper function to setup
  a knot vector as an input to
  \code{\link[splines]{splineDesign}}. The user can decide
  the exact position of knots, or specify the desired
  number of basis functions (similar to what you do with
  \code{df} in \code{\link[splines]{ns}}). In fact, this
  function can be viewed as an extension to
  \code{\link[splines]{ns}}, where only (natural) cubic
  spline basis are supported. All functions have a plotting
  method to help guide the user. Development is at
  \url{https://github.com/floswald/gridR} The main
  functions are \code{\link{grid.maker}} and
  \code{\link{knot.select}}.
}
\author{
  Florian Oswald <florian.oswald@gmail.com>
}
\seealso{
  \link[splines]{splines-package}
}

