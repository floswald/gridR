\name{gridR}
\alias{gridR}
\title{grid maker}
\usage{
  gridR(bounds, num.points, spacing = "expo",
    plotit = FALSE, ...)
}
\arguments{
  \item{bounds}{numeric vector of length 2: bounds of
  interval}

  \item{num.points}{number of points desired}

  \item{spacing}{character vector indicating the desired
  spacing rule. Currently one of \code{\link{expo}},
  \code{\link{expo2}}, \code{\link{hyp.sine}},
  \code{\link{log.grid}}, \code{\link{gumbel.grid}},
  \code{\link{lognorm.grid}} or \code{\link{beta.grid}}}

  \item{plotit}{boolean TRUE if want a plot of result}

  \item{...}{additional arguments needed for grid.makers
  using distributions}
}
\value{
  numeric vector of class "gridR" with attribute "spacing"
}
\description{
  produce differently scaled one-dimensional grids
}
\examples{
gridR(bounds=c(-1,5),num.points=10,spacing="expo",plotit=FALSE)
gridR(bounds=c(-1,5),num.points=10,spacing="expo",plotit=TRUE)
gridR(bounds=c(-1,5),num.points=10,spacing="expo2",plotit=TRUE)
gridR(bounds=c(-1,5),num.points=10,spacing="expo2",plotit=FALSE)
gridR(bounds=c(-1,5),num.points=10,spacing="hyp.sine",plotit=FALSE)
gridR(bounds=c(-10,10),num.points=20,spacing="hyp.sine",plotit=TRUE)
gridR(bounds=c(-1,5),num.points=10,spacing="log.grid",plotit=TRUE)
gridR(bounds=c(-1,5),num.points=10,spacing="log.grid",plotit=FALSE)
gridR(bounds=c(1,5),num.points=10,spacing="lognorm.grid",plotit=TRUE,meanlog=3,sdlog=0.5)
gridR(bounds=c(-1,5),num.points=10,spacing="lognorm.grid",plotit=FALSE,meanlog=3,sdlog=0.5)
gridR(bounds=c(-1,5),num.points=10,spacing="gumbel.grid",plotit=TRUE,loc=1,scale=1.1)
gridR(bounds=c(-1,5),num.points=10,spacing="gumbel.grid",plotit=FALSE,loc=1,scale=1.1)
gridR(bounds=c(-1,5),num.points=10,spacing="beta.grid",plotit=TRUE,shape1=2,shape2=5,ncp=1)
gridR(bounds=c(-1,5),num.points=10,spacing="beta.grid",plotit=FALSE,shape1=2,shape2=5,ncp=1)
}
\author{
  Florian Oswald <florian.oswald@gmail.com>
}

