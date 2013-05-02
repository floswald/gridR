\name{grid.maker}
\alias{grid.maker}
\title{grid maker}
\usage{
  grid.maker(bounds, num.points, spacing = "log.g",
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
grid.maker(bounds=c(-1,5),num.points=10,spacing="log.g",plotit=FALSE)
grid.maker(bounds=c(-1,5),num.points=10,spacing="log.g",plotit=TRUE)
grid.maker(bounds=c(-1,5),num.points=10,spacing="log.g2",plotit=TRUE)
grid.maker(bounds=c(-1,5),num.points=10,spacing="log.g2",plotit=FALSE)
grid.maker(bounds=c(-1,5),num.points=10,spacing="hyp.sine",plotit=FALSE)
grid.maker(bounds=c(-10,10),num.points=20,spacing="hyp.sine",plotit=TRUE)
grid.maker(bounds=c(-1,5),num.points=10,spacing="exp.grid",plotit=TRUE)
grid.maker(bounds=c(-1,5),num.points=10,spacing="exp.grid",plotit=FALSE)
grid.maker(bounds=c(1,5),num.points=10,spacing="lognorm.grid",plotit=TRUE,meanlog=3,sdlog=0.5)
grid.maker(bounds=c(-1,5),num.points=10,spacing="lognorm.grid",plotit=FALSE,meanlog=3,sdlog=0.5)
grid.maker(bounds=c(-1,5),num.points=10,spacing="gumbel.grid",plotit=TRUE,loc=1,scale=1.1)
grid.maker(bounds=c(-1,5),num.points=10,spacing="gumbel.grid",plotit=FALSE,loc=1,scale=1.1)
grid.maker(bounds=c(-1,50),num.points=10,spacing="gumbel.grid",plotit=TRUE,loc=1,scale=10)
grid.maker(bounds=c(-1,50),num.points=10,spacing="gumbel.grid",plotit=TRUE,loc=10,scale=10)
grid.maker(bounds=c(-1,5),num.points=10,spacing="beta.grid",plotit=TRUE,shape1=2,shape2=5,ncp=0)
grid.maker(bounds=c(-10,5),num.points=20,spacing="beta.grid",plotit=TRUE,shape1=0.5,shape2=0.5,ncp=0)
grid.maker(bounds=c(-10,5),num.points=20,spacing="beta.grid",plotit=TRUE,shape1=0.5,shape2=1,ncp=0)
grid.maker(bounds=c(-10,5),num.points=20,spacing="beta.grid",plotit=TRUE,shape1=0.5,shape2=1,ncp=2)
grid.maker(bounds=c(-1,5),num.points=10,spacing="beta.grid",plotit=FALSE,shape1=2,shape2=5,ncp=1)
}

