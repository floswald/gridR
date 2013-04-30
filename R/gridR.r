



#' grid maker 
#'
#' produce differently scaled one-dimensional grids
#' @param bounds numeric vector of length 2: bounds of interval
#' @param num.points number of points desired
#' @param spacing character vector indicating the desired spacing rule. Currently one of \code{\link{expo}}, \code{\link{expo2}}, \code{\link{hyp.sine}}, \code{\link{log.grid}}, \code{\link{gumbel.grid}}, \code{\link{lognorm.grid}} or \code{\link{beta.grid}}
#' @param plotit boolean TRUE if want a plot of result
#' @param ... additional arguments needed for grid.makers using distributions
#' @author Florian Oswald <florian.oswald@@gmail.com>
#' @import evd
#' @export
#' @return numeric vector of class "gridR" with attribute "spacing"
#' @examples
#' gridR(bounds=c(-1,5),num.points=10,spacing="expo",plotit=FALSE)
#' gridR(bounds=c(-1,5),num.points=10,spacing="expo",plotit=TRUE)
#' gridR(bounds=c(-1,5),num.points=10,spacing="expo2",plotit=TRUE)
#' gridR(bounds=c(-1,5),num.points=10,spacing="expo2",plotit=FALSE)
#' gridR(bounds=c(-1,5),num.points=10,spacing="hyp.sine",plotit=FALSE)
#' gridR(bounds=c(-10,10),num.points=20,spacing="hyp.sine",plotit=TRUE)
#' gridR(bounds=c(-1,5),num.points=10,spacing="log.grid",plotit=TRUE)
#' gridR(bounds=c(-1,5),num.points=10,spacing="log.grid",plotit=FALSE)
#' gridR(bounds=c(1,5),num.points=10,spacing="lognorm.grid",plotit=TRUE,meanlog=3,sdlog=0.5)
#' gridR(bounds=c(-1,5),num.points=10,spacing="lognorm.grid",plotit=FALSE,meanlog=3,sdlog=0.5)
#' gridR(bounds=c(-1,5),num.points=10,spacing="gumbel.grid",plotit=TRUE,loc=1,scale=1.1)
#' gridR(bounds=c(-1,5),num.points=10,spacing="gumbel.grid",plotit=FALSE,loc=1,scale=1.1)
#' gridR(bounds=c(-1,5),num.points=10,spacing="beta.grid",plotit=TRUE,shape1=2,shape2=5,ncp=1)
#' gridR(bounds=c(-1,5),num.points=10,spacing="beta.grid",plotit=FALSE,shape1=2,shape2=5,ncp=1)
gridR <- function(bounds,num.points,spacing="expo",plotit=FALSE,...){

	if (!is.character(spacing)) stop("spacing must be a character specifying the rule you want")
	stopifnot(length(bounds)==2)
	b <- sort(bounds)
	n <- num.points

	# apply transformation

	if (spacing=="expo"){

		z <- expo(b,n,plotit)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else if (spacing=="expo2"){

		z <- expo2(b,n,plotit)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else if (spacing=="hyp.sine"){

		z <- hyp.sine(b,n,plotit)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else if (spacing=="log.grid"){

		z <- log.grid(b,n,plotit)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else if (spacing=="gumbel.grid"){

		z <- gumbel.grid(b,n,plotit,...)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else if (spacing=="lognorm.grid"){

		z <- lognorm.grid(b,n,plotit,...)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)
	
	} else if (spacing=="beta.grid"){

		z <- beta.grid(b,n,plotit,...)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else {
		cat('you chose a rule that does not exist. 
			make sure you chose one of 
			expo, expo2, hyp.sine, log.grid, gumbel.grid, lognorm.grid or beta.grid')
		return(NULL)
	}
}







