



#' grid maker 
#'
#' produce differently scaled one-dimensional grids
#' @param bounds numeric vector of length 2: bounds of interval
#' @param num.points number of points desired
#' @param spacing character vector indicating the desired spacing rule. Currently one of expo, expo2, hyp.sine, log.grid, gumbel, log.norm or beta
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
#' gridR(bounds=c(1,5),num.points=10,spacing="log.norm",plotit=TRUE,meanlog=3,sdlog=0.5)
#' gridR(bounds=c(-1,5),num.points=10,spacing="log.norm",plotit=FALSE,meanlog=3,sdlog=0.5)
#' gridR(bounds=c(-1,5),num.points=10,spacing="gumbel",plotit=TRUE,loc=1,scale=1.1)
#' gridR(bounds=c(-1,5),num.points=10,spacing="gumbel",plotit=FALSE,loc=1,scale=1.1)
#' gridR(bounds=c(-1,5),num.points=10,spacing="beta",plotit=TRUE,shape1=2,shape2=5,ncp=1)
#' gridR(bounds=c(-1,5),num.points=10,spacing="beta",plotit=FALSE,shape1=2,shape2=5,ncp=1)
gridR <- function(bounds,num.points,spacing="expo",plotit=FALSE,...){

	if (!is.character(spacing)) stop("spacing must be a character specifying the rule you want")
	stopifnot(length(bounds)==2)
	b <- sort(bounds)
	n <- num.points

	# apply transformation

	if (spacing=="expo"){

		z <- gridR.expo(b,n,plotit)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else if (spacing=="expo2"){

		z <- gridR.expo2(b,n,plotit)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else if (spacing=="hyp.sine"){

		z <- gridR.hyp.sine(b,n,plotit)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else if (spacing=="log.grid"){

		z <- gridR.log.grid(b,n,plotit)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else if (spacing=="gumbel"){

		z <- gridR.gumbel(b,n,plotit,...)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else if (spacing=="log.norm"){

		z <- gridR.lognorm(b,n,plotit,...)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)
	
	} else if (spacing=="beta"){

		z <- gridR.beta(b,n,plotit,...)
		class(z) <- "gridR"
		attr(z,"spacing") <- spacing
		return(z)

	} else {
		cat('you chose a rule that does not exist. 
			make sure you chose one of 
			expo, expo2, hyp.sine, log.grid, gumbel, log.norm or beta')
		return(NULL)
	}
}





#' exponentially scaled grid
#'
#' make a grid with more points towards the lower bound. adjusts to negative lower bound as well.
#' @param b numeric vector of length 2. bounds.
#' @param n number of desired points
#' @param plotit boolean TRUE if want a plot of result
#' @family grid.makers
#' @return numeric vector of gridpoints
gridR.expo <- function(b,n,plotit) {
		out <- rep(0,n)
		off <- 1	# offset for log(0) in case b[1] is positive
		if (b[1]<0) off   <- 1 - b[1] #  adjust in case of neg bound
		out[1]            <- log(b[1] + off)
		out[n]            <- log(b[2] + off)
		out               <- seq(from=out[1],to=out[n],le=n)
		out               <- exp( out ) - off
		if (plotit){
			oldpar <- par()$mar
			par(mar=c(15,4,15,2))
			plot(x=out,y=rep(1,n),yaxt="n",ylab="",pch=3,xlab="point allocation",main="exponentially scaled grid")
			par(mar=oldpar)
		}
		return(out)
}
			
#' twice exponentially scaled grid
#'
#' make a grid with concentrated even more towards the lower bound. adjusts to negative lower bound as well.
#' @param b numeric vector of length 2. bounds.
#' @param n number of desired points
#' @param plotit boolean TRUE if want a plot of result
#' @family grid.makers
#' @return numeric vector of gridpoints
gridR.expo2 <- function(b,n,plotit) {
		out <- rep(0,n)
		off <- 1
		if (b[1]<0) off   <- 1 - b[1] #  adjust in case of neg bound
		out[1]            <- log( log(b[1] + off) + off )
		out[n]            <- log( log(b[2] + off) + off )
		out               <- seq(from=out[1],to=out[n],le=n)
		out               <- exp( exp(out) - off ) - off
		if (plotit){
			oldpar <- par()$mar
			par(mar=c(15,4,15,2))
			plot(x=out,y=rep(1,n),yaxt="n",ylab="",pch=3,xlab="point allocation",main="double exponentially scaled grid")
			par(mar=oldpar)
		}
		return(out)
}
		  



#' hyperbolic sine scaling towards zero
#'
#' increase concentration of points symmetrically around zero.
#' @param b numeric vector of length 2. bounds.
#' @param n number of desired points
#' @param plotit boolean TRUE if want a plot of result
#' @family grid.makers
#' @return numeric vector of gridpoints
gridR.hyp.sine <- function(b,n,plotit) {
		out <- sinh(seq(asinh(b[1]),asinh(b[2]),le=n))
		if (plotit){
			oldpar <- par()$mar
			par(mar=c(15,4,15,2))
			plot(x=out,y=rep(1,n),yaxt="n",ylab="",pch=3,xlab="point allocation",main="Hyperbolic sine Scaling",sub="scales towards zero")
			par(mar=oldpar)
		}
		return(out)
}



#' log scaled grid: more points at upper bound
#'
#' concentrate points towards the upper bound.
#' @param b numeric vector of length 2. bounds.
#' @param n number of desired points
#' @param plotit boolean TRUE if want a plot of result
#' @family grid.makers
#' @return numeric vector of gridpoints
gridR.log.grid <- function(b,n,plotit) {
		out    <- rep(0,n)
		out[1] <- exp( b[1] )
		out[n] <- exp( b[2] )
		out    <- seq(from=out[1],to=out[n],le=n)
		out    <- log(out)
		if (plotit){
			oldpar <- par()$mar
			par(mar=c(15,4,15,2))
			plot(x=out,y=rep(1,n),yaxt="n",ylab="",pch=3,xlab="point allocation",main="log scaled grid")
			par(mar=oldpar)
		}
		return(out)
}



#' points distributed according to gumbel pdf
#'
#' distribute points according to the gumbel density. use location and scale parameter to change shape of distribution.
#' @param b numeric vector of length 2. bounds.
#' @param n number of desired points
#' @param plotit boolean TRUE if want a plot of result
#' @param loc location parameter
#' @param scale parameter
#' @seealso \link[evd]{pgumbel}
#' @family grid.makers
#' @return numeric vector of gridpoints
gridR.gumbel <- function(b,n,plotit,...) {
		y <- list(...)
		out    <- qgumbel(p=seq(from=0.01,to=0.99,le=n),...)
		out <- b[1] + diff(b)*out	# linear map of [0,1] into [lb,ub]
		if (plotit){
			par(mfrow=c(2,1))
			#                 oldpar <- par()$mar
			#                 par(mar=c(15,4,15,2))
			plot(x=out,y=rep(1,n),yaxt="n",ylab="",pch=3,xlab="point allocation",main=sprintf("gumbel density scaling with loc=%s and scale=%s",...))
			abline(v=y$loc,col="red")
			#                 par(mar=oldpar)
			curve(dgumbel(x,...),from=b[1],to=b[2])
			abline(v=y$loc,col="red")
			par(mfrow=c(1,1))
		}
		return(out)
}

#' points distributed according to log normal
#'
#' distribute points according to the log normal density. only for points in positive range.
#' @param b numeric vector of length 2. bounds.
#' @param n number of desired points
#' @param plotit boolean TRUE if want a plot of result
#' @param meanlog mean of log normal
#' @param sdlog standard deviation of log normal
#' @seealso \link[stats]{dlnorm}
#' @family grid.makers
#' @return numeric vector of gridpoints
gridR.lognorm <- function(b,n,plotit,...) {
	y <- list(...)
		bounds <- plnorm(q=b,...)
		out    <- qlnorm(p=seq(from=bounds[1],to=bounds[2],le=n),...)
		if (plotit){
			par(mfrow=c(2,1))
			#                 oldpar <- par()$mar
			#                 par(mar=c(15,4,15,2))
			plot(x=out,y=rep(1,n),yaxt="n",ylab="",pch=3,xlab="point allocation",main=sprintf("lognormal density scaling with meanlog=%s and sdlog=%s",...))
			abline(v=y$meanlog,col="red")
			#                 par(mar=oldpar)
			curve(dlnorm(x,...),from=b[1],to=b[2])
			abline(v=y$meanlog,col="red")
			par(mfrow=c(1,1))
		}
		return(out)
}


#' points distributed according to beta dist
#'
#' distribute points according to the beta density. use shape1, shape2 and noncentrality parameter to affect shape.
#' @param b numeric vector of length 2. bounds.
#' @param n number of desired points
#' @param plotit boolean TRUE if want a plot of result
#' @param shape1 shape param 1 ("alpha")
#' @param shape2 shape param 2 ("beta")
#' @param ncp non-centrality paramter
#' @seealso \link[stats]{dbeta}
#' @family grid.makers
#' @return numeric vector of gridpoints
gridR.beta <- function(b,n,plotit,...) {
	y <- list(...)
		out    <- qbeta(p=seq(from=0.01,to=0.99,le=n),...)
		out <- b[1] + diff(b)*out	# linear map of [0,1] into [lb,ub]
		if (plotit){
			par(mfrow=c(2,1))
			#                 oldpar <- par()$mar
			#                 par(mar=c(15,4,15,2))
			plot(x=out,y=rep(1,n),yaxt="n",ylab="",pch=3,xlab="point allocation",main=sprintf("beta density scaling with shape1=%s, shape2=%s and noncentrality=%s",...))
			abline(v=b[1] + diff(b)*(y$shape1/(y$shape1+y$shape2)),col="red")
			#                 par(mar=oldpar)
			curve(dbeta((x-b[1])/diff(b),y$shape1,y$shape2,y$ncp),from=b[1],to=b[2],ylab="beta density")
			abline(v= b[1] + diff(b)*(y$shape1/(y$shape1+y$shape2)),col="red")
			par(mfrow=c(1,1))
		}
		return(out)
}


#' knot.selector
#'
#' @description select knots for splineDesign at quantiles of data or at user-specified locations
#' @details user selects \emph{degree} of spline and optionally the number of desired basis functions. If \code{num.basis} is not \code{NULL}, function produces a knot vector on supplied data vector \code{x}, where the interior knots are placed at the quantiles of \code{x}. If If \code{num.basis} is \code{NULL}, the interior knots are given by \code{x[-c(1,length(x))]} and \code{num.basis} is chosen accordingly. The crucial relationship is num.basis = \code{length(knots)} - degree - 1, which we use to find \code{length(knots)} = num.basis + degree + 1. The minimum number of basis functions to obtain a valid knot vector with correct multiplicity is \code{min(num.basis) = deg + 1}. 
#' @param degree positive integer for spline degree
#' @param x numeric vector of data sites or desired spline knots
#' @param num.basis optional. if not \code{NULL}, integer of desired number of basis functions. If \code{NULL}, 
#' @param plotit logical of whether plot result
#' @return numeric vector of spline knots with multiplicity \code{degree+1} of class \emph{knotVec} with attribute \emph{num.basis}. '
#' @export
#' @examples 
#' knot.select(degree=4,x=1:10)
#' knot.select(degree=3,x=1:10,num.basis=8,plotit=TRUE)
#' knot.select(degree=3,x=1:10,num.basis=3)  # warning
#' knot.select(degree=1,x=1:10,num.basis=2)
#' knot.select(degree=4,x=1:10)
#' knot.select(degree=3,x=1:10,num.basis=6,stretch=0.01,plotit=TRUE)
knot.select <- function(degree,x,num.basis=NULL,plotit=FALSE){

    n     <- length(x)
	x     <- sort(x)
	kdown <- rep(x[1],times=degree+1)
	kup   <- rep(x[n],times=degree+1)

	if (is.null(num.basis)){
		# just extend x to a knot vector and return the number of basis funcitons
		knots <- c(kdown, x[-c(1,n)], kup)
		num.basis <- length(knots) - degree - 1

	} else {
		# given num.basis and x, allocate interior knots at quantiles of x

		if (num.basis < degree + 1) {
			num.basis <- degree + 1
			warning(c("you chose too few basis functions. I selected the required minimum of degree + 1 =",num.basis))
		}
		# any remaining points for interior knots?
		z <- num.basis - (degree+1)
		iknots <- NULL
		if (z > 0) {
			quants <- seq.int(from=0,to=1,length.out=z+2)[-c(1,z+2)]	# quantiles of data sites do consider (excluding first and last, since those are included in kdown and kup)
			iknots <- quantile(x,quants)
		}
		knots <- c(kdown, iknots, kup)
		names(knots) <- NULL
		stopifnot(length(knots) == num.basis + degree + 1)
	}
	if (plotit){
		plot(x=x,y=rep(1,n),yaxt='n',ylab="",ylim=c(0.8,1.4),xlim=range(knots),xlab="data index",main=sprintf("Spline Knots and Data\nsetup implies %s basis functions",num.basis),sub=sprintf("note knot multiplicity of %s at first and last data point",degree+1))
		points(x=knots,y=rep(1.2,length(knots)),pch=3)
		legend("bottomright",legend=c("data","knots"),pch=c(1,3))
	}
	class(knots) <- 'knotVec'
	attr(knots,'num.basis') <- num.basis
	return(knots)
}
