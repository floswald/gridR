
#' knot.selector: produce knot vector for splineDesign
#'
#' @description select knots for \code{\link[splines]{splineDesign}} at quantiles of data or at user-specified locations. 
#' @details user selects \emph{degree} of spline and optionally the number of desired basis functions. If \code{num.basis} is not \code{NULL}, function produces a knot vector on supplied data vector \code{x}, where the remaining interior knots are placed at the quantiles of \code{x}. \emph{Remaining} means "after beginning/end multiplicity was constructed". If \code{num.basis} is \code{NULL}, the interior knots are given by \code{x[-c(1,length(x))]} (i.e. all but first and last element of x) and \code{num.basis} is chosen accordingly. The relationship is \code{num.basis} = \code{length(knots)} - degree - 1. The minimum number of basis functions to obtain a valid knot vector with correct multiplicity is \code{min(num.basis) = deg + 1}. The function overrides choices where \code{num.basis} is too small with a warning.
#' @param degree positive integer for spline degree
#' @param x numeric vector of data sites or desired spline knots
#' @param num.basis optional. 
#' @param plotit logical of whether plot result
#' @return numeric vector of spline knots of length \code{num.basis} + \code{degree} + 1, with knot multiplicity \code{degree+1} of class \emph{knotVec} with attribute \emph{num.basis}. 
#' @export
#' @references \url{http://www.cs.mtu.edu/~shene/COURSES/cs3621/NOTES/spline/B-spline/bspline-basis.html}, \url{http://www.uio.no/studier/emner/matnat/ifi/INF-MAT5340/v07/undervisningsmateriale/}
#' @examples 
#' knot.select(degree=4,x=1:10)
#' knot.select(degree=3,x=1:10,num.basis=8,plotit=TRUE)
#' knot.select(degree=3,x=1:10,num.basis=3)  # warning
#' knot.select(degree=1,x=1:10,num.basis=2)
#' knot.select(degree=4,x=1:10)
#' knot.select(degree=3,x=1:10,num.basis=6,plotit=TRUE)
#' #
#' # make a double exponentially-scaled grid and use as knots
#' #
#' log.grid <- grid.maker(c(-10,15),20,spacing="log.g2")
#' knot.select(degree=4,x=log.grid,num.basis=NULL,TRUE)
#' #
#' # choose number of basis funs on same grid: remaining interior (after constructing multiplicities)
#' # knots will be placed at quantiles of grid
#' #
#' knot.select(degree=4,x=log.grid,num.basis=8,TRUE)
#' #
#' # make a hyperbolic-sine-scaled grid and use as knots
#' #
#' hyp.grid <- grid.maker(c(-10,15),20,spacing="hyp.sine")
#' knot.select(degree=5,x=hyp.grid,num.basis=NULL,TRUE)
#' #
#' # choose number of basis funs on same grid
#' #
#' knot.select(degree=5,x=hyp.grid,num.basis=9,TRUE)
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
		par(mfcol=c(2,1))
		plot(x=x,y=rep(1,n),yaxt='n',ylab="",ylim=c(0.8,1.4),xlim=range(knots),xlab="data index",main=sprintf("Spline Knots and Data\nsetup implies %s basis functions",num.basis),sub=sprintf("note knot multiplicity of %s at first and last data point",degree+1))
		points(x=knots,y=rep(1.2,length(knots)),pch=3)
		legend("bottomright",legend=c("data","knots"),pch=c(1,3))
		y <- seq(x[1],x[n],length=500)
		basis <- splineDesign(knots,x=y,ord=degree+1)
		ry <- range(y)
		dry <- diff(ry)
		off <- 0
		plot(c(ry[1]-off*dry,ry[2]+off*dry), c(0,1), type = "n", xlab = "x", ylab = "",main =  "implied B-splines")
		matlines(y,basis,ylim=c(0,1),xlim=c(ry[1]-0.1*dry,ry[2]+0.1*dry))
		par(mfcol=c(1,1))
	}
	class(knots) <- 'knotVec'
	attr(knots,'num.basis') <- num.basis
	return(knots)
}
