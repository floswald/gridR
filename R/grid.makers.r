
#' exponentially scaled grid
#'
#' make a grid with more points towards the lower bound. adjusts to negative lower bound as well.
#' @param b numeric vector of length 2. bounds.
#' @param n number of desired points
#' @param plotit boolean TRUE if want a plot of result
#' @family grid.makers
#' @return numeric vector of gridpoints
#' @examples
#' gridR(bounds=c(-1,5),num.points=10,spacing="expo",plotit=TRUE)
expo <- function(b,n,plotit) {
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
#' @examples
#' gridR(bounds=c(-1,5),num.points=10,spacing="expo2",plotit=TRUE)
expo2 <- function(b,n,plotit) {
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
#' @examples
#' gridR(bounds=c(-10,10),num.points=20,spacing="hyp.sine",plotit=TRUE)
hyp.sine <- function(b,n,plotit) {
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
#' @examples
#' gridR(bounds=c(-1,5),num.points=10,spacing="log.grid",plotit=TRUE)
log.grid <- function(b,n,plotit) {
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
#' @examples
#' gridR(bounds=c(-1,5),num.points=10,spacing="gumbel.grid",plotit=TRUE,loc=1,scale=1.1)
gumbel.grid <- function(b,n,plotit,...) {
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
#' @examples
#' gridR(bounds=c(1,5),num.points=10,spacing="lognorm.grid",plotit=TRUE,meanlog=3,sdlog=0.5)
lognorm.grid <- function(b,n,plotit,...) {
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
#' @examples
#' gridR(bounds=c(-1,5),num.points=10,spacing="beta.grid",plotit=TRUE,shape1=2,shape2=5,ncp=1)
beta.grid <- function(b,n,plotit,...) {
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
