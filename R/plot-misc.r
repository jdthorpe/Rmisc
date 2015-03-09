# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# ploting functions
# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------

#' Calculate plot positoins from relative coordatinates
#'
#' Calculate plot positoins from relative coordatinates. In relative coordinates
#' The plot region ranges from 0 to 1 on both axes.  The coordinates returned
#' are in plot coordinates.
#' @param x,y x and y coordinats, usually  in the interval [0,1]
#' @export
#' @examples
#' plot(1:5,1:3)
#' plotloc(1,1) # the top right corner of the plotting region 
#' plotloc(0,0) # the bottom left corner of the plotting region 
plotloc<-function(x = .5,y = .5){
	usr<-par('usr')
	x.out = usr[1] + diff(usr[1:2])*x
	y.out = usr[3] + diff(usr[3:4])*y
	if(par('xlog')){x.out<-10^x.out}
	if(par('ylog')){y.out<-10^y.out}
	list(x = x.out,y = y.out)
}

#' Reads the position of the graphics cursor when the (first) mouse button is pressed. 
#'
#' Reads the position of the graphics cursor when the (first) mouse button is pressed. 
#' unlike \code{graphics::locator()}, the values returned are in relative coordinates.
#' In relative coordinates, The plot region ranges from 0 to 1 on both axes. 
#' @inheritParams graphics::locator
#' @keywords cats
#' @export
#' @examples
#' cat_function()OT LOCATIONS
plotlocator<-function(n=1,...){
	xy <- locator(n...)
	usr<-par('usr')
	if(par('xlog')){xy$x<-log(xy$x)/log(10)}
	if(par('ylog')){xy$y<-log(xy$y)/log(10)}
	x.out = (xy$x - usr[1])/diff(usr[1:2])
	y.out = (xy$y - usr[3])/diff(usr[3:4])
	list(x = x.out,y = y.out)
}


# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------

#' Add an Axis to a Plot.
#'
#' Add an Axis to a Plot. A wrapper for \code{\link[graphics]{axis}} 
#' which supports the ability to log transforming the default axis labels
#' which comes in handy when calling plot(log(x),log(y),...) is easier than 
#' calling  plot(x,y,log='xy',...)
#' @param log If set to \code{TRUE}, displays the tic marks on the log scale (default=FALSE)
#' @inheritParams graphics::axis
#' @seealso the original \code{\link[graphics]{axis}} function
#' @export
axis <- function(...,log=FALSE)
	if(log) logAxis(...) else graphics::axis(...)


#' Plots the axis on the log scale 
#'
#' Add an Axis to a Plot onthe log scale. A wrapper for \code{\link[graphics]{axis}} 
#' which supports the ability to log transforming the default axis labels
#' which comes in handy when calling plot(log(x),log(y),...) is easier than 
#' calling  plot(x,y,log='xy',...)
#'
#' @inheritParams graphics::axis
#' @seealso \code{\link{axis}}
#' @export
logAxis <- function(side,
					at=log(labels),
					labels= if((fold = diff(par('usr')[if (side %%2) 1:2 else 3:4]/log(10)))<1.4) {
								i <- 0;
								usr <- par('usr')[if (side %%2) 1:2 else 3:4]
								while(TRUE){
									if(i == 20)stop('range too small')
									i <- i + 1
									if(ceiling(exp(usr)[1]) < floor(exp(usr)[2]))
										break
									else
										usr <- usr*10
								};
								seq.int(ceiling(exp(usr)[1])*(10^i), floor(exp(usr)[2])*(10^i), length.out = 5)/10^i
							} else 
								axTicks(side,
									log=TRUE,
									usr= (par("usr")[if (side %%2) 1:2 else 3:4]/log(10)),
									axp = c(10^ceiling(par('usr')[if (side %%2) 1 else 3] / log(10)),
										   	10^  floor(par('usr')[if (side %%2) 2 else 4] / log(10)),
											n=if((fold = diff(par('usr')[if (side %%2) 1:2 else 3:4]/log(10)))<1.4)
											  	-1 else if((fold = diff(par('usr')[if (side %%2) 1:2 else 3:4]/log(10)))>((width=par("pin")[if (side %%2) 1 else 2])))
												1 else if((fold = diff(par('usr')[if (side %%2) 1:2 else 3:4]/log(10)))*1.3>((width=par("pin")[if (side %%2) 1 else 2])))
												2 else 3 
											),
									nintLog = c(if((fold = diff(par('usr')[if (side %%2) 1:2 else 3:4]/log(10)))<1.4) 
												5 else if((fold = diff(par('usr')[if (side %%2) 1:2 else 3:4]/log(10)))>((width=par("pin")[if (side %%2) 1 else 2])))
												floor(fold+1) else if((fold = diff(par('usr')[if (side %%2) 1:2 else 3:4]/log(10)))*1.3>((width=par("pin")[if (side %%2) 1 else 2])))
												floor(fold*2) else  
												floor(fold*3)))
					, ...){
	graphics::axis(side,at =at ,labels=labels,...)
}

#-- #' A stupid wrapper for graphics::abline
#-- #'
#-- #' A stupid wrapper for \code{graphics::abline}.
#-- #' @inheritParams graphics::abline
#-- #' @export
#-- #' @seealso the original \code{\link[graphics]{abline}.} in the graphics package
#-- abline <- function(x,...)
#-- 	UseMethod('abline')
#-- 
#-- abline.default <- function(x,...) 
#-- 	graphics:::abline(x,...)
#-- 
#-- abline.list <- function(x,...){
#-- 	stopifnot('x' %in% names(x))
#-- 	stopifnot('y' %in% names(x))
#-- 	slope <- (diff(x$y)[1])/(diff(x$x)[1])
#-- 	intercept <- x$y[1] - (x$x[1]*slope)
#-- 	graphics:::abline(a = intercept ,
#-- 					  b = slope,...)
#-- }



