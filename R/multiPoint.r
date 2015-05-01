# --------------------------------------------------------------------------------
# Programmer: Jason Thorpe
# Date        Wednesday, May 01, 2013 3:27:01 PM
# Language:   R (.r) Version 2.15.0
# Purpose:    plot multiple adjacent boxes with colors and/or characteres
# Comments:   
# --------------------------------------------------------------------------------

#' Plotting with compound symbols
#' 
#' Plotting with compound symbols made from a matrix of standard symbols 
#' each with their own characteristics
#' 
#' @param x,y the coordinates of points in the plot 
#' @param n = if(!missing(col))
#' @param col a matrix of colors with 1 row for each plot point. See \code{\link[graphics]{points}} for details.
#' @param bg a matrix of background colors with 1 row for each plot point. See \code{\link[graphics]{points}} for details.
#' @param pch a matrix of plotting ‘character’, i.e., symbol to use. See \code{\link[graphics]{points}} for details.
#' @param xheight = strheight('M')*cex * 1.1,
#' @param xwidth = strwidth('M')*cex * 1.1,
#' @param ... additional arguments to 'plot'
#' 
#' @export
#' @examples
#' n <- c(2,2)
#' x <- 1:5
#' y <- 1:5
#' #-- col <- matrix(rainbow(20),5,4)
#' col <- rbind(rainbow(4),
#' 			 rainbow(4),
#' 			 rainbow(4),
#' 			 rainbow(4),
#' 			 rainbow(4))
#' 
#' col1 <- col
#' col1[] <- 'hotpink'
#' col[,3] <- NA
#' plot(x,y)
#' multiPoints(x,y,n = 4,col=col)
#' multiPoints(x,y,n = 1,col=col)
#' multiPoints(x,y,n = 2,col=col)
#' multiPoints(x,y,n = 2,col=col1,pch = matrix(1:4,5,4,byrow=TRUE))
#' multiPoints(x,y,n = 1,col=col1,pch = matrix(letters[1:4],5,4,byrow=TRUE),cex = 2)
#' 
#' plot(x,y)
#' multiPoints(x,y,n = 1,col=col,cex = 2)
#' multiPoints(x,y,n = 1,col=col1,pch = matrix(letters[1:4],5,4,byrow=TRUE),cex = 2)
#' points(x,y,pch = 15)
#' 
#' #multiPoints(x,y,n = 2,col='black',pch = matrix(1:20,5))
#' multiPoints(x,y,n = 2,col=col)

multiPoints <- function(x,y,
					  n = if(!missing(col))
							  rep(ceiling(sqrt(ncol(col))),2)
						  else 
							  rep(ceiling(sqrt(ncol(pch))),2),
					  col=matrix('black',length(x),ncol(pch)), 
					  bg=matrix(NA,length(x),ncol(col)),
					  pch=matrix(NA,length(x),ncol(col)),
					  xheight = strheight('M')*cex * 1.1,
					  xwidth = strwidth('M')*cex * 1.1,
					  ...){
	# basics:
	varArgs <- list(...)
	if('cex' %in% names(varArgs)) 
		cex <- varArgs[['cex']]
	else 
		cex <- par('cex')
	if(length(cex) > 1)
		message("Only using first value from paramter 'cex'")
	cex <- cex[1]
	stopifnot(length(n) == 1)
	stopifnot(!missing(col) | !missing(pch)) # one or the other required (otherwise what's the point?)
	stopifnot(length(x) == length(y))
	stopifnot(length(x) == nrow(col))
	stopifnot(length(x) == nrow(pch))
	stopifnot(ncol(col)==ncol(pch))
	if(!missing(pch)) 
		nboxes <- ncol(pch) 
	else 
		nboxes <- ncol(col) 
	stopifnot(nboxes > 1)
	# get the box heights
	multiPoint <- function(x,y,col = NA,pch = NA,...){
		if(length(col) == 1)
			tryCatch(col <- matrix(col,nrow(pch),ncol(pch)),
					 error=function(w)browser())
		if(mode(col)  ==  'character' || all(is.na(col)))
			col[is.na(col)] <- 'black'
		for(i in 1:nboxes){
			xshift <-  ((i-1)%%n - (n-1)/2)
			yshift <- -((i-1)%/%n - (nboxes%/%n-1)/2)
			points(x = x +  xshift*xwidth,
					  y = y + yshift *xheight,
					  col = col[, i],
					  pch = pch[, i],
					  bg = bg[, i],
					  ...)
		}
	}
	multiRectangle <- function(x,y,col = NA,...){
		# a convenience function
		rectangle <- function(x,y,...){
			rect(xleft  = x - xwidth/2,
				 xright = x + xwidth/2,
				 ybottom= y - xheight/2,
				 ytop   = y + xheight/2,
				 border = 'black',
				 ...)
		}
		for(i in 1:nboxes){
			xshift <-  ((i-1)%%n - (n-1)/2)
			yshift <- -((i-1)%/%n - (nboxes%/%n-1)/2)
			rectangle(x = x + xshift *xwidth,
					  y = y + yshift *xheight,
					  col = col[, i],
					  ...)
		}
	}
	if(missing(pch))
		multiRectangle(x,y,col,...)
	else
		multiPoint(x,y,col=col,pch=pch,...)
}




