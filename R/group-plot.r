#' Plot marker values by group
#'
#' Plot marker values by group with wobble added
#' @export plot.group
#'
#' @param group Group membership 
#' @param y Values to be plotted
#' @param labels labels to use for each group. Labels must be 
#' in unique(group) and can be use to re-arrange or filter the display of the data
#' @param bg.panel Colors used as backgrounds for the panels
#' @param xlab,ylab labels for the x and y axes. See \code{\link[graphics]{plot.default}} for details.
#' @param axes axes to be plotted ('x','y','xy', or logical; default = 'xy'). See \code{\link[graphics]{plot.default}} for details.
#' @param add if TRUE, figures are added to the current plotting region, otherwise a new plot is created
#' @param log a character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic. See \code{\link[graphics]{plot.default}} for details.
#' @param ... additional arguments to \code{\link[graphics]{plot.default}}.
#'
#' @return a list of the x and y locations of the points within the figure
#'
#' @examples
#' g <- rep(c('small','medium','large'),times= 5)
#' y <- rep(1:3,times = 5) + runif(15)
#' pts <- plot.group(group = g,
#' 			 y = y,
#' 			 col = rep(c('blue','green','red'),times= 5),
#' 			 pch = 1:15,
#' 			 labels = c('large','medium','small'),
#' 			 xlab = 'group',
#' 			 ylab = 'y value',
#' 			 main = 'sample group plot',
#' 			 log = 'y',
#' 			 sub = 'hello world',
#' 			 bg.panel = c('gray','white','dodgerblue'))
#' points(pts$x, pts$y, cex = 2)

plot.group<-function(group,
					 y,
					 labels = if(inherits(group,'factor')) levels(group) else unique(group),
					 log = '',
					 bg.panel,
					 add = FALSE,
					 axes =TRUE,
					 xlab =deparse(substitute(group)),
					 ylab =deparse(substitute(y)),
					 ...) {
	#######################################################################
	#VALIDATE THE INPUTS
	#######################################################################
	#MAKE SURE THE LENGTHS OF THE INPUTS MAKE SENSE
	if(length(group) != length(y))
		stop('x and y lengths differ')
	if( (!identical(grep('y',log),integer(0))) & (min(y, na.rm = TRUE) <= 0))
		message('WARNING: negative values and zeros are excluded from plot.')
	if(!add) {
		#SETUP THE PLOT
		plot( x = list(c(.8,length(labels) + .2),c(.6,2.4))[[1 + (length(labels) == 2)]],
			 y = c(if(!identical(grep('y',log),integer(0)))
				       min(y[y>0], na.rm = TRUE) else min(y, na.rm = TRUE),
				   max(y, na.rm = TRUE)),
			 axes = FALSE,
			 log = log,
			 type = 'n',
			 xlab = xlab,
			 ylab = ylab,
			 ...)
		# BACKGROUND COLORS
		if(!missing(bg.panel)) {
			if(length(bg.panel) == 1)
				bg.panel <- rep(bg.panel,length(labels))
			if(length(bg.panel) != length(labels))
				stop('background colors must be length 1 or lenth equal to number of categories')
			for(i in 1:length(labels))
				polygon( x = c(i - .5,i - .5,i + .5,i + .5)
						,y = if(par('ylog'))
								10^(par('usr')[c(3,4,4,3)])
							else
								par('usr')[c(3,4,4,3)]
						,col = bg.panel[i]
						,border = NA
						)
		}
		# VERTICAL LINES BETWEEN SECTIONS
		abline(v = (1:(length(labels)-1)) + .5,
			   col = 'gray42',
			   lty = 'dotted')
		box()
		# AXIS LABELS
		if(identical(axes,TRUE))
			axes = 'xy'
		else if(identical(axes,FALSE))
			axes = ''
		if(grepl('y',axes))
			axis(2)
		if(grepl('x',axes))
			axis(side = 1
				,at = 1:length(labels)
				,labels = labels)
	}

	#GENERATE RANDOMS FOR FOR WOBBLE WITIN THE PANE
	randoms<-runif(length(group))*.4 -.2

	# PLOT THE DATA
	points(x = match(group,labels) + randoms, y = y, ... )
	invisible(data.frame(x = match(group,labels) + randoms, y = y))
}


