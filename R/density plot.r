
#' Plot smooth densities of marker values by group
#'
#' Plot smooth densities of marker values by group.
#' @export density.group
#' 
#' @param group Group membership 
#' @param y Values to be plotted
#' @param col fill color of the density blobs (default = "skyblue2", optionally a vector of colors) 
#' @param border border color of the density blobs (default = "dodgerblue4", optionally a vector of colors) 
#' @param labels = if(inherits(group,'factor')) levels(group) else unique(group)
#' @param axes Axes to be plotted ('x','y','xy', or logical; default = 'xy')
#' @param bg.panel Colors used as backgrounds for the panels
#' @param cex.xlab Character expansion factor for xlab (default = 1)
#' @param type Type of plot (default = 's'; 's' is the only supported option for now)
#' @param kernel kernal type (devault = 'c'; see \code{stats::density'} for details
#' @param bandwidth bandwidth smoothing parameter ( see \code{stats::density} for details
#' @param adjust = 0.5 (see \code{stats::density'} for details
#' @param maxwidth = 0.4 maximum half-width of the density blob
#' @param med.text If TRUE, the word 'median' is included above the meedian line. 
#' @param ... additional arguments to plot()
#' 
#' @examples
#' density.group(group = rep(c('small','medium','large'),times= 5),
#'			y = rep(1:3,times = 5) + rnorm(15),
#'			col = c('blue','green','red'),
#'			labels = c('large','medium','small'),
#'			xlab = 'group',
#'			ylab = 'y value',
#'			main = 'sample group plot',
#'			xaxis = T,
#'			yaxis = T,
#'			log = '',
#'			label = NA,
#'			bg.panel = c('white','gray') ) 

density.group<-function( group,
					y,
					col = "skyblue2",
					border = 'dodgerblue4',
					labels = if(inherits(group,'factor')) levels(group) else unique(group),
					xlab =deparse(substitute(group)),
					ylab =deparse(substitute(y)),
					axes = T,
					log = '',
					bg.panel = 'white',
					cex.xlab = 1,
					type = 'p',
					kernel = 'c',
					adjust = 0.5,
					maxwidth = 0.4,
					med.text = T,
					xlim,
					...)
{
	#MAKE SURE THE LENGTHS OF THE INPUTS MAKE SENSE
	if(length(group) != length(y))
		stop('x and y lengths differ')

	if(!length(border) %in% c(length(labels),1)  ){
		message('border not the same length as labels: only using first value')
		border = border[1]
	}
	if(!length(col) %in% c(length(labels),1)  ){
		message('col not the same length as labels: only using first value')
		col = col[1]
	}
	if( length(border) == 1)
		border = rep(border,times = length(labels))
	if( length(col) == 1)
		col = rep(col,times = length(labels))
	ylog <- length(grep('y',log)) > 0

	# GENERATE THE DENSITY DISTRIBUTIONS
	ldensity_x<-ldensity_y<-list()
	lowertrim <- 0.0001 #trim density function below this density
	if(missing(bandwidth)){
		bandwidth <- density(if((!ylog))  y[is.finite(y)] else  log(y[is.finite(y)&(y>0)] ),
							  kernel = kernel)$bw
		bandwidth <- bandwidth*(length(unique(labels))^(1/3))
	}

	ok <-( (!is.na(y))
		 & (is.finite(y))
		 & ((y>0) |(!ylog ))
		 )
	for( g in 1:length(labels)) {
		if(sum(ok & (group == labels[g]))  >2)
		{
			if(!ylog)
			{
					temp_density 	 <- density(y [(group == labels[g]) & is.finite(y)],
												bw = bandwidth,
												kernel = kernel,
												adjust = adjust)
					ldensity_x[[g]]  <- (temp_density$x)
					ldensity_y[[g]]  <- (temp_density$y)
			}else{
					temp_density 	 <- density(log(y) [(group == labels[g]) & is.finite(y)&(y > 0)],
												bw = bandwidth,
												kernel = kernel,
												adjust = adjust)
					ldensity_x[[g]]  <- exp((temp_density$x))
					ldensity_y[[g]]  <- (temp_density$y)
			}
		} else {
			ldensity_x[[g]]  <- NA
			ldensity_y[[g]]  <- NA
		}
	}

	# this version won't complain if everything is missing...
	rangeOrNA <- function(x){
		x <- x[!is.na(x)]
		x <- x[is.finite(x)]
		if(length(x))
			range(x)
		else
			NA
	}
	#rangedensity_x<-  rangeOrNA(unlist(sapply(ldensity_x,rangeOrNA)))
	rangedensity_y<-  rangeOrNA(unlist(sapply(ldensity_y,rangeOrNA)))

	#######################################################################
	#SETUP THE PLOT
	#######################################################################

	if( (ylog) & (min(y, na.rm = T) <= 0))
		message('WARNING: negative and zero values are excluded from plot')

	if( (ylog) & (max(y, na.rm = T) <= 0)) {
		message('WARNING: cannot plot negative valus on logrithmic scale')
		log = ''
	}

	if(ylog)
		ymin = range(y[y>0 & is.finite(y)], na.rm = T,finite = T)[1]
	else
		ymin = range(y, na.rm = T,finite = T)[1]

	# PREPARE THE PLOTTING REGION
	plot(x = if(length(labels) == 2) c(.6,2.4) else c(.8,length(labels) + .2),
		 y = c(ymin,range(y, na.rm = T,finite = T)[2]),
		 xlim = if(missing(xlim)) c(0.5,0.5+length(labels)) else xlim,
		 axes = F,
		 type = 'n',
		 log = log,
		 xlab = xlab,
		 ylab = ylab,
		 ...)
	box()
	if(identical(axes,T))
		axes = 'xy'
	else if(identical(axes,F))
		axes = ''
	if(grepl('y',axes))
		axis(2)
	if(grepl('x',axes))
		axis(side = 1
			,at = 1:length(labels)
			,labels = labels
			, cex.axis = cex.xlab)

	# ADD THE BACKGROUND COLORS
	if(!identical(bg.panel, 'white')) {
		if(length(bg.panel) == 1)
			bg.panel <- rep( bg.panel, times = length(bg.panel))
		if(length(bg.panel) != length(labels))
			stop('background colors must be length 1 or lenth equal to number of categories')
		for(i in 1:length(labels)) {
			polygon( x = c(i - .5,i - .5,i + .5,i + .5)
					,y = c(	 min(y,na.rm = T) - abs( max(y,na.rm = T) - min(y,na.rm = T))
							,max(y,na.rm = T) + abs( max(y,na.rm = T) - min(y,na.rm = T))
							,max(y,na.rm = T) + abs( max(y,na.rm = T) - min(y,na.rm = T))
							,min(y,na.rm = T) - abs( max(y,na.rm = T) - min(y,na.rm = T))
							)
					,col = bg.panel[i]
					,border = NA)
		}
	}

	# PLOT THE DATA
	if(type == 's') { #s is for smooth
		for(g in 1:length(labels))
		if(!all(is.na(y[which(group == labels[g])] )|is.null(y[which(group == labels[g])] ))) {
			polygon( y =                ldensity_x[[g]][c(1:length(ldensity_x[[g]]),length(ldensity_x[[g]]):1,1   )]
					,x = g + maxwidth*((ldensity_y[[g]][c(1:length(ldensity_y[[g]]),length(ldensity_y[[g]]):1,1   )]/(rangedensity_y[2]))*(c(rep(1,length(ldensity_y[[g]])),rep(-1,length(ldensity_y[[g]])),1  )))
					,col = col[g]
					,border = border[g]
					,lwd = 2
					)
			temp_med <- median(y[which(group == labels[g])],
							   na.rm = T)
			lines(x = g + c(-.5,.5),y = rep(temp_med,2),lty = 1, lwd = 2, col = border)
			if(med.text)
				text(g,temp_med,'median',pos = 3)
			if(par('ylog')){
				y.text<-par('usr')[3] + .05*diff(par('usr')[3:4])
				y.text<-10^y.text
				# number of readins falling below 0 [ since log(x) for x < 0 is undefined ]
				nbelow <- sum((y [group == labels[g]]) <= 0 )
				if(nbelow > 0)
					text(x = g, y = y.text , paste(nbelow), col = 'red',cex = 1.5)
			}
			y.text <- par('usr')[3] + .95*diff(par('usr')[3:4])
			if(par('ylog'))
				y.text<-10^y.text
			nabove <- sum(is.infinite(y [group == labels[g]]) )
			if(nabove > 0)
				text(x = g, y = y.text, paste(nabove), col = 'red',cex = 1.5)
		}
	}
	abline(v = (1:(length(labels)-1)) + .5,
		   col = 'gray42',
		   lty = 'dotted')
	return()
}

