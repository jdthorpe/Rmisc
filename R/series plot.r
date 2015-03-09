# --------------------------------------------------------------------------------
# Programmer: Jason Thorpe
# Date        Tuesday, March 05, 2013 8:31:54 AM
# Language:   R (.r) Version 2.15.0
# Purpose:    makes the serial value plots as in Mc
# Comments:   Plot arguments are applied to individual points when 
# --------------------------------------------------------------------------------


#' Plot data in a series
#' 
#' Plot data with 'timeline' like series (not in fact an S3 method for class 'serial')
#' @param x the horizontal position of the points in the plot \strong{NOTE} 
#' that only one of x,y should be specified.  For horizontal layout, y, should be specified.
#' 
#' @param y the vertical position of the points in the plot \strong{NOTE} that 
#' only one of x,y should be specified.  For vertical layout, y, should be specified.
#' 
#' @param id a grouping variable which indicates the points that will appear 
#' in the same series.  (similar to the argument INDEX of \code{link[base]{tapply}})
#' 
#' @param if TRUE, the figure is added to an existing plot allowing you plot 
#' lines and then add labels 
#' 
#' @param ax.labels = labels of the individual series.  note that can be 
#' used to arrange or filter the data
#' @param bg.panel color for background panels
#' @param sep.col = 'gray42', color of the lines that separate the rows (columns). (default= 'gray42')
#' @param sep.lty = 2, type of the the lines that separate the rows (columns). (default= 2)
#' @param sep.lwd = 1, width of the the lines that separate the rows (columns). (default= 1)
#' @param xlab see \code{link[base]{plot}} for details
#' @param ylab see \code{link[base]{plot}} for details
#' @param log see \code{link[base]{plot}} for details
#' @param axes see \code{link[base]{plot}} for details
#' @param add see \code{link[base]{plot}} for details
#' @param main see \code{link[base]{plot}} for details
#' @param xlim see \code{link[base]{plot}} for details
#' @param ylim see \code{link[base]{plot}} for details
#' @param m.text see \code{link[base]{plot}} for details
#' @param m.side=2 see \code{link[base]{plot}} for details
#' @param fun,# optional custom plotting function
#' @param ...
#' 
#' @note Plot arguments are applied to individual points when 
#' length(arg) == length(id).  Otherwise, the values are 
#' cycled between rows (columns) of the figure.
#' 
#' @examples
#' m = 20;n=3
#' x = rnorm(m*n)
#' id = letters[rep(1:m,n)]
#' pch = sample(LETTERS,m*n,replace=T)
#' data.frame(x,id,pch)[id=='a',]
#' data.frame(x,id,pch)[id=='r',]
#' 
#' plot.serial(x = x,
#' 			id = id,
#' 			type='b',
#' 			pch = pch,# argument applied to inividual points when length(arg) == length(y)
#' 			cex = 1.2,# applied to all values
#' 			col = c('black','brown3','dodgerblue3'),# cycle through values with length(arg) != length(y)
#' 			bg = c(NA,'gray55'),# cycle through values with length(arg) != length(y)
#' 			main ='foo',
#' 			xlab = 'Value',
#' 			ylab = 'ID') 
#' 
#' # --------------------------------------------------
#' # veritical orientation + filtering via ax.labels parameter
#' # --------------------------------------------------
#' plot.serial(y = x,
#' 			id = id,
#' 			ax.labels = letters[1:13*2], # filter and orginize the rows (columns)
#' 			type='b',
#' 			pch = pch,# argument applied to inividual points when length(arg) == length(y)
#' 			cex = 1.2,# applied to all values
#' 			bg = c('green','gray55'),# cycle through values with length(arg) != length(y)
#' 			main ='foo',
#' 			xlab = 'ID',
#' 			ylab = 'Value') 
#' 
#' # --------------------------------------------------
#' # an ugly but illustrative use of 'add = T'
#' # --------------------------------------------------
#' plot.serial(x = x,
#' 			id = id,
#' 			type='b',
#' 			pch = NA,
#' 			col = c('black','brown3','dodgerblue3'),# cycle through values with length(arg) != length(y)
#' 			bg = c(NA,'gray55'),# cycle through values with length(arg) != length(y)
#' 			xlab = 'Value',
#' 			ylab = 'ID') 
#' 
#' plot.serial(x = x,
#' 			id = id,
#' 			add=T,
#' 			labels = paste(pch,tolower(pch)),# argument applied to inividual points when length(arg) == length(y)
#' 			cex = 1.2,# applied to all values
#' 			col = c('black','brown3','dodgerblue3'),# cycle through values with length(arg) != length(y)
#' 			) 
#' 




plot.serial<- function(x,y,id,
					 # axis labels (can be used to filter the data)
					 ax.labels = if(inherits(id,'factor')) levels(id) else unique(id),
					 # color for background panels
					 bg.panel,
					 # separattor line paramters
					 sep.col = 'gray42',
					 sep.lty = 2,
					 sep.lwd = 1,
					 # standard plot arguments
					 xlab =deparse(substitute(x)),# standard default plot labels
					 ylab =deparse(substitute(y)),# standard default plot labels
					 log = '',
					 axes =T,
					 add=F,
					 main,
					 xlim,
					 ylim,
					 m.text,
					 m.side=2,
					 fun,# optional custom plotting function
					 ...) {
	#VALIDATE THE INPUTS
	stopifnot(xor(missing(x),missing(y)))
	if(missing(x)){
		stopifnot(length(y) == length(id))
		labelSide <- 1
		valueSide <- 2
		values <- y
		x <- as.integer(factor(id,levels=ax.labels))
		xlim <- c(1,length(ax.labels))
		if(missing(ylim))
			ylim <- range(if(!identical(grep('y',log),integer(0))) log(values) else values,na.rm=T)
		if( (!identical(grep('y',log),integer(0))) & (min(values, na.rm = T) <= 0))
			message('WARNING: negative values and zeros are excluded from plot.')
	}else{
		stopifnot(length(x) == length(id))
		labelSide <- 2
		valueSide <- 1
		values <- x
		y <- as.integer(factor(id,levels=ax.labels))
		ylim <- c(length(ax.labels),1)
		if(missing(xlim))
			xlim <- range(if(!identical(grep('x',log),integer(0))) log(values) else values,na.rm=T)
		if( (!identical(grep('x',log),integer(0))) & (min(values, na.rm = T) <= 0))
			message('WARNING: negative values and zeros are excluded from plot.')
	}
	#SETUP THE PLOT
	if(!add){
		if(!missing(m.text)){
			if(!m.side %in% (1:2*2)) 
				stop('unsupported m.side value: ',m.side)
			# w is the width as a percentage of the window ( usr[1:2])
			curDev <- dev.cur()
			if(names(curDev) == 'windows'){
				usr <- par('usr')
				# maximum string width (preserving sign)
				w <- (max(abs(strwidth(m.text)))*(abs(diff(usr[1:2]))/diff(usr[1:2])))
				# width as a percentage of the plotting region
				w <- (w / diff(usr[1:2])
					  + 0.02 )# add a margin
				rm(usr)
			}else if(names(curDev) == 'pdf') {
#-- 				browser()
				dl <- dev.list()
				stopifnot('windows' %in% names(dl) )
				dev.set(dl[which(names(dl) == 'windows')])
				windowsFonts(Helvetica=windowsFont('TT Helvetica'))
				usr <- par('usr')
				# maximum string width (preserving sign)
				w <- (max(abs(strwidth(m.text,family='Helvetica')))*(abs(diff(usr[1:2]))/diff(usr[1:2])))
				# width as a percentage of the plotting region
				w <- (w / diff(usr[1:2])
					  + 0.02 # add a margin
					)#*1.05 # text is slightly larger on the pdf
				dev.set(curDev)
				rm(curDev,dl,usr)
			} else {browser();stop('unsupported device');}
			# w as a percentage of xlim
			w <- w * 1.08
			#inflate the width becuase it's is in user units which change with the re-plotting
			xlim1 <- xlim
			if(m.side == 2)
				xlim[1] <- xlim[1] - diff(xlim)*w/(1-w)
			else
				xlim[2] <- xlim[2] + diff(xlim)*w/(1-w)
		}
		plot(x = NA, y = NA,
			 xlim=xlim,ylim=ylim, 
			 xlab=xlab,ylab=ylab, main=main,
			 axes = F, log = log, type = 'n')

#-- 		browser()
		# BACKGROUND COLORS
		if(!missing(bg.panel)) {
			usr <- par('usr')
			if(par('xlog')) usr[1:2] <- 10^(usr[1:2])
			if(par('ylog')) usr[1:2] <- 10^(usr[3:4])
			if(valueSide==1){
				valpts <- usr[c(1,2,2,1)]
				for(i in 1:length(ax.labels))
					polygon(x = valpts,
							y = if(i == 1) 
									c(usr[4],usr[4],i + .5,i + .5)
								else if(i == length(ax.labels))
									c(i - .5,i - .5,usr[3],usr[3])
								else 
									c(i - .5,i - .5,i + .5,i + .5),
							col = bg.panel[(i-1)%%length(bg.panel) + 1],
							border = NA)
				# lines between sections
				abline(h = (1:(length(ax.labels)-1)) + .5,
					   col = sep.col,
					   lty = sep.lty,
					   lwd = sep.lwd)
			}else{
				valpts <- usr[c(3,4,4,3)]
				for(i in 1:length(ax.labels))
					polygon(x = if(i == 1) 
									c(usr[1],usr[1],i + .5,i + .5)
								else if(i == length(ax.labels))
									c(i - .5,i - .5,usr[2],usr[2])
								else 
									c(i - .5,i - .5,i + .5,i + .5),
							y = valpts,
							col = bg.panel[(i-1)%%length(bg.panel) + 1],
							border = NA)
				# lines between sections
				abline(v = (1:(length(ax.labels)-1)) + .5,
					   col = sep.col,
					   lty = sep.lty,
					   lwd = sep.lwd)
			}
		}
		# AXIS ax.labels
		if(axes){
			box()
			axis(valueSide)
			axis(side = labelSide
				,at = 1:length(ax.labels)
				,labels = ax.labels)
		}
	}
	else if(!missing(m.text))
		warning("ignoring 'm.text' paramter")
	
	# PUT THE DATA ONTO THE PLOT
	ptArgs <- list(...)
	for(i in 1:length(ax.labels)){
		if(!missing(m.text)){
			xlim1
			if(m.side == 2)
				xx <-xlim1[1] - diff(usr[1:2])*0.01 -(max(abs(strwidth(m.text)))*(abs(diff(usr[1:2]))/diff(usr[1:2])))
			else
				xx <-xlim1[2] + diff(usr[1:2])*0.01 
			text(x = xx,
				 y = i,
				 pos = 4,
				 m.text[which(id == ax.labels[i])][1])
		}
		indx <- order(values[which(id == ax.labels[i])])
		if(!length(indx)) # no data with this index
			next 
		tmpArgs <- lapply(ptArgs,function(x)if(inherits(x,'matrix')){
						  						if(nrow(x) == length(values) )
#-- 													tryCatch(
															 matrix(matrix(x[which(id == ax.labels[i]),],
																		   ncol=ncol(x)) [indx,], 
																	ncol=ncol(x))
#-- 														error=function(w)browser())
												else 
													matrix(x[(i-1)%%length(x) + 1,],ncol=ncol(x)) 
											} else if(length(x) == length(values)) 
												# filter arguments intended for individual points
												x[which(id == ax.labels[i])] [indx]
											else # cycle throuhg other options
												x[(i-1)%%length(x) + 1] 
											)
		tmpArgs$x <- x[which(id == ax.labels[i])][indx]
		tmpArgs$y <- y[which(id == ax.labels[i])][indx]
		if(!missing(fun))
			do.call(fun,tmpArgs)
		else if('labels' %in% names(tmpArgs))
			do.call(text,tmpArgs)
		else
			do.call(points,tmpArgs)
	}
}





