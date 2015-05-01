# --------------------------------------------------------------------------------------
# Programmer: Jason Thorpe
# Date        08/26/2010
# Language:   R (Version 2.6.0)
# Purpose:    Make Venn Diagrams to Summarize 1 to 4 logical variables
# Comments:   
# --------------------------------------------------------------------------------------


#' Plot Venn Diagrams
#'
#' Make Venn Diagrams to Summarize 1 to 4 indicator (logical) variables
#' @export
#' @param data a \code{data.frame} containing 1 to 4 indicator (logical) variables
#' @param main the title of the figure
#' @param labels labels for the indicator variables (defaults to names(data))
#' @param ntext a label to describe objects that are \code{FALSE} for every indicator variable (default = 'All Others')
#' @param add If \code{TRUE}, the digram is added to an existing plot
#' @param va a typesetting parameter
#' @param col symbol color
#' @param col.txt text color
#' @param lty line type 
#' @param prefix I can't remember
#' @param suffix  I can't remember
#' @examples
#' venn( data.frame(A = rnorm(1000)>0
#' 					 ,B = rnorm(1000)>0
#' 					 ,C = rnorm(1000)>0
#' 					 ,D = rnorm(1000)>0
#' 					 ))

venn <- function(data,
				 main='',
				 labels=dimnames(data)[[2]],
				 ntext='All Others',
				 add=FALSE,
				 va=0,
				 col,
				 col.txt,
				 lty,
				 prefix='',
				 suffix = ''){
	stopifnot(inherits(data,'data.frame'))
	if(ncol(data)>4)
		stop('parameter "data" must have 4 or fewer columns')
	if(length(labels)!=ncol(data))
		stop("length of 'labels' must equal number of columns in 'data'")
	nclasses <- ncol(data)
	
	
if(nclasses < 4){
	#CALCULATE PARAMETERS
	if(missing(col))
		col <- rainbow(4)[nclasses]
	if(missing(col.txt))
		col.txt <-'black'
	
	#get the plotting parameters
	radii <- c(0.6,0.55,0.5)[nclasses]
	yadj  <- c(0,0,0.1)[nclasses]
	radians <- 2*((0:(nclasses-1))/nclasses + ((1/2)^(nclasses - 1)))
	center_x <- (c(0,0.35,0.35)[nclasses])*cos(radians*pi)
	center_y <- (c(0,0.35,0.35)[nclasses])*sin(radians*pi) - (yadj)
	
	if(!add){# make the figure
		plot(0,0
			,xlim = c(-1,1)
			,ylim = c(-1,1)
			,axes = FALSE
			,main=main
			,xlab=''
			,ylab=''
			,type='n'
			)
		box()
	}#END: make the figure
	
	#make the venn symbols
	graphics::symbols( x= center_x
					 , y= center_y 
					 , circles=rep(radii,nclasses)
					 , add = TRUE
					 , inches = FALSE
					 , fg = col
					 , lwd = 3
					 )
	
	#print the numbers
	if(nclasses %in% 1:2){
		text(x = center_x  
			,y = center_y  + 1.3*radii
			#,y = center_y #+ rep(radii,nclasses)*1.3*(-1)^(((0:(nclasses-1))/nclasses + ((1/2)^(nclasses - 1))) > 1/2)
			,labels#LETTERS[1:nclasses]
			, cex = 1.5
			)	
	}else{ #nclasses %in% 3
		text(x = center_x  + 1.2*radii*cos(radians*pi)
			,y = center_y  + 1.3*radii*sin(radians*pi)
			#,y = center_y #+ rep(radii,nclasses)*1.3*(-1)^(((0:(nclasses-1))/nclasses + ((1/2)^(nclasses - 1))) > 1/2)
			,labels#LETTERS[1:nclasses]
			, cex = 1.5
			)	
	}
	(table_data <- table(data))#table(data[,1:nclasses]))
	
	# PRINT THE LABLES
	
	#perm <- gtools::permutations(2,nclasses,repeats=TRUE)
	perm <- as.matrix(rev(do.call(expand.grid,rep(list(1:2),nclasses))))
	for(i in 2:(2^nclasses)){
		(x_text <- mean(center_x[perm[i,] == 2])*(c(1,1.3,1.7,2)[nclasses]))
		(y_text <- mean(center_y[perm[i,] == 2])*(c(1,1.3,1.7,2)[nclasses]) + va) 
		text(x = x_text,y = y_text
			 ,paste(prefix,table_data[array(perm[i,],dim = c(1,nclasses))],suffix,sep = '')
			 , cex = 1.5
			 , col = col.txt
			 )
		#print(table_data[array(perm[i,],dim = c(1,nclasses))])
	}
	text(0,-0.98 + va
		,paste(prefix,table_data[array(rep(1,nclasses),dim=c(1,nclasses))],suffix,ntext,sep = '')
		, cex = 1.2
		, col = col.txt
		)
	invisible(list(radii=radii, center_x=center_x,center_y=center_y))

}else{
	#CALCULATE PARAMETERS
	if(missing(col)){col <-c(NA,"#88888880","#88888880",NA) }
	if(missing(lty)){lty <- c(1,2,2,1)}
	if(missing(col.txt)){col.txt <- rep('black',4)}
	
	f <- numeric(0)
	f[1] = graphics::strheight(labels[1],cex = 1.3,units = 'inches')/par('pin')[2]
	f[2] = graphics::strheight(labels[2],cex = 1.3,units = 'inches')/par('pin')[2]
	f[3] = graphics::strwidth( labels[3],cex = 1.3,units = 'inches')/par('pin')[1]
	f[4] = graphics::strwidth( labels[4],cex = 1.3,units = 'inches')/par('pin')[1]
	f[5] = graphics::strwidth( ntext,cex = 1.3,units = 'inches')/par('pin')[1]
	
	if(!add){# make the figure
		plot(0,0
			,ylim = c(0,max(c(1.22 # graphics::strwidth('case',cex = 1.3,units = 'inches')/(par('pin')[1])
							,1.15/(1-(1.08*f[1])) 
							,1.05/(1-(1.08*f[2]))
							)))
			,xlim = c(0,max(c(1.25
							 ,1.05/(1-(1.08*f[3]))
							 ,1.15/(1-(1.08*f[4]))
							 ,1.04/(1-(1.08*f[5]))
							 )))
			,axes = FALSE
			,main=main
			,xlab=''
			,ylab=''
			,type='n'
			)
		box()

		#make the venn symbols
		rectangle(x = c( 0.00,0.50)	,y = c(0.04 ,1.15) ,lwd = 3,lty =lty[1], col = col[1])
		rectangle(x = c( 0.25,0.75)	,y = c(0.015,1.05) ,lwd = 3,lty =lty[2], col = col[2])
		rectangle(y = c( 0.25,0.75)	,x = c(0.02 ,1.05) ,lwd = 3,lty =lty[3], col = col[3])
		rectangle(y = c(-0.01,0.50)	,x = c(0.04 ,1.15) ,lwd = 3,lty =lty[4], col = col[4])

		rectangle(x = c( 0.00,0.50)	,y = c(0.04 ,1.15) ,lwd = 3,lty =lty[1])
		rectangle(x = c( 0.25,0.75)	,y = c(0.015,1.05) ,lwd = 3,lty =lty[2])
		rectangle(y = c( 0.25,0.75)	,x = c(0.02 ,1.05) ,lwd = 3,lty =lty[3])
		rectangle(y = c(-0.01,0.50)	,x = c(0.04 ,1.15) ,lwd = 3,lty =lty[4])

	}
	
	#get the plotting parameters
	vec <- c(0.13,0.375,0.625,0.875)
	temp <- table(data)
	TF <- c('TRUE','FALSE')
	tf <- c(TRUE,FALSE)
	#print the numbers
	for(i1 in 1:2)
	for(i2 in 1:2)
	for(i3 in 1:2)
	for(i4 in 1:2)	
	text( paste(prefix,temp[TF[i1],TF[i2],TF[i3],TF[i4]],suffix,sep = '')
		, x = (list(vec[1:2],vec[3:4])[[i1]])[1 + ((tf[i1]&tf[i2])|((!tf[i1])*(!tf[i2])))]
		, y = (list(vec[1:2],vec[3:4])[[i4]])[1 + ((tf[i4]&tf[i3])|((!tf[i4])*(!tf[i3])))] + va
		, cex = 1.3
		, col = col.txt
		)
	
	text(ntext,x=1.04,y = 0.875, pos = 4, cex = 1.3)
	# print the lables
	text(labels[1],x = 0.25,y = 1.15, pos = 3, cex = 1.3)
	text(labels[2],x = 0.5 ,y = 1.1, pos = 4, cex = 1.3)
	text(labels[3],x = 1.05+0.03 + (graphics::strwidth(labels[3],cex = 1.3)/2),y = 0.625, cex = 1.3)
	text(labels[4],x = 1.15+0.03 + (graphics::strwidth(labels[4],cex = 1.3)/2),y = 0.25, cex = 1.3)
	
}

}

#make a rectangle from the x and y limits
rectangle <- function(x,y,...){
	polygon( x[c(1,1,2,2,1)]
			,y[c(1,2,2,1,1)]
			,...)
}


