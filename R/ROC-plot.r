# --------------------------------------------------------------------------------------
# Programmer: Jason Thorpe
# Date        06/22/2010
# Language:   R (Version 2.6.0)
# Purpose:    plots an roc curve and calculates many statistics
# Dependancies: The package ROCR
# Comments:   
# --------------------------------------------------------------------------------------

#' Calculate the Area under an ROC curve
#'
#' Calculate the Area under an ROC curve.
#' @param x A vector of marker values
#' @param case A logical vector where TRUE indicates that the corresonding marker value in X
#' @export
#' @examples
#' AUC( x = rnorm(100) + rep(c(1,0),50), case = rep(c(TRUE,FALSE),50))
AUC <- function(x,case)
	sum(outer(x[case],x[!case],`>`))/(sum(case)*sum(!case))



#' Plot ROC curves and calculate sensitivity / specificity statistics
#'
#' Plot ROC curves and calculate sensitivity / specificity statistics.
#' @param x A vector of marker values
#' @param cases A logical vector where TRUE indicates that the corresonding marker value in X
#' is taken from a 'Case', and FALSE otherwise. 
#' @param comparison.label A Sting with a label for the comparison printed above the 
#' included in the plot sensitivities and specifities 
#' @param add If TRUE, the ROC curve is added to an existing plot. (default = FALSE)
#' @param color The color of the ROC curve, or 'colorize' if a color scale is used to color 
#' the curve. (default = 'colorize')
#' @param print If TRUE, the comparison label, AUC, and/or sensitiviies are added to the fugure. (default = plot) # print a summary of the values
#' @param print.auc If TRUE, the AUC is added to the fugure. ,
#' @param print.col Color of the text added to the figure (default blue, or 'col' if specified')
#' @param print.loc location of the text added to the fugure. (default = c(0.5,0.5))
#' @param spec Specificities for which sensitivities should be included in the figure and in the 
#' list that is returned (invisibly) by plot.roc (default = c(1.00,0.98,0.95,0.90))
#' @param lwd Line width of the ROC curve. (default = 3)
#' @param plot If TRUE, the ROC cuve is plotted. (default = TRUE) ('plot=FALSE' is handy for setting up a figure)
#' @param summarize If TRUE, a list of all possible sensitivities and specificities is included
#' in the list returned (invisibly) by plot.roc (default = FALSE)
#' @param xlab x-axis label (default = 'False Positive Rate')
#' @param ylab y-axis label (default = 'True Positive Rate')
#' @param cex.text Character expansion parameter for displayed text (default = 1.2)
#' @param cex.label Character expansion parameter for the 'label' parameter (default = cex.text) ,
#' @param gridlines If set to TRUE or a numeric vector, gridlines are displayed
#' @param threshold.args (optional) a list of arguments to \code{\link{threshold}}
#' @param units units of measurement of the marker (optional, printed in the right margin) 
#' @param digits number of decimals in the AUC and P-Value (default = 3)
#' @param digits.pvalue  number of decimals in P-Value (default = digits)
#' @param digits.auc  number of decimals in the AUC (default = digits)
#' @param conf.int Should confidence intervals be included? (default = FALSE)
#' @param conf.level confidence level (default = 0.95)
#' @param ... additional arguments plot()
#' @return a list, returned invisibly, containing the AUC, numbers 
#' of cases and controls, p-value for the AUC, the plot location of the 
#' printed text, and the sensitivities and specifities included in the 
#' figure, and optional a summary of the ROC curve (if summarize = TRUE).  
#' @export plot.roc
#' @examples
#' plot.roc( x = rnorm(100) + rep(c(1,0),50),
#' 		case = rep(c(TRUE,FALSE),50),
#' 		comparison.label = 'Cases v. Controls',
#' 		main = 'My Favorite Bio-Marker',
#' 		color = 'brown3',
#' 		print = TRUE,
#' 		print.col = 'green4',
#' 		print.loc = c(0.5,0.32),
#' 		lty = 3,
#' 		lwd = 5,
#' 		spec  = c(1.00,0.9,0.85,0.8),
#' 		xlab = 'False Positive Rate (x axis)',
#' 		ylab = 'True Positive Rate (y axis)',
#' 		gridlines = 1:10/10)			

plot.roc<-function( x, # a vector of marker values
					cases, # a logical vector with True indicating case and False indicating Control
					comparison.label = "",
					add = FALSE,
					color = 'colorize',
					print = plot, # print a summary of the values
					print.auc = print,
					print.col = if(color == 'colorize') 'blue' else color,
					print.loc = c(0.5,0.5),
					spec  = c(1.00,0.98,0.95,0.90),
					lwd = 3,
					plot = TRUE,
					summarize = FALSE,
					xlab = 'False Positive Rate',
					ylab = 'True Positive Rate',
					cex.text = 1.2,
					cex.label = cex.text,
					gridlines,
					threshold.args=NULL,
					units,  # units of measurement of the marker
					digits=3,
					digits.pvalue=digits,
					digits.auc=digits,
					conf.int = FALSE, 
					conf.level = 0.95,
					...) {

	if(inherits(x,'roc'))
		stop("Jmisc::plot.roc is not an S3 method")
#-- 	if(!require('ROCR',quietly = TRUE))
#-- 		stop('required package "ROCR" not installed')
	if(length(cases)!= length(x))
		stop('x and cases must have the same length')
	if(!is.logical(cases))
		stop('cases vector is not of type logicl')
	if(any(is.na(x))) {
		message('missing values are left out of x')
		cases<-cases[!is.na(x)]
		x<-as.numeric(x)
		x<-x[!is.na(x)]
	}
	print <- print | !missing(print.auc) | !missing(print.col) | !missing(print.loc)

	obj <- ROCR::prediction(x,factor(cases,levels=unique(cases)))
	auc <- (attributes(ROCR::performance(obj, measure="auc"))$y.value[[1]][[1]][1])
	if(plot){
		plot(ROCR::performance(obj
							, measure="tpr"
							, x.measure="fpr"
							)
			,colorize = identical(color , 'colorize')
			,lwd = lwd
			,add = add
			,col = c(NA,color)[1 + !identical(color , 'colorize')]
			,xlab = xlab
			,ylab = ylab
			,...)
		if(!missing(gridlines)) {
			if(identical(gridlines,TRUE))
				gridlines <- (0:5)/5
			abline(v = gridlines,lty = 2, col = 'gray',lwd = 2)
			abline(h = gridlines,lty = 2, col = 'gray',lwd = 2)
		}
		if(!missing(units))
			mtext(text = units, side = 4, line = -1.5)
	}


	sens.at.spec<-function(val,case,spec = .95){
		controls<-val[!case]
		controls<-controls[order(controls)]
		cutoff<- controls[ceiling(spec*length(controls))]
		sens<-mean(val[case]>cutoff)
		out<-list()
		out$specificity<-spec
		out$sensitivity<-sens
		out$cutoff<-cutoff
		out
	}
	

	AUC <- function(x,case)
		sum(outer(x[case],x[!case],`>`))/(sum(case)*sum(!case))
	# some defaults

	if(summarize) {# GIVE A FULL SUMMARY OF THE ROC CURVE
		roc_summary<-matrix(numeric(0),
							nrow = length(spec),
							ncol =3,
						   	dimnames = list(1:length(spec),
											   c('specificity','sensitivity','cutoff') ))
		for( i in 1:sum(!cases))
			roc_summary[i,]<-unlist(sens.at.spec(x,cases,i/sum(!cases)))
	}



	# ------------------------------------------------------------
	# calculate the sensitivities at various specificities
	# ------------------------------------------------------------
	if(length(spec)) {
		temp<-matrix(numeric(0),
					 nrow = length(spec),
					 ncol =3,
					 dimnames = list(1:length(spec),
									 c('specificity','sensitivity','cutoff') ))
		for( i in 1:length(spec))
			temp[i,]<-unlist(sens.at.spec(x,
										  cases,
										  spec[i]))
	}

	# ----------------------------------------
	# PRINT VARIOUS SUMMARY STATISTICS
	# ----------------------------------------
	threshold.args[['x']] <- x[!cases]
	threshold.args[['probs']] <- spec
	threshold.args[['case.values']] = x[cases]
	threshold.args[['quietly']] = TRUE
	cutoff<- do.call(threshold,threshold.args)

	if( print) {
		if(missing(print.loc) && !length(grep('pdf',names(dev.cur()))) ){
			message('Choose legend location with cursor or specify the text location with 
        plot.roc(...,print.loc = c(0.5,0.5))
')
			print.loc<-locator(1)
		}else if(is.numeric(print.loc) & (length(print.loc) == 2)) {
			print.loc<-data.frame(x = print.loc[1],y = print.loc[2])
		}
		# print the comparison label
		yoffset = 0
		if(!missing(comparison.label)){
			text(print.loc$x,
				print.loc$y,
				comparison.label,
				col = print.col,
				cex = cex.label ,
				pos = 4)
			yoffset <- 1.2*strheight(comparison.label,cex = cex.label)
		}
		STAT <- wilcox.test(x = x[cases], y = x[!cases])
		# get the p-value for the Wilcox test
		p.value <- STAT$p.value
		if(p.value > (10^-digits.pvalue)*0.5)
			p.value.text <- paste('=',tochar(p.value,digits.pvalue))
		else
			p.value.text <- paste('< 0.',paste(rep('0',digits.pvalue),collapse = ''),'5',sep = '')

		if(print.auc){
			if(conf.int){
				if(conf.level!=0.95)
					stop("conf.level must be 0.95 due to limitations of the pROC package")
				CI = ci.auc(roc(cases,x))
				# print the AUC, CI and p-value
				txt <- paste0('AUC = ', tochar(auc,digits.auc), 
							  ' [',tochar(CI[1],digits.auc),
							  ',' ,tochar(CI[3],digits.auc),']')
				text(print.loc$x,
					print.loc$y - yoffset,
					txt,
					col = print.col,
					cex = cex.label,
					pos = 4)
				yoffset <- yoffset + 1.3*strheight(txt,cex = cex.label)
				# print the p-value
				txt <- paste('p', p.value.text)
				text(print.loc$x,
					print.loc$y - yoffset,
					txt,
					col = print.col,
					cex = cex.label,
					pos = 4)
				yoffset <- yoffset + 1.3*strheight(txt,cex = cex.label)
			}else{
				# print the AUC and p-value
				txt <- paste0('AUC = ', tochar(auc,digits.auc), ' (p ', p.value.text, ')')
				text(print.loc$x,
					print.loc$y - yoffset,
					txt,
					col = print.col,
					cex = cex.label,
					pos = 4)
				yoffset <- yoffset + 1.3*strheight(txt,cex = cex.label)
			}
		}
		# print out the various sensitivities and specificities
		cwd =  1.1*strwidth('Specificity',cex = cex.text)
		if(length(spec)>0) {
			text(x = print.loc$x,
				 y = print.loc$y - yoffset,
				 'Specificity',
				 col = 'gray40',
				 cex = cex.text,
				 pos = 4)
			text(x = print.loc$x + cwd,
				 y = print.loc$y - yoffset,
				 'Sensitivity',
				 col = 'gray40',
				 cex = cex.text,
				 pos = 4)
			yoffset <- yoffset + 1.3*strheight('Specificity',cex = cex.text)
			for(r in 1:length(spec)) {
				text(x = print.loc$x,
					 y = print.loc$y - yoffset
					,tochar(temp[r,1],digits),
					,col = 'gray40',cex = cex.text,pos = 4)
				text(x = print.loc$x + cwd,
					 y = print.loc$y - yoffset,
					 tochar(temp[r,2],digits),# temp.text
					 col = 'gray40',
					 cex = cex.text,
					 pos = 4)
				yoffset <- yoffset + 1.3*strheight(tochar(temp[r,2],3),cex = cex.text)
			}
		}
	}
	out <- list(auc = auc
					,ncases = sum(cases)
					,ncontrols = sum(!cases)
					,p = wilcox.test(x = x[cases], y = x[!cases])$p.value
					,print.loc = print.loc
					)
	if(length(spec)){
					out[['sens']] = temp
					out[['spec']] = spec
	}

	if(summarize) {# GIVE A FULL SUMMARY OF THE ROC CURVE
		roc_summary<-matrix(numeric(0),
							nrow = sum(!cases),
							ncol =3,
						   	dimnames = list(1:sum(!cases),
											   c('specificity','sensitivity','cutoff') ))
		for( i in 1:sum(!cases))
			roc_summary[i,]<-unlist(sens.at.spec(x,cases,i/sum(!cases)))
	}


#-- 	if(summarize) {
#-- 		# INCLUDE A COMPLETE SUMMARY OF THE ROC CURVE IN THE OUTPUTS
#-- 		roc_summary<-matrix(numeric(0),
#-- 							nrow = sum(!cases),
#-- 							ncol =3,
#-- 						   	dimnames = list(1:sum(!cases),
#-- 											   c('specificity','sensitivity','cutoff') ))
#-- 		for( i in 1:sum(!cases))
#-- 			roc_summary[i,]<-unlist(.SENS.AT.SPEC(x,
#-- 												 cases,
#-- 												 i/sum(!cases)))
#-- 		out[['roc_summary']] <- roc_summary
#-- 	}

	invisible(out)
}




#-- .SENS.AT.SPEC <- function(val,
#-- 						 case,
#-- 						 spec = .95,
#-- 						 ...){
#-- 	controls<-val[!case]
#-- 	controls<-controls[order(controls)]
#-- 	cutoff<- controls[ceiling(spec*length(controls))]
#-- 	sens<- apply( outer(val[case],cutoff,function(x,y)x>=y),2,mean,na.rm = TRUE)
#-- 	out<-list()
#-- 	out$cutoff<-cutoff
#-- 	out$sens<-sens
#-- 	out
#-- }


#-- # the empirical sepecificy (not the estimated sensitivity)
#-- .SENS.AT.SPEC<-function(x,case,spec = .95){
#-- 	if(is.null(threshold.args)){
#-- 		controlLevels<-x[!case]
#-- 		controlLevels<-controlLevels[order(controlLevels)]
#-- 		cutoff<- controlLevels[ceiling(spec*length(controlLevels))]
#-- 	}else{
#-- 		threshold.args[['x']] <- x[!case]
#-- 		threshold.args[['probs']] <- spec
#-- 		threshold.args[['case.values']] = x[case]
#-- 		cutoff<- do.call(threshold,threshold.args)
#-- 	}
#-- 	sens<-mean(x[case]>cutoff)
#-- 	out<-list()
#-- 	out$specificity<-spec
#-- 	out$sensitivity<-sens
#-- 	out$cutoff<-cutoff
#-- 	out
#-- }



#' Plot a Beysian Expected Value curve
#' 
#' Plot a Beysian Expected Value curve, which is similar to an ROC curve, 
#' though maximum likelihood esimtates of the true and false positive rates
#' are used instead of the (biased) true and false positivity rates. In addition
#' confidence bands are included  beta distribution, which is the prior for 
#' the binomial distribution
#' @param x A vector of marker values
#' @param case A logical vector where TRUE indicates that the corresonding marker value in X
#' is taken from a 'Case', and FALSE otherwise. 
#' @param conf.level confidence level of the interval. (default = 0.95)
#' @param ci If TRUE, confidence bands are plotted. (default = TRUE)
#' @param col The color of the BEV curve. (default = 'black')
#' @param band logical. Should a confidence band be plotted?
#' @param col.band The color of the confidenc band, or 'colorize' if a color scale is used. (default = 'colorize')
#' @param add If TRUE, the ROC curve is added to an existing plot. (default = FALSE)
#' @param lwd Line width of the ROC curve. (default = 3)
#' @param xlab x-axis label (default = 'Estimated False Positive Rate')
#' @param ylab y-axis label (default = 'Estimated True Positive Rate')
#' @param xlim,ylim bounds for the x and y axes
#' @param ... additional arguments plot()

bev <- function(x,
				case,
				conf.level = 0.95,
				band = TRUE,
				ci = TRUE,
				col='black',
				col.band='colorize',
				add=FALSE,
				lwd = 3, 
				xlim=0:1,
				ylim=0:1,
				xlab = 'Estimated False Positve Rate',
				ylab = 'Estimated True Positive Rate',
				...){ # further arguments to plot()

	# ----------------------------------------------------------
	# a couple of dependencies
	# ----------------------------------------------------------
	col2hex <- function (cname,alpha = 1,overridealpha = TRUE) {
		if(length(alpha) == 1){alpha <- rep(alpha,length(cname))}
		lhex <- which( (nchar(cname)== 7)
						 & (substr(cname,0,1) == '#')
						 )
		lcol <- cname %in% colors()
		lalpha <-  which( (nchar(cname)== 9)
						 & (substr(cname,0,1) == '#')
						 & (substr(cname,8,9) == 'FF')
						 & rep(overridealpha,length(alpha))
						 )
		#process the color names
		colMat <- col2rgb(cname[lcol])
		cname[lcol] <- rgb(red  = colMat[1, ]/255
						  ,green = colMat[2, ]/255
						  ,blue  = colMat[3, ]/255
						  ,alpha = alpha[lcol]
						  )
		#process the hex values
		if(length(lhex))
		cname[lhex] <- paste(cname[lhex],as.hexmode(round(alpha[lhex]*255)),sep = '')
		#OVERRIDE THE ALPHA VALUES
		if(length(lalpha))
		cname[lalpha] <- paste(substr(cname[lalpha],0,7)
							  ,as.hexmode(round(alpha[lalpha]*255))
							  ,sep = ''
							  )
		return(cname)
	}
	#make a rectangle from the x and y limits
	rectangle <- function(x,y,...)
		polygon( x[c(1,1,2,2,1)]
				,y[c(1,2,2,1,1)]
				,...)
	# ----------------------------------------------------------
	# a couple of dependencies
	# ----------------------------------------------------------

	cases <- x[case]
	controls <- x[!case]
	xs <- sort(unique(x))
	thresholds <- c(-Inf,(xs[-length(xs)] + xs[-1])/2,Inf)

	specMx <- sensMx <- matrix(NA,length(xs) + 1,3)

	for(i in 1:length(thresholds)){
		specMx[i,] <- c(qbeta(c((1-conf.level)/2,1-((1-conf.level)/2))
							  ,sum(controls > (thresholds[i])) + 1
							  ,sum(controls < (thresholds[i])) + 1
							  )
						,(sum(controls > (thresholds[i])) + 1)/(length(controls)+1)
						)
		sensMx[i,] <- c(qbeta(c((1-conf.level)/2,1-((1-conf.level)/2))
						,sum(cases > (thresholds[i])) + 1
						,sum(cases < (thresholds[i])) + 1
						)
						,(sum(cases > (thresholds[i])) + 1)/(length(cases)+1)
						)
	}

	if(!add)
		plot(0,0,
			 xlim=xlim,
			 ylim=ylim,
			 type = 'n',
			 xlab = 'Estimated False Positve Rate',
			 ylab = 'Estimated True Positive Rate',
			 ...)
 	if(band){
		if(col.band == 'colorize')
			cols <- col2hex(rainbow(length(xs) + 1),alpha = 0.7)
		else
			cols <- rep(col.band[1],length(xs) + 1)

		for(i in 1:(length(xs) + 1)){
			rectangle( x = specMx[i,1:2]
					  ,y = sensMx[i,1:2]
					  ,col = cols[i]
					  ,border = NA
					  )

		}
#		plot.roc(x,case,print = FALSE,col = 'black', add = TRUE)
	}
	lines(x = specMx[,3],y = sensMx[,3],lty = 1, lwd = lwd, col = col)
	if(ci){
		lines(x = specMx[,3],y = sensMx[,2],lty = 3, lwd = lwd, col = col)
		lines(x = specMx[,3],y = sensMx[,1],lty = 3, lwd = lwd, col = col)
	}
	pbeta(c(0.025,0.975),1,1)
}

