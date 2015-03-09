# --------------------------------------------------------------------------------
# color related functions
# --------------------------------------------------------------------------------

#' Choose from among the named colors in R 
#' Choose from among the named colors in R using a graphical interface
#'
#' @param N the number of colors to chose
#' @param alpha (optional) an integer between 1 and 255 with the alpha chanel
#' @export
#' @examples
#' cat_function()
colorchoose<-function(n = 1,alpha,term=F){
	termcols <- c('black', 'blue', 'brown', 'cyan', 'darkBlue',
			'darkcyan', 'darkgray', 'darkgreen', 'darkgrey',
			'darkmagenta', 'darkred', 'darkyellow', 'gray',
			'green', 'grey', 'lightblue', 'lightcyan',
			'lightgray', 'lightgreen', 'lightgrey', 'lightmagenta',
			'lightred', 'magenta', 'red', 'white', 'yellow')
	cex = 2.4
	cols<-colors()
	if(term){
		cols <- cols[cols %in% termcols]
		cat('also available:', paste(termcols[!termcols %in% cols], collapse = ', '),'\n')
		cex <- 5
	}
	mod <- ceiling(sqrt(length(cols)))
	plot(xlab = '',
		 ylab = '',
		 main = 'click for color name',
		 c(0,mod ),
		 c(0,mod ),
		 type = 'n',
		 axes = F)
	for(i in 1:length(cols))
		points(i %% mod,
			   i %/% mod,
			   col = cols[i],
			   pch = 15,
			   cex = 2.4)
	p<-locator(n)
	#GET THE cols NAMES
	cols <- cols[round(p$y)*mod + round(p$x)]
	#ADD THE ALPHA CHANNEL IF REQUESTED
	if(!missing(alpha))
		cols <- col2hex(cname=cols,alpha=alpha)
	return(cols)
}

#' Invert a color 
#'
#' Invert a color.
#' @param x the colors to invert
#' @param intensity (optional) around which to invert the individual R,G, and B values
#' @export
inv.col <- function(x, intensity=255){
	type <- substr(x,1,1) == '#'
	r <- hex2n(substr(x,1+type,2+type))
	g <- hex2n(substr(x,3+type,4+type))
	b <- hex2n(substr(x,5+type,6+type))

	#invert the chanels
	ir <- intensity - r
	ig <- intensity - g
	ib <- intensity - b

	#scale to the same total ('is' = inverted and scaled)
	isr <- pmin(floor( ir*(r+b+g) / (ir+ib+ig)),255)
	isg <- pmin(floor( ig*(r+b+g) / (ir+ib+ig)),255)
	isb <- pmin(floor( ib*(r+b+g) / (ir+ib+ig)),255)

	rh <- as.hexmode(isr)
	gh <- as.hexmode(isg)
	bh <- as.hexmode(isb)
	toupper(paste(iif(type,'#',''),rh,gh,bh,substr(x,7+type,10),sep = ''))
}
#plot(rep(1:3,times = 2),rep(1:2,each = 3),col = c(x,inv.col(x)),pch = 16, cex = 5)

hex2n <- function(string){
	hex2n_1 <- function(string){
		string <- toupper(string)
		n <- 0
		for(i in 1:nchar(string))
			n <- n + ((16^(nchar(string)-i))*
					  ((0:15)[match(substr(string,i,i),c(paste(0:9),LETTERS[1:6]) )]	))
		return(n)
	}
	n <- numeric(length(string))
	for(i in 1:length(string))n[i] <- hex2n_1(string[i])
	return(n)
}

colorMean <- function(col1,col2,
					  alpha=1,
					  weight1 = 0.5,
					  weight2=1-weight1){
	stopifnot((length(col1) == 1)|
			  (length(col2) == 1)|
			  (length(col1) == length(col2)))
	if(length(col1) == 1)
		col1 <- rep(col1,length(col2))
	if(length(col2) == 1)
		col2 <- rep(col2,length(col1))

	# convert the colors to hex
	col1 <- col2hex(col1)
	col2 <- col2hex(col2)

	type <- substr(col1,1,1) == '#'
	r1 <- hex2n(substr(col1,1+type,2+type))
	g1 <- hex2n(substr(col1,3+type,4+type))
	b1 <- hex2n(substr(col1,5+type,6+type))
	a1 <- hex2n(substr(col1,7+type,8+type))
	a1[is.na(a1)] <- floor(alpha*255)

	type <- substr(col2,1,1) == '#'
	r2 <- hex2n(substr(col2,1+type,2+type))
	g2 <- hex2n(substr(col2,3+type,4+type))
	b2 <- hex2n(substr(col2,5+type,6+type))
	a2 <- hex2n(substr(col2,7+type,8+type))
	a2[is.na(a2)] <- floor(alpha*255)

    return(rgb(red   = (weight1*r1 + weight2*r2)/((weight1 + weight2)*255),
			   green = (weight1*g1 + weight2*g2)/((weight1 + weight2)*255),
			   blue  = (weight1*b1 + weight2*b2)/((weight1 + weight2)*255),
			   alpha = if(missing(alpha)) (weight1*a1 + weight2*a2)/((weight1 + weight2)*255) else alpha))
}

#GET A SPECTRUM OF COLORS
#rainbow2<-function(n = 2)
#{
#	alpha = (0:(n - 1))/(n - 1)
#	c1 = col2rgb(colorchoose())
#	c2 = col2rgb(colorchoose())
#	temp<-	cbind(c2)%*%rbind(alpha) + cbind(c1)%*%(1-rbind(alpha))
#	temp<-temp/256
#	rgb(r = temp[1,], g =  temp[2,],b =  temp[3,])
#}
rainbow2<-function(n = 2, alpha = NA, col = NA){
	if(identical(alpha,NA)) {
		alpha = (0:(n - 1))/(n - 1)
		n = length(alpha)
	}else{
		alpha = ((alpha - min(alpha))/(max(alpha) - min(alpha)))
	}
	if(identical(col,NA))
		col = col2rgb(colorchoose(2))
	else
		col = col2rgb(col)

	temp<-cbind(col[,2])%*%rbind(alpha) + cbind(col[,1])%*%(1-rbind(alpha))
	temp<-temp/256
	rgb(r = temp[1,], g =  temp[2,],b =  temp[3,])
}

#-- rainbow3 <- function(...){
#-- 	message( 'rainbow3() has been deprecated, please use rainbow() instead')
#-- 	rainbow(...)
#-- }
#-- 
#-- rainbow<-function(n,...){
#-- 	if(length(list(...)) || (n > 12 ))
#-- 		return(grDevices::rainbow(n))
#-- 	cols <- c(
#-- 		"brown3" ,
#-- 		"brown2" ,
#-- 		"darkorange1",
#-- 		"darkorange2",
#-- 		"goldenrod1" ,
#-- 		"goldenrod2" ,
#-- 		"green3",
#-- 		"green4",
#-- 		"dodgerblue2",
#-- 		"dodgerblue3",
#-- 		"violet",
#-- 		"darkviolet")
#-- 	if(n == 1)return(cols[2*c(1)])
#-- 	if(n == 2)return(cols[2*c(1,5)])
#-- 	if(n == 3)return(cols[2*c(1,4,5)])
#-- 	if(n == 4)return(cols[2*c(1,3,4,5)])
#-- 	if(n == 5)return(cols[2*c(1,2,3,4,5)])
#-- 	if(n == 6)return(cols[2*c(1,2,3,4,5,6)])
#-- 	if(n == 1 + 6)return(cols[sort(c(2*1:6,1))])
#-- 	if(n == 2 + 6)return(cols[sort(c(2*1:6,1,4))])
#-- 	if(n == 3 + 6)return(cols[sort(c(2*1:6,1,4,5))])
#-- 	if(n == 4 + 6)return(cols[sort(c(2*1:6,1,3,4,5))])
#-- 	if(n == 5 + 6)return(cols[sort(c(2*1:6,1,2,3,4,5))])
#-- 	if(n == 6 + 6)return(cols[sort(c(2*1:6,1,2,3,4,5,6))])
#-- }

col2hex <- function (cname,alpha = 1,overridealpha = T) {
	if(length(alpha) == 1)
		alpha <- rep(alpha,length(cname))

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
		cname[lhex] <- paste(cname[lhex],
						 as.hexmode(round(alpha[lhex]*255)),
						 sep = '')

	#OVERRIDE THE ALPHA VALUES
	if(length(lalpha))
	cname[lalpha] <- paste(substr(cname[lalpha],0,7)
						  ,as.hexmode(round(alpha[lalpha]*255))
						  ,sep = ''
						  )

	return(cname)
}

