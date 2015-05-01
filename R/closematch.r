# --------------------------------------------------------------------------------------
# Programmer: Jason Thorpe
# Purpose:    nearest neightbor matching
# --------------------------------------------------------------------------------------

#' One-To-One Nearest Neighbor Matching
#'
#' One-To-One Nearest Neighbor Matching.
#'
#' @param x,y values on which to match individul records (similar to \code{match(x,y)})
#' @param id.x,id.y If provided, x and y are matched within group. 
#' @param d Maximum distance between points for a match to succeed (default= Inf)
#' @param nomatch Value assigned to non-matched controls (default NA_integer_)
#' @param debug If provided, the object returned by \code{closematch()} includes
#' @param oneToOne If True values in X are matched to a unique value in Y and vice versa.
#' otherwise, an left-join type index returned as in \code{\link[base]{match}}
#' helpful attributes useful for debugging 
#' 
#' @export
#' 
#' @examples
#' u <- c(1,2,NA,2,3,NA,NA,NA,5)
#' v <- c(1,NA,3)
#' cbind(u,v[closematch(u,v)])
#' 
#' x = c(1,50,54,100)
#' y = c(1,4,51,100)
#' closematch(x,y)
#' closematch(x,y,d=2)
#' closematch(x,y,oneToOne=TRUE)
#' closematch(x,y,d=2,oneToOne=TRUE)
#' 
#' x <- rnorm(50000)
#' y <- rnorm(50000)
#' x[1:5] <- NA
#' y[y > 3] <- NA
#' y[20:25] <- NA
#' indx <-  closematch(x,y)
#' range(x-y[indx])
#' plot(x,y[indx])
#' abline(0,1)

closematch <- function(x,
					   y,
					   id.x,
					   id.y,
					   d=Inf,
					   nomatch=NA_integer_,
					   oneToOne=FALSE,
					   debug=FALSE){
	# VALIDATE THE ARGUMENTS
	if(missing(id.x) != missing(id.y))
		stop('id.x and id.y must both be provided or both be absent')

	# THE INNER FUNCTION THAT DOES LEFT-JOIN MATCHING
	inner <- function(x,y,d,nomatch=NA_integer_){
		if(length(x)==0)
			return(numeric(0))
		if(length(y)==0)
			return(rep(NA,length(x)))
		stopifnot(length(d) == 1)
		stopifnot(!is.na(d))
		ordx <- order(y)
		ordy <- order(x)
		xx <- y[ordx]
		yy <- x[ordy]
		xydiff <- xindx <- rep(NA,length(x))
		i <- 1
		for(j in 1:length(yy)){
			if(is.na(yy[j])) # nothing to match
				next
			# INCEREMENT `i` AS NECESSARY
			while( (!is.na(xx[i+1])) && # otherwise we've arrived at the largest non-missing value in x
				  (diff(diff(c(xx[i],yy[j],xx[i + 1]))) < 0))# yy[j] is closer to xx[i + 1] => were not looking at the relevent interval
					i <- i + 1
			# ASSIGN THE INDEX
			xindx[j] <- i
			xydiff[j] <- xx[i] - yy[j]
		}
		xindx[abs(xydiff) > d] <-nomatch 
		return(ordx[xindx[order(ordy)]])
	}

	# A WRAPPER THAT PROVIDES THE 1:1 MATCHING
	inner2 <- function(x,y,oneToOne,d,nomatch,debug){
		if(oneToOne){
			(xy <- inner(x,y,d=d,nomatch=nomatch))
			(yx <- inner(y,x,d=d,nomatch=nomatch))
			(xyDups <- duplicated(xy) & !is.na(xy))
			(yxDups <- duplicated(yx) & !is.na(yx))
			if(debug){
				xdebug <- factor(rep('NoMatch',length(x)),levels = c('Matched','NoMatch','MultipleMatches'))
				ydebug <- factor(rep('NoMatch',length(y)),levels = c('Matched','NoMatch','MultipleMatches'))
				stopifnot(length(xdebug) == length(xy))
				stopifnot(length(ydebug) == length(yx))
				xdebug[!is.na(xy)] <- 'Matched'
				ydebug[!is.na(yx)] <- 'Matched'
				xdebug[xyDups] <- 'MultipleMatches'
				ydebug[yxDups] <- 'MultipleMatches'
				attributes(xy) <- list(x=xdebug,y=ydebug,class='mdebug')
			}
			if(any(xyDups))
				xy[xy  %in% xy[xyDups]] <- NA
			if(any(yxDups))
				xy[unique(yx[yxDups])] <- NA
			return(xy)
		}
		else return(inner(x,y,d=d,nomatch=nomatch))
	}

	# A WRAPPER THAT PROVEDS THE WITHIN-ID MATCHING
	if(missing(id.x))
		return(inner2(x,y,d=d,nomatch=nomatch,oneToOne=oneToOne,debug=debug))
	else{
		stopifnot(all(!is.na(id.y)))
		stopifnot(all(!is.na(id.x)))
		stopifnot(length(x) ==length(id.x))
		stopifnot(length(y) ==length(id.y))
		# initialize output variables
		out <- numeric(length(x))
		if(debug){
			xdebug <- factor(rep('NoMatch',length(x)),levels = c('Matched','NoMatch','MultipleMatches'))
			ydebug <- factor(rep('NoMatch',length(y)),levels = c('Matched','NoMatch','MultipleMatches'))
		}
		# itereate over the id's
		for(id in unique(id.x)){
			xIndx <- (id.x == id)
			yIndx <- (id.y == id)
			tmp <- inner2(x=x[xIndx],y=y[yIndx],d=d,nomatch=nomatch,oneToOne=oneToOne,debug=debug)
			out[xIndx] <- which(yIndx)[as.numeric(tmp)]
			if(debug){
				xdebug[xIndx] <- attributes(tmp)$x
				ydebug[yIndx] <- attributes(tmp)$y
			}
		}
		if(debug)
			attributes(out) <- list(x=xdebug,y=ydebug,class='mdebug')
		return(out)
	}
}

#' @export
print.mdebug <- function(x,...){
	cat('Index with debug attributes\n')
	attributes(x) <- NULL
	NextMethod()
}

