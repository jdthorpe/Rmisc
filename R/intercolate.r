
# --------------------------------------------------------------------------------

#' Intercolte two items
#'
#' This function allows intercolates 2 variables
#' @param x,y two variables of the same class to be intercolated
#' @param x.text,y.text prefixes to be prepended to the \code{names(x)} and \code{names(y)} respectively
#' @export
#' @examples
#' intercolate(paste(1:3),LETTERS[1:3])
#' intercolate(paste(1:4),LETTERS[1:3])
#' \dontrun{
#' intercolate(paste(1:4),LETTERS[1:3])
#' }

intercolate <- function(x,y,x.text='x.',y.text='y.'){
	if(class(x) != class(y))
		stop('x and y must be of the same class')
	UseMethod('intercolate')
}

#' @export
intercolate.default <- function(x,y,x.text='x.',y.text='y.')
	stop(paste('I dont know how to intercolate.default a "', class(x), '" variable\n'))

#' @export
intercolate.list <- function(x,y,x.text='x.',y.text='y.'){
	if(! length(y) %in% (length(x) -0:1))
		stop('y is not the correct length')
	out <- list( )
	out[(1:length(x))*2-1] <- x
	out[(1:length(y))*2] <- y

	if((!(is.null(names(x))))|
	   (!(is.null(names(y))))){
		if(is.null(names(x)))
			names(x) <- paste(x.text,1:length(x),sep='')
		if(is.null(names(y)))
			names(y) <- paste(y.text,1:length(y),sep='')
		names(out) <- intercolate(names(x),names(y))
	}
	return(out)
}

#' @export
intercolate.numeric <- function(x,y,x.text='x.',y.text='y.'){
	if(! length(y) %in% (length(x) -0:1))
		stop('y is not the correct length')
	out <- c( )
	out[(1:length(x))*2-1] <- x
	out[(1:length(y))*2] <- y

	if((!(is.null(names(x))))|(!(is.null(names(y))))){
		if(((is.null(names(x))))){names(x) <- paste(x.text,1:length(x),sep='')}
		if(((is.null(names(y))))){names(y) <- paste(y.text,1:length(y),sep='')}
		names(out) <- intercolate(names(x),names(y))
	}
	return(out)
}


#' @export
intercolate.character <- intercolate.numeric
#' @export
intercolate.integer <- intercolate.numeric


#' @export
intercolate.matrix <- function(x,y,x.text='x.',y.text='y.'){
	stopifnot((nrow(x)) == (nrow(y))) 
	if(! (dim(y)[[2]]) %in% ((dim(x)[2]) -0:1))
		stop('y must have the same number or one fewer columns than x')
	#container for the dimnames
	dn <- list(NULL,NULL)
	#get the row names
	if(  (!is.null(dimnames(x)[[1]])) #if they both have row names, use names from x
		&(!is.null(dimnames(y)[[1]]))){
		if(any((dimnames(x)[[1]]) != (dimnames(y)[[1]])) )
		warning('x and y do not have the same row names, taking row names from x')
		dn[[1]] <- dimnames(x)[[1]]
	}else{
		if((is.null(dimnames(y)))){
			#if y's names are mising use, x names
			warning('x and y do not have the same row names, taking row names from x')
			dn[[1]] <- dimnames(x)[[1]]
		}else{
			#otherwise use y names (even if they are null)
			warning('x and y do not have the same row names, taking row names from y')
			dn[[1]] <- dimnames(y)[[1]]
		}
	}

	#get the column names
	if(is.null(dimnames(y)[[2]])){
		dny <- paste(y.text,1:(dim(y)[2]),sep = '')
	}else{
		dny <- paste(y.text,dimnames(y)[[2]],sep = '')
	}
	if(is.null(dimnames(x)[[2]])){
		dnx <- paste(x.text,1:(dim(x)[2]),sep = '')
	}else{
		dnx <- paste(x.text,dimnames(x)[[2]],sep = '')
	}

	dn[[2]] <- intercolate(dnx,dny)#"Did you mean: recursion"

	out <- matrix(NA
				 ,nrow = dim(x)[1]
				 ,ncol = (dim(x)[2]) + (dim(y)[2])
				 ,dimnames = dn
				 )
	out[,(1:(dim(x)[2]))*2-1] <- x
	out[,(1:(dim(y)[2]))*2] <- y
	return(out)
}



