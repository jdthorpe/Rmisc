

#' Regular expressoin searches of the names of a data frame.
#'
#' Regular expressoin searches of the names of a data frame
#' @param pattern a reguralar expression pattern
#' @param x an object to be searched. If x is a \code{data.frame} then the names of x are searched
#' @param ic logical. If FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching
#' @param ... additional parameters for \code{\link[base]{grep}}
#' @export
gr <- function(pattern,# the pattern to search for.  Note that this is *always* treated as a string
			   x=names(data),# the vector to search, (`data` just happens to be my standard name for a dataset)
			   ic=TRUE,
			   ...){

	if(missing(x)){
		x = try(get('data',as.envoronment(-1)),TRUE)
		if(inherits(x,'try-error'))
			x = try(get('data',.GlobalEnv),TRUE)
		if(inherits(x,'try-error'))
			stop("argument 'x' missing with no default")
	}

	if(is.list(x))
	   	x <- names(x)

	if(inherits(suppressWarnings(try(force(pattern),TRUE)),'try-error')||
	   (mode(pattern)=='function'))
		pattern <- deparse(substitute(pattern))
	grep(pattern,x,value=TRUE,ignore.case=ic,...)
}

