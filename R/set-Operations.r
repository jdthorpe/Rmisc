# --------------------------------------------------
# Slight improvements over the base functions
# --------------------------------------------------

#' Performs the union of 2 or more vectors
#'
#' Recursively performs the union of 2 or more vectors.
#' @param ... vectors to be intersected
#' @export
#' @seealso \code{\link[base]{union}} and \code{\link[base]{intersect}} on which these functions are based
#' @rdname sets
union <- function(...){
	# a more perfect union (hahaha)
	argList <- list(...)
	names(argList) <- NULL
	switch(as.character(length(argList)),
		   '0'=NULL,
		   '1'=argList[[1]],
		   '2'=do.call('union',argList,envir=as.environment('package:base')),
		   do.call('union',
				   c(list(do.call('union',argList[1:2],envir=as.environment('package:base'))),
					 argList[-(1:2)])))
}

#' @export
#' @rdname sets
intersect <- function(...){
	# a more perfect intersect 
	argList <- list(...)
	names(argList) <- NULL
	switch(as.character(length(argList)),
		   '0'=NULL,
		   '1'=argList[[1]],
		   '2'=do.call('intersect',argList,envir=as.environment('package:base')),
		   do.call('intersect',
				   c(list(do.call('intersect',argList[1:2],envir=as.environment('package:base'))),
					 argList[-(1:2)])))
}

# search for fields that match a pattern in 'data' or any other 
# dataframe or vector

