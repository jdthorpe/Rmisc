#' Apply a Function Over a Ragged Array in a data.frame
#'
#' Apply a Function Over a Ragged Array in a data.frame (D is for data.frame)
#' Like \code{\link[base]{sapply}}, tries to coerse the return values into a vector.
#' The return values of the function FUN inserted into the output vector 
#' in posisions corresponding to the values of INDEX 
#'
#' Note that the \code{FUN} must return values of length 1 or with length equal that 
#' of it's first input. 
#'
#' @export
#' @inheritParams base::tapply
#' 
#' @param X an atomic object, typically a vector.
#' 
#' @param INDEX list of one or more factors, each of same length as
#' \code{X}.  The elements are coerced to factors by \code{\link{as.factor}}.
#' 
#' @param FUN the function to be applied, or \code{NULL}.  In the case of
#' functions like \code{`+`}, \code{\%*\%}, etc., the function name must
#' be backquoted or quoted.  If \code{FUN} is \code{NULL}, tapply
#' returns a vector which can be used to subscript the multi-way array
#' \code{tapply} normally produces.

#' @param ... optional arguments to \code{FUN}: the Note section.


dapply<-function(X,INDEX,FUN,...){
	tmp<-tapply(X,INDEX,FUN,...)
	#ARE ALL THE RETURN VALUES LENGTH 1?
	all_length_1 <- TRUE
	for(name in names(tmp))
		if(length(tmp[[name]]) > 1){
			all_length_1 <- FALSE
			break
		}
	#IF EVERY ELEMENT OF tmp IS LENGTH 1, APPLY TO EACH OF THE VALUES
	if(all_length_1)
		return(tmp[match(INDEX,names(tmp))])
	# OTHERWISE, APPLY 1 RETURN VALUE PER ELEMENT
	out <- c()
	for(name in names(tmp) ){
		if( sum(y == ifelse(inherits(y,'factor'),name,as(name,typeof(y)))) != length(tmp[[name]]))
			stop('FUN must return values of length 1 or with length equal to the number of replicates')
		out[INDEX == name] <- tmp[[name]]
	}
	return(out)
}

