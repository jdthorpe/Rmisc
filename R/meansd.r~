
#' Pretty printing of the mean and SD of a variable
#'
#' This function allows you to express your love of cats.
#' @param x a variable to be summarized
#' @export
meansd<- function(x,...)
	UseMethod('meansd')

#' @export
meansd.default <- function(...)
	cat('I don\'t know how to calculate the mean or sd of a "',
		class(list(...)[[1]])[1],
		'" variable\n',
		sep = '')

#' @export
meansd.numeric <- function(x,digits = 2, join = '',na.rm=FALSE){
	stopifnot(na.rm|(!any(is.na(x))))
	if(any(is.infinite(x))) 
		return('Contains Inf')
	if( length(digits)==1)
		digits <- c(digits,digits)
	paste(format(mean(x,na.rm = TRUE),digits=digits[1])
		,join
		,'('
		,format(sd(x,na.rm = TRUE),digits=digits[2])
		,')'
		, sep = ''
		)
}

#' @export
meansd.double <- meansd.numeric 

#' @export
meansd.integer <- meansd.numeric 

#' @export
meansd.Date <- function(x,digits = 0, join = '',na.rm=FALSE){
	stopifnot(na.rm|(!any(is.na(x))))
	if(any(is.infinite(x))) 
		return('Contains Inf')
	if( length(digits)==1)
		digits <- c(digits,digits)
	m <- floor(mean(unclass(x),na.rm = TRUE))
	class(m) <- 'Date'
	paste(m,
		  join,
		  '(' ,
		  format(sd(x,na.rm = TRUE),digits=digits[2]) ,
		  ' days)' ,
		  sep = '')
}

