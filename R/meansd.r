
#' Pretty printing of the mean and SD of a variable
#'
#' This function allows you to express your love of cats.
#' @param x a variable to be summarized
#' @param digits The number of digits to include in the mean (\code{digits[1]}) and standard deviation (\code{digits[1]}).
#' @param join a string used to separate the mean and parens around the standard deviation
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @export
meansd<- function(x,digits = 2, join = '',na.rm=FALSE)
	UseMethod('meansd')

#' @export
meansd.default <- function(x,digits = 2, join = '',na.rm=FALSE)
	stop(paste0('I don\'t know how to calculate the mean or sd of a "', class(x)[1], '" variable'))

#' @export
meansd.numeric <- function(x,digits = 2, join = '',na.rm=FALSE){
	stopifnot(na.rm|(!any(is.na(x))))
	if(any(is.infinite(x))) 
		return('Contains Inf')
	if( length(digits)==1)
		digits <- c(digits,digits)
	paste0(format(mean(x,na.rm = TRUE),digits=digits[1])
		,join
		,'('
		,format(sd(x,na.rm = TRUE),digits=digits[2])
		,')')
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
	paste0(m,
		  join,
		  '(' ,
		  format(sd(x,na.rm = TRUE),digits=digits[2]) ,
		  ' days)')
}

