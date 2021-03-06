#' Typed: Pretty printing to the console
#'
#' Pretty printing of variables, which is particulary useful if the printed text will be used
#' in a script or processed in a text editor.
#' @param x a variable to print to the console
#' @export
#' @examples
#' typed(1:4)
#' typed(sample(c(TRUE,FALSE),5,TRUE))
#' typed(c('one','two','three'))
typed <- function(x)UseMethod('typed')

#' @export
typed.default <- function(...)
	print(x)
#' @export
typed.numeric <- function(x)
	for(val in x)
		cat(val,'\n')

#' @export
typed.integer <- typed.numeric

#' @export
typed.Date <- function(x)
	typed.character(as.character(x,'%Y-%m-%d'))

#' @export
typed.factor <- function(x)
	for(val in x)
		cat('"',gsub('\n','\\\\n',as.character(val)),'"\n',sep = '')

#' @export
typed.character <- typed.factor 

#' @export
typed.logical <- function(x)
	for(val in x)
		cat(ifelse(val,'TRUE','FALSE'),'\n',sep = '')

