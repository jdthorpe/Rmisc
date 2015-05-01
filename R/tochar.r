# --------------------------------------------------------------------------------
# pretty conversion of variables to mode character
# --------------------------------------------------------------------------------

#' Convert object to pretty strings
#'
#' Convert objects to strings with more contol than \code{\link[base]{as.character}()} 
#' and less confusion than \code{\link[base]{format}()} 
#' @param x An object to be coerced to character
#' @param digits If applicable, the number of digits to be included in the character representation.
#' @param ... Additional arguments (unused as of this writing)
#' @export
#' @rdname tochar
#' @examples
#' tochar(0.00123,digits=4)
#' as.character(round(0.00123,digits=4)) #same as above
#'
#' tochar(123,digits=4)
#' as.character(round(123,digits=4))# no significant digits
#'
#' tochar(0.000123,digits=4)
#' as.character(round(0.000123,digits=4)) #not pretty
#'
#' tochar(0.0000123,digits=4)
#' as.character(round(0.0000123,digits=4)) #no significant digits
#'
#' \dontrun{
#' tochar(sin)# cant coerce a function to string...
#' }

tochar <- function(x,...)
	UseMethod('tochar')

#' @export
#' @rdname tochar
tochar.default <- function(x,...)
	stop(paste0('I don\'t know how to coerce a variable of type "', class(x)[1], '" to a string\n'))

#' @export
tochar.character <- function(x,...)
	return(x)

#' @export
tochar.logical <- function(x,...)
	ifelse(x,'TRUE','FALSE')

#' @export
tochar.Date <- function(x,...)
	as.character(x)

#' @export
tochar.factor <- tochar.Date 

#' @export
#' @rdname tochar
tochar.numeric <- function(x,digits=1,...){
	ntoc1 <- function(x,digits){
		if(is.infinite(x) )
			return(as.character(x))
		x <- round(x,digits=digits)
		if(is.na(x))return('NA')
		x <- format(x,scientific = FALSE)
		if(length(grep('[.]',x))){# x has a decimal place
			temp <- strsplit(x,'[.]')[[1]]
			temp[2] <- substr(temp[2],1,digits)
		}else{temp <- c(x,'')}
		if(digits < 1)return(temp[1])
		if(nchar(temp[2])<digits)for(i in (nchar(temp[2])+1):digits)temp[2] <- paste(temp[2],'0',sep = '')
		return(paste(temp[1],temp[2],sep = '.'))
	}
	out <- character(0)
	for(i in 1:length(x))out[i] <- ntoc1(x[i],digits)
	return(out)
}

#' @export
tochar.double <- tochar.numeric

#' @export
tochar.integer <- tochar.numeric

#' @export
tochar.matrix <- function(x,...)
	matrix(tochar(as.vector(x),...),dim(x)[1],dim(x)[2])

