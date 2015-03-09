# function: npercent 
#####################################################################

#' Pretty printing of a numerator and percentage
#'
#' Pretty printing of a numerator and percentage.
#' @param numerator The numerator of a fraction 
#' @param denominator The denominator of a fraction. (Default = sum(numerator,na.rm = na.rm))
#' @param na.rm If TRUE,  missing values are excluded when calculating the numerator and/or denominator.(Default = FALSE)
#' @param digits number of digits included in the percentage. (defatlt = 1)
#' @param join a separator between the numerator and the folloing open paren. (defatlt = ' ')
#' @param of If True, the denominator is included in the out put text. (default=FALSE)
#' @param N If True, the numerator is included in the output text. (default= TRUE)
#' @param perc If True, the percentage is included in the output. (default= TRUE)
#' @export
#' @examples
#' npercent(6,9)
#' npercent(6,9,of=TRUE)
#' npercent(factor(c('a','b','b','c','c','c')),of=TRUE)
#' npercent(factor(c('a','b','b','c','c','c')),denominator=12,of=TRUE,N=FALSE)
#' npercent(sample(c(TRUE,FALSE),10,TRUE),of=TRUE)
#' npercent(strsplit('finished files are the result of years of scientific study combined with the experience of many years','')[[1]])

npercent <- function(numerator,...)
	UseMethod('npercent')

#' @export
npercent.default <- function(numerator,...)
	stop(paste('I don\'t know how to calculate the perctage for variables of class "',
			   class(numerator)[1]))

# a helper function used by npercent.numeric to calculate a single entry
np1 <- function(numerator,
				denominator,
				digits = 1,
				join = ' ',
				of = FALSE,
				N=TRUE,
				perc=T){
	paste(ifelse(N,numerator,'')
		 ,ifelse(perc & N,join,'')
		 ,ifelse(perc & N,'(','')
		 ,ifelse(perc,tochar(100*numerator/denominator,digits=digits),'')
		 ,ifelse(perc,'%','')
		 ,ifelse(of,' of ','')
		 ,ifelse(of,denominator,'')
		 , sep = ''
		 ,ifelse(perc & N,')','')
		 )
} 

#' @export
npercent.numeric <- function(numerator,
							 denominator=sum(numerator,na.rm = na.rm),
							 na.rm=FALSE,
							 digits=1,
							 join=' ',
							 of=FALSE,
							 N=TRUE,
							 perc=TRUE,
							 ...){
	if(length(denominator) == 1) 
		denominator <- rep(denominator,length(numerator))
	if(length(denominator) != length(numerator))
		stop('The length(denominator) must be 1 or length(numerator)')
	out <- NULL
	for(i in 1:length(numerator))
		out <- c(out,
				 np1(numerator[i],
					 denominator[i],
					 digits,
					 join,
					 of,
					 N,
					 perc))
	names(out) <- names(numerator)
	return(out)
}

#' @export
npercent.double <- npercent.numeric

#' @export
npercent.integer <- npercent.numeric

#' @export
npercent.logical <- function(numerator,
							 denominator=sum(!is.na(numerator),na.rm = na.rm),
							 na.rm = FALSE,
							 ...) {
	force(denominator)
	npercent(sum(numerator,na.rm = na.rm),denominator=denominator,...)
}

#' @export
npercent.factor <- function(numerator,...){
	out <- NULL
	for(l in levels(numerator))
		out <- c(out,npercent(numerator == l,...))
	names(out) <- levels(numerator)
	return(out)
}

#' @export
npercent.character <- function(x,...)
	npercent.factor(factor(x),...)

#' @export
npercent.table <- function(numerator,...){
	tmp  <-  npercent(unclass(numerator),...)
	class(tmp) <- 'table'
	numerator[] <- tmp
	return(unclass(numerator))
}

