#' Summarize a variable in a nice table
#'
#' Summarize a variable in a nice table that is compatibile with summaries produced by \code{\link{twobyn}}
#' @keywords cats
#' @export
#' @seealso \code{\link{twobyn}} for summarizeing a variable between two groups
#'
#' @param x a variabile to summarized
#' @param varName = 'x' # name of the variable x
#' @param lof if \code{TRUE}  the denomiator is included in the percntage (defautl=FALSE).
#' [example:\code{"15 (15\% of of 100)"}]
#' 
#' @param mof if \code{TRUE}  the number of missing values is included in text summary
#' 
#' @param lyesonly If \code{TRUE}, the number of false reponses is dropped from the output table (default = FALSE)
#' @param digits number of decimals used if the summary statistic is the mean and standard deviation (default = c(1,1))
#'
#' @param ... additional arguments to \code{\link{meansd}}
#' the mean and sd of x 
summarize <- function(x # x is any variabile to be compared to 'ltwo', a logical variable.
				  ,varName = deparse(substitute(x)) 
				  ,lof=FALSE # (logical) do you want to include the denomiator in the percntage? 'ie 15 (15% of of 100)'
				  ,mof=FALSE # like lof but for the number of missing values
				  ,lyesonly = FALSE # report only the % Trues for logical variables?
				  ,digits=1
				  ,...)
	UseMethod('summarize')

#' @export
summarize.default <- function(...)
	cat('I dont know how to summarize a "',
		class(list(...)[[1]]),
		'" variable\n',file='')

#' @export
summarize.logical <- function(x
				  ,varName = 'x' # name of the variable x
							  ,lof=FALSE,mof=FALSE,lyesonly=FALSE,digits=1,...){
	if(lyesonly){
		out <- rbind( c(  varName
						# summary description
						,'Yes'
						# summary statistics
						, npercent( x,of = lof,na.rm = TRUE,...)
						# missings
						, ifelse(sum(is.na(x)),npercent( is.na(x),of = mof,...),'None')
						)
					)

	}else{
		out <- rbind( c( varName
						# summary description
						,'No'
						# summary statistics
						, npercent(!x,of = lof,na.rm = TRUE,...)
						# missings
						, ''
						)
					, c(  ''#varName
						# summary description
						,'Yes'
						# summary statistics
						, npercent( x,of = lof,na.rm = TRUE,...)
						# missings
						, ifelse(sum(is.na(x)),npercent( is.na(x),of = mof,...),'None')
						)
					)
	}
	dimnames(out)[[2]] <- c('Variable Name'
							,'Value'
							,paste(varName)
							,'Missing'
							)
	return(out)
}

#' @export
summarize.numeric <- function(x
				  ,varName = 'x' # name of the variable x
							  ,lof=FALSE,mof=FALSE,lyesonly=FALSE,digits = 1,...){
	if( length(digits)==1)
		digits <- c(digits,digits)
	out <- cbind(  varName
				# summary description
				, 'Mean(sd)'
				# summary statistics
				, meansd( x,digits=digits)
				# statistcial test
				# missings
				, ifelse(sum(is.na(x)),
						 npercent( is.na(x),of = mof,...),'None')
				)
	dimnames(out)[[2]] <- c('Variable Name'
							,'Value'
							,paste(varName)
							,'Missing'
							)
	return(out)
}

#' @export
summarize.integer <- summarize.numeric

#' @export
summarize.character <- function(x
				  ,varName = 'x' # name of the variable x
								,lof=FALSE,mof=FALSE,lyesonly=FALSE,digits=1,...){
	xtab <- table(x)
	temp <- NULL
	for(r in 1:dim(xtab)){
		temp <- rbind(temp,
					 npercent(xtab[r],sum(xtab),digits=digits,...)
					)
	}
	out <- cbind(c(varName,rep('',length(xtab)-1 ))
					# summary description
					, dimnames(xtab)[[1]]
					# summary statistics
					, temp
					# missings
					, c(rep('',length(xtab)-1 )
						,if(sum(is.na(x))>0) 
							npercent( is.na(x),of = mof,...) else 'None')
					)
	dimnames(out)[[2]] <- c('Variable Name'
							,'Value'
							,paste(varName)
							,'Missing'
							)
	return(out)
}

#' @export
summarize.factor <- summarize.character
