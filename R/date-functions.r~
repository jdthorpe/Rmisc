# --------------------------------------------------------------------------------
# Date functions
# --------------------------------------------------------------------------------

#' Date to string conversions
#'
#' Convert Dates into strings with format YYYYMMDD
#' @param x a varaiable coersable to Date
#' @export
#' @examples
#' dtos()
#' dtos('2013-01-31')
#' dtos('01-31-2013')
#' dtos('01/31/2013')
#' dtos(c('2013-01-31','01/31/2013'))
dtos <- function(x=as.Date(date(),"%a %b %d %H:%M:%S %Y")){
	if(all(is.na(x)))
		return(x)
	if(!inherits(x,c('Date',"POSIXct","POSIXt")))
		tmp <- try(x <- easyDateTime(x),TRUE)
	if((!inherits(x,'Date')))
		stop("x must be a coercable to class 'Date'")
	out <- rep(NA,length(x))
	out[!is.na(x)] <- format(x[!is.na(x)],'%Y%m%d')
	return(out)
}

#' Coersion to date format.
#'
#' Coersion to date format. Note that this is *much* slower than converting 
#' to date using as.Date(x,pattern) if you already know the date pattern
#'
#' @param x a varaiable coersable to Date
#' @export
#' @examples
#' as.date('2013-01-31')
#' as.date('01-31-2013')
#' as.date('01/31/2013')
#' as.date(c('2013-01-31','01/31/2013'))
easyDateTime <- function(x){
	# this paralells  the code in read.csv
	if(inherits(x,c('Date',"POSIXct","POSIXt")))
		return(x)
	# try as.Date 
	if(all(is.na(x)))
		return(as.Date(x))
	if(inherits(x,'factor'))
		x <- as.character(x)
	if(!inherits(x,'character'))
		stop('x is not coercable to date/time')
	patternList <- list(# Y-M-D 
						c(valPattern='^\\d{4}-\\d{2}-\\d{2}$',
						  naPattern='^ *- *- *$',
						  formatString="%Y-%m-%d",
						  className='Date'),
						# M/D/Y 
						c(valPattern='^\\d{1,2}/\\d{1,2}/\\d{4}$',
						  naPattern='^ */ */ *$',
						  formatString='%m/%d/%Y',
						  className='Date'),
						# M/D/Y H:M:S
						c(valPattern="^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}$",
						  naPattern="^ */ */ *: *: *$",
						  formatString="%m/%d/%Y %H:%M",
						  className='POSIXct'),
						# Y/M/D H:M:S
						c(valPattern="^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}(\\.[0-9]*)?$",
						  naPattern="^ */ */ *: *: *$",
						  formatString="%Y/%m/%d %H:%M:%S",
						  className='POSIXct'),
						# M-D-Y H:M:S
						c(valPattern="^\\d{2}-\\d{2}-\\d{4} \\d{2}:\\d{2}:\\d{2}$",
						  naPattern="^ *- *- *: *: *$",
						  formatString="%m-%d-%Y %H:%M",
						  className='POSIXct'),
						# Y-M-D H:M:S
						c(valPattern="^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}(\\.[0-9]*)?$",
						  naPattern="^ *- *- *: *: *$",
						  formatString="%Y-%m-%d %H:%M:%S",
						  className='POSIXct'))
	xna <- is.na(x)
	for(pl in patternList){
		isEmpty <- grepl(pl['naPattern'],x) | is.na(x) | grepl('^ *$',x)
		if(all(grepl(pl['valPattern'],x) | isEmpty)) {
			if(pl['className'] == 'Date')
				x <- as.Date(as.character(x),pl['formatString'])
			else 
				x <- as.POSIXct(as.character(x),format=pl['formatString'])
			if(any(is.na(x) & !isEmpty))
				warning(paste(sum(any(is.na(x) & !isEmpty)), 'NAs introduced when converting field by coersion'))
			return(x)
		}
	}
	stop('Format of X does not match any known date or date/time pattern')
}



