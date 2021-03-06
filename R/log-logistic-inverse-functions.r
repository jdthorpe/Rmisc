# --------------------------------------------------------------------------------
# inversions of the 4 and 5 point log-logistic standard curve models
# --------------------------------------------------------------------------------

#' Inverse of the 5 parameter log-logistic inverse function.  
#'
#' Inverse of the 5 parameter log-logistic inverse function.  This is useful for 
#' when applying a standard curve to ELISA or other similar data
#'
#' @param y oberseved readings to back transformed into concentrations
#' @param b,c,d,e,f parameters of the log-ligistic function
#' @export predict.5pl.inv
#' @seealso 4 parameter log-logistic inverse function: \code{\link{predict.4pl.inv}}
predict.5pl.inv<-function(y,b,c,d,e,f){
	suppressWarnings(out<- exp((log((((d-c)/(y - c)))^(1/f) - 1) / b)) *e)
	if(sum(y < c,na.rm=TRUE))
		warning(paste(sum(y < c,na.rm=TRUE),'readings(s) fell below the standard curve'))
	if(sum(y > d,na.rm=TRUE))
		warning(paste(sum(y > d,na.rm=TRUE),'readings(s) fell above the standard curve'))
	out[y < c] <- 0
	out[y > d] <- Inf
	out
}

#' Inverse of the 4 parameter log-logistic inverse function.  
#'
#' Inverse of the 4 parameter log-logistic inverse function.  This is useful for 
#' when applying a standard curve to ELISA or other similar data
#'
#' @param y oberseved readings to back transformed into concentrations
#' @param b,c,d,e parameters of the log-ligistic function
#' @export predict.4pl.inv
#' @seealso 5 parameter log-logistic inverse function: \code{\link{predict.5pl.inv}}
predict.4pl.inv<-function(y,b,c,d,e)
		predict.5pl.inv(y,b,c,d,e,1)

