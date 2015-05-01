##########################################################################################

#' Empirical thresholds corresponding to given specificities
#'
#' Empirical thresholds corresponding to given specificities.
#' @param x A vector of marker values
#' @param case A logical vector where TRUE indicates that the corresonding marker value in X was 
#' is taken from a 'Case', and FALSE otherwise. 
#' @param spec A vector specificities for the returned thresholds
#' @param ... (optional) Additoinal arguments to \code{threshold()}
#' @return A list containing the sensetivities, specificiees, and the corresponding thresholds (cutoffs)
#' @export
#' @seealso \code{\link{threshold}}


sens.at.spec<-function(x,case,spec = .95){
	controls<-x[!case]
	controls<-controls[order(controls)]
	cutoff<- controls[ceiling(spec*length(controls))]
	sens<-mean(x[case]>cutoff)
	out<-list()
	out$specificity<-spec
	out$sensitivity<-sens
	out$cutoff<-cutoff
	out
}


# this version is based on  the function 'threshold'
#-- sens.at.spec<-function(x,case,spec,...){
#-- 	ARGS  <- list(...)
#-- 	if(length(ARGS)){
#-- 		ARGS[['x']] <- x[!case]
#-- 		ARGS[['probs']] <- spec
#-- 		ARGS[['case.values']] = x[case]
#-- 		cutoff<- do.call(threshold,ARGS)
#-- 	} else{
#-- 		controlLevels<-x[!case]
#-- 		controlLevels<-controlLevels[order(controlLevels)]
#-- 		cutoff<- controlLevels[ceiling(spec*length(controlLevels))]
#-- 	}
#-- 	sens <- apply(outer(x[case],cutoff,`>`),2,mean)
#-- 	return(list(specificity=spec,
#-- 			  sensitivity=sens,
#-- 			  cutoff=cutoff))
#-- }

#-- sens.at.spec <- function(val,
#-- 						 case,
#-- 						 spec = .95,
#-- 						 ...){
#-- 	controls<-val[!case]
#-- 	controls<-controls[order(controls)]
#-- 	cutoff<- controls[ceiling(spec*length(controls))]
#-- 	sens<- apply( outer(val[case],cutoff,function(x,y){x>y}),2,mean,na.rm = TRUE)
#-- 	out<-list()
#-- 	out$cutoff<-cutoff
#-- 	out$sens<-sens
#-- 	out
#-- }


THRESHOLD <- function(x
					,probs
					,specificity.bias='over'
					,case.values
					,sensitivity.bias='proportional'
					,na.rm = FALSE
					,ci= FALSE
					,conf.level = 0.95
					,verbose = FALSE
					,quietly =FALSE 
					){
	stopifnot(length(probs) == 1)
	spec <- probs
	# return a threshold which gives a specificity of spec. the ou parameter
	#let's face it qnantile(x,probs = specificity) sucks.
	# indicicates if a threshold should be just over or under the
	# if there are case.values which lie between the upper and lower bounds for the
	# threshold, then the method 'method' is used to determin the appropriate
	# theshold

	specificity.bias <- match.arg(specificity.bias, c('over','under'))
	sensitivity.bias <- match.arg(sensitivity.bias, c('lower','upper','median','mean','proportional'))

	if(missing(case.values)) 
		case.values <- c()
	if(na.rm)
		x <- x[!is.na(x)]
	if(na.rm & (length(case.values)))
		case.values <- case.values[!is.na(case.values)]
	if(any(is.na(x    )))
		stop("x cannot contain NA's if na.rm == FALSE")
	if(length(case.values) && any(is.na(case.values)))
	   	stop("case.values cannot contain NA's if na.rm == FALSE")


##########################################################################################
# a handy function
##########################################################################################
	getconf <- function(cutoff ){
		if(!ci){
			return(cutoff)
		}else{
		spec.mle = 1-mean(x > (cutoff))
		spec.exp = 1-(sum(x > (cutoff)) + 1) / (length(x > (cutoff))+1)
		ci.spec  = 1-qbeta(c((1-conf.level)/2,1-((1-conf.level)/2))
						  , sum(x > (cutoff)) + 1
						  , sum(x < (cutoff)) + 1
						  )
		if(length(case.values)){
			sens.mle = mean(case.values > (cutoff))
			sens.exp = (sum(case.values > (cutoff)) + 1) /( length(case.values > (cutoff)) + 1)
			ci.sens = qbeta(c((1-conf.level)/2,1-((1-conf.level)/2))
						  , sum(case.values > (cutoff)) + 1
						  , sum(case.values < (cutoff)) + 1
						  )
		}else{sens.exp <- sens.mle <- ci.sens <- NA}

		return( list(  threshold  = cutoff
					, conf.level = conf.level
					, spec.mle   = spec.mle
					, spec.exp   = spec.exp
					, ci.spec    = ci.spec
					, spec.n 	 = length(x)
					, sens.mle   = sens.mle
					, sens.mle   = sens.mle
					, ci.sens    = ci.sens
					, sens.n 	 = length(case.values)
					))
		}
	}


	(x <- sort(x))
	(Ex <- (1:length(x))/(length(x) + 1)) # expected values of the order statistics
	(boundary <- x[ c( last(Ex <= spec,none=1         )
					 ,first(Ex >= spec,none=length(x) ))
					 ])
	(pboundary <- Ex[ c( last(Ex <= spec,none=1         )
					 ,first(Ex >= spec,none=length(x) ))
					 ])

	if(!diff(boundary)){
		#-- warning('ties were found at the boundary')
		# ties in the controls at the specificity level
		# result in ties at the boundary. Include too many contorls (sens.bias = under)
		# or too few controls (spec bias = over)?


		#if the specificity is above the expected value for the highest order statistic
		#DESIRED THRESHOLD IS ABOVE THE CONTROLS
		if(boundary[1] == max(x)){
			if( max(case.values) <= (boundary[1])){
				if(!quietly)
					warning('Threshold is the highest value among all observations')
				#return(boundary[1])
				out <- boundary[1]
				return.value <- getconf(out)
				return(return.value)

			}
			#else:
			if(!quietly)
				warning('Threshold is higher than all observed values in the controls')
			out <- (min(case.values[case.values > (boundary[1])]) + (boundary[1]))/2
			return.value <- getconf(out)
			return(return.value)
		}

		#if the specificity is below the expected value for the highest order statistic
		#DESIRED THRESHOLD IS BLOW THE CONTROLS
		if(boundary[1] == min(x)){
			if( min(case.values) >= (boundary[1])){
				if(!quietly)
					warning('Threshold is the lowest value among all observations')
				#return(boundary[1])
				out <- boundary[1]
				return.value <- getconf(out)
				return(return.value)
			}
			if(!quietly)
				warning('Threshold is lower than all observed values in the controls')
			out <- (max(case.values[case.values < (boundary[1])]) + (boundary[1]))/2
			return.value <- getconf(out)
			return(return.value)
		}

		# the specificity is within the range of expected values of the order statistics but
		# THERE ARE TIES IN THE DATA AND THE DESIRED THRESHOLD IS BETWEEN THE TIED VALUES!
		# Now we need to choose if the threshold has to be a liitle
		# too high or a little to low
		if(specificity.bias == 'over'){
			#over specific means choose a higher threshold
			if( all(c(x,case.values) <= (boundary[1]))){
				#there are no higher values
				if(!quietly)
					warning('Threshold is the highest value among all observations')
				#return(boundary[1])
				out <- boundary[1]
				return.value <- getconf(out)
				return(return.value)
			}
			if( all(x <= (boundary[1]))){
				if(!quietly)
					warning('Threshold is higher than all observed values in the controls')
			}else{
				if(!quietly)
					warning('There were ties among the controls at the desired specificity; threshold is above the tie and identifeis too few controls')
			}
			out <- (min(c(x    [x     > (boundary[1])]
						  ,case.values[case.values > (boundary[1])]
						  ))
					+ (boundary[1])
					)/2
			return.value <- getconf(out)
			return(return.value)
		}else{#specificity.bias == 'under'
			#under specific means choose a lower threshold (=over fpr)
			if( all(c(x,case.values) >= (boundary[1]))){
				if(!quietly)
					warning('Threshold is the lowest value among all observations')
				#return(boundary[1])
				out <- boundary[1]
				return.value <- getconf(out)
				return(return.value)
				return(boundary[1])
			}
			if( all(x <= (boundary[1]))){
				if(!quietly)
					warning('Threshold is lower than all observed values in the controls')
			}else{
				if(!quietly)
					warning('There were ties among the controls at the desired specificity; threshold is below the tie and identifeis too many controls')
			}
			#the average of (the max of all values below the threshold) and (the boundary value)
			out <- (max(c(x    [x     < (boundary[1])] #NOTE THAT boundary[1] == boundary[2]
						  ,case.values[case.values < (boundary[1])]
						  ))
					+ (boundary[1])
					)/2
			return.value <- getconf(out)
			return(return.value)
		}
	}

	# The desired threshold lies between two controls with discinct values.
	# Check if there are any case.values between the boundary controls and if so
	# Choose an appropriate threshold WRT the distribution of those case.values.

	#-- print(boundary)
	#-- print(sum(( (case.values > boundary[1])
	#-- 	     & (case.values < boundary[2]))))
	if( (length(case.values) > 0)
		& any( (case.values > boundary[1])
			 & (case.values < boundary[2]))){

		#warning('	case.values lie between the boundaries')
		#get the boundary points (controls) and the case.values between the boundaries
		pts <- sort(c( boundary[1]
					, case.values[(case.values > (boundary[1]))
						   &(case.values < (boundary[2]))]
					, boundary[2]
					))
		if(length(pts) <=2)stop('error in selecting case.values between boundaries')
		(midpoints <- ((pts[-1] + pts[-length(pts)] )/2))

		mid.lower <- function(x)x[floor((length(x) + 1)/2)]
		#mid.upper <- function(x)x[ceiling((length(x) + 1)/2)]

		thresholds <- list( 'mean'   = mean(boundary)
							,'median' = mid.lower(midpoints)
							,'lower'  = min(midpoints[midpoints > (boundary[1]) ])
							,'upper'  = max(midpoints[midpoints < (boundary[2]) ])
							,'proportional' = midpoints[first( ((1:length(midpoints))/(length(midpoints)+1))
																	> ((spec - pboundary[1])/diff(pboundary))
																,none = length(midpoints))]
							)

		#if there are ties with the median and a boundary, default to the upper (lower)
		if( thresholds[['median']] == (boundary[1]))thresholds[['median']] <- thresholds[['lower']]
		if( thresholds[['median']] == (boundary[2]))thresholds[['median']] <- thresholds[['upper']]

		if( is.na(thresholds[['proportional']]))thresholds[['proportional']] <- thresholds[['median']]

		# if the mean would give a threshold equal to observed value, default to the median
		if( thresholds[['mean']] %in% case.values)thresholds[['mean']] <- thresholds[['median']]

		if(any(unlist(thresholds) %in% boundary))stop("Error in 'threshold'")
		if(verbose)
			print(thresholds)
		return(getconf(thresholds[[sensitivity.bias]]))

	}

	# easy peasy: no case.values between the controls and no ties at the specificity level
	thresholds <- list( 'mean'   = mean(boundary)
				,'median' = mean(boundary)
				,'lower'  = boundary[1]
				,'upper'  = boundary[2]
				,'proportional' =  boundary[1] + diff(boundary)*((spec - pboundary[1])/diff(pboundary))
				)
	if(verbose)
		print(thresholds)
	return.val <- 
		getconf(thresholds[[sensitivity.bias]])
	return(return.val)

}

#' Calculate a threshold from a set of marker values, and associted case status
#'
#' Calculate a threshold from a set of marker values, and associted case status.
#' This is a particularly complicated topic, because there is a lot of choice in 
#' how to calculate a threshold from a small dataset. In particular, if there
#' are wide gaps in beween sequential marker values in the dataset, there is a 
#' range of thresholds which result in identical specificity levels (and possibly
#' differing sensitivity levels) in your fitting dataset.
#'
#' The choice of threshold will influence the precision and repoducability of
#' the estimated threshold.
#'

#' @param x a vector of marker values from unaffected individuals
#' @param probs Specificity threshold(s) for the returned threshold(s). 
#' @param specificity.bias one of ('over','under') which deterens how the specificity is reported.  
#' @param case.values observed values from the case.values
#' @param sensitivity.bias on of ('lower','upper','median','mean','proportional') 
#' which deterimens how the senwitivity
#' is reported when the probability falls between data points.
#' @param na.rm If TRUE, missing values are dropped from x and case.values. (default = FALSE)
#' @param ci If TRUE, a confidence interval is included in the outputs (default = FALSE)
#' @param conf.level confidence level of the interval. (default = 0.95)
#' @param verbose If TRUE, thresholds are printed out. (default = FALSE)
#' @param quietly If TRUE, warning messages regarding setting boundaries 
#' at outlying positions (such as "Threshold is the highest value among 
#' all observations") are suppressed. (default = FALSE).
#' 
#' @export
#' @examples
#' # an example where a control ties the specificity level
#' threshold(x = 1:10, 
#'           probs = 0.9) # by default the threshold is higher than the value
#' 
#' # an example where two controls have the same value
#' # and the specificity is between them
#' threshold(x = c(2:10,9),
#'           probs = 0.9) # by default the threshold is higher than the tied values
#' 
#' # an example where there are case.values between the 
#' # two controls just above and below the specificity level
#' threshold(x = 1:10,
#'           probs = 0.95,
#'           case.values = 9+((1:9/10)))
#' threshold(x = 1:10,
#'           probs = 0.95,
#'           case.values = 9+((1:9/10)),
#'           sensitivity.bias = 'median')
#' # note that since the mean would return a threshold 
#' # equal to observed value, it defaults to the median
#' threshold(x = 1:10,probs = 0.95,
#'           case.values = 9+((1:9/10)),
#'           sensitivity.bias = 'mean') 
#' threshold(x = 1:10,probs = 0.95,
#'           case.values = 9+((1:9/10)),
#'           sensitivity.bias = 'lower')
#' threshold(x = 1:10,probs = 0.95,
#'           case.values = 9+((1:9/10)),
#'           sensitivity.bias = 'upper')
#' 
#' # an example with the specificity at the top level
#' threshold(x = c(1,1:10,10),probs = 0.99)
#' # an example with the specificity at the bottom level
#' threshold(x = c(1,1:10,10),probs = 0.01,
#'           specificity.bias='under')
#' 
#' probs <- 0.70
#' temp <- c()
#' n = 50
#' x = round(rnorm(n),2)
#' for(i in 0:n){
#' 	(temp[i+1] <- sum( x < threshold(x=x,probs = i/n)))
#' }
#' max(abs(temp - (0:n)))
#' max(table(x))
#' threshold(x = rnorm(20)
#'		 ,probs =0.99
#'		 , case.values = rnorm(20) + 1
#'		 ,specificity.bias='over'
#'		 ,sensitivity.bias='proportional'
#'		 )

threshold <- Vectorize(THRESHOLD,'probs')
#-- threshold <- function(x,probs,...){
#-- 	out <- NULL
#-- 	for(p in probs)
#-- 		out <- c(out,THRESHOLD(x,probs=p,...))
#-- 	return(out)
#-- }



#' Empirical thresholds corresponding to given specificities
#'
#' Empirical thresholds corresponding to given specificities.
#' @param x A vector of marker values
#' @param ref an optional reference group.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param spec A vector specificities for the returned thresholds
#' @return A list containing the sensetivities, specificiees, and the corresponding thresholds (cutoffs)
#' @export
#' @seealso \code{\link{threshold}}


ecdf2 <- function(x,
				  ref,
				  #,interpMethod='proportional'
				  spec,
				  na.rm = FALSE){
	if(missing(ref))
		ref <- x# an optional reference group

	interpMethod='proportional'
	interpMethod_options <- c('proportional','ordered','between')
	#print(specificity.bias)
	interpMethod <- interpMethod_options[pmatch(interpMethod,interpMethod_options)]
	#print(interpMethod)
	if(is.na(interpMethod))stop("interpMethod should be one of ('proportional','ordered','between')")

	if(na.rm)
		ref <- ref[!is.na(ref)]
	if(any(is.na(ref)))stop("ref (x if missing) cannot contain NA's if na.rm == FALSE")

	(x <- sort(x))
	(Ex <- (1:length(x))/(length(x) + 1)) # expected values of the order statistics
	(boundary <- x[ c( last(Ex <= spec,none=1         )
					 ,first(Ex >= spec,none=length(x) ))
					 ])
	(pboundary <- Ex[ c( last(Ex <= spec,none=1         )
					 ,first(Ex >= spec,none=length(x) ))
					 ])

	if(!diff(boundary)){
		#-- warning('ties were found at the boundary')
		# ties in the controls at the specificity level
		# result in ties at the boundary. Include too many contorls (sens.bias = under)
		# or too few controls (spec bias = over)?


		#if the specificity is above the expected value for the highest order statistic
		#DESIRED THRESHOLD IS ABOVE THE CONTROLS
		if(boundary[1] == max(x)){
			if( max(case.values) <= (boundary[1])){
				if(!quietly)
					warning('Threshold is the highest value among all observations')
				return(boundary[1])
			}
			#else:
			if(!quietly)
				warning('Threshold is higher than all observed values in the controls')
			return( (min(case.values[case.values > (boundary[1])]) + (boundary[1]))/2 )
		}

		#if the specificity is below the expected value for the highest order statistic
		#DESIRED THRESHOLD IS BLOW THE CONTROLS
		if(boundary[1] == min(x)){
			if( min(case.values) >= (boundary[1])){
				if(!quietly)
					warning('Threshold is the lowest value among all observations')
				return(boundary[1])
			}
			if(!quietly)
				warning('Threshold is lower than all observed values in the controls')
			return( (max(case.values[case.values < (boundary[1])]) + (boundary[1]))/2)
		}

		# the specificity is within the range of expected values of the order statistics but
		# THERE ARE TIES IN THE DATA AND THE DESIRED THRESHOLD IS BETWEEN THE TIED VALUES!
		# Now we need to choose if the threshold has to be a liitle
		# too high or a little to low
		if(specificity.bias == 'over'){
			#over specific means choose a higher threshold
			if( all(c(x,case.values) <= (boundary[1]))){
				#there are no higher values
				if(!quietly)
					warning('Threshold is the highest value among all observations')
				return(boundary[1])
			}
			if( all(x <= (boundary[1]))){
				if(!quietly)
					warning('Threshold is higher than all observed values in the controls')
			}else{
				if(!quietly)
					warning('There were ties among the controls at the desired specificity; threshold is above the tie and identifeis too few controls')
			}
			return( (min(c(x    [x     > (boundary[1])]
						  ,case.values[case.values > (boundary[1])]
						  ))
				    + (boundary[1])
				    )/2)
		}else{#specificity.bias == 'under'
			#under specific means choose a lower threshold (=over fpr)
			if( all(c(x,case.values) >= (boundary[1]))){
				if(!quietly)
					warning('Threshold is the lowest value among all observations')
				return(boundary[1])
			}
			if( all(x <= (boundary[1]))){
				if(!quietly)
					warning('Threshold is lower than all observed values in the controls')
			}else{
				if(!quietly)
					warning('There were ties among the controls at the desired specificity; threshold is below the tie and identifeis too many controls')
			}
			#the average of (the max of all values below the threshold) and (the boundary value)
			return( (max(c(x    [x     < (boundary[1])] #NOTE THAT boundary[1] == boundary[2]
						  ,case.values[case.values < (boundary[1])]
						  ))
				    + (boundary[1])
				    )/2)
		}
	}

	# The desired threshold lies between two controls with discinct values.
	# Check if there are any case.values between the boundary controls and if so
	# Choose an appropriate threshold WRT the distribution of those cases.

	#-- print(boundary)
	#-- print(sum(( (case.values > boundary[1])
	#-- 	     & (case.values < boundary[2]))))
	if( (length(case.values) > 0)
		& any( (case.values > boundary[1])
		     & (case.values < boundary[2]))){

		#warning('	case.values lie between the boundaries')
		#get the boundary points (controls) and the case.values between the boundaries
		pts <- sort(c( boundary[1]
					, case.values[(case.values > (boundary[1]))
						   &(case.values < (boundary[2]))]
					, boundary[2]
					))
		if(length(pts) <=2)stop('error in selecting case.values between boundaries')
		(midpoints <- ((pts[-1] + pts[-length(pts)] )/2))

		mid.lower <- function(x)x[floor((length(x) + 1)/2)]
		#mid.upper <- function(x)x[ceiling((length(x) + 1)/2)]

		out <- list( 'mean'   = mean(boundary)
					,'median' = mid.lower(midpoints)
					,'lower'  = min(midpoints[midpoints > (boundary[1]) ])
					,'upper'  = max(midpoints[midpoints < (boundary[2]) ])
					,'proportional' = midpoints[first( ((1:length(midpoints))/(length(midpoints)+1))
															> ((spec - pboundary[1])/diff(pboundary))
														,none = length(midpoints))]
					)

		#if there are ties with the median and a boundary, default to the upper (lower)
		if( out[['median']] == (boundary[1]))out[['median']] <- out[['lower']]
		if( out[['median']] == (boundary[2]))out[['median']] <- out[['upper']]

		if( is.na(out[['proportional']]))out[['proportional']] <- out[['median']]

		# if the mean would give a threshold equal to observed value, default to the median
		if( out[['mean']] %in% case.values)out[['mean']] <- out[['median']]

		if(any(unlist(out) %in% boundary))stop("Error in 'threshold'")
		return(out[[sensitivity.bias]])
	}
	# easy peasy: no case.values between the controls and no ties at the specificity level

	out <- list( 'mean'   = mean(boundary)
				,'median' = mean(boundary)
				,'lower'  = boundary[1]
				,'upper'  = boundary[2]
				,'proportional' =  boundary[1] + diff(boundary)*((spec - pboundary[1])/diff(pboundary))
				)
	return(out[[sensitivity.bias]])
}

