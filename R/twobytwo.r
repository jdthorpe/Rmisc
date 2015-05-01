# --------------------------------------------------------------------------------
# Programmer: Jason Thorpe
# Date        Tuesday, September 17, 2013 7:42:39 AM
# Language:   R (.r) Version 2.15.0
# --------------------------------------------------------------------------------

#   -------------
#   |  a  |  c  |
#   -------------
#   |  b  |  d  |
#   -------------

#' Calculate the contents of a 2 by 2 table from any 4 (independent) values.
#'
#' Calculate the contents, margins, and/or Odds Ratio from any 4 (independent) values of a two by two table.
#'
#' Every onece in a while, a the contents of a two-by-two table need to
#' be calculated based on the margins, odds ratios, and/or sum of the entries, which 
#' can be a nuisance to calculate by hand.  Hence \code{twoByTwo} calculates a two-by-two matrix, from any
#' combination of 4 parameters which fully specify the contents of a two by two table.
#' parameters are named based on the following table :
#' \tabular{cc}{a \tab c \cr b \tab d }
#'
#' @param ... four arguments either [a] with of the format a,b,ab,cd,acd,abcd,etc. which represent 
#' the sum of 1 or more cells of the matrix 
#' \tabular{cc}{a \tab c \cr b \tab d }
#' with the parameter names being alphabetical lists of the cells which they sum.
#' or [b] the parameter \code{odds.ratio} with the odds ratio (\code{(a/b)/(c/d)})
#' @export
#' @examples
#' 
#' # most trivial of all, same as matrix(1:4,2)
#' twoByTwo(a= 1,b = 2,c = 3,d = 4) 
#'
#' # still trivial
#' twoByTwo(a= 1,ab=2,abc=3,abcd=4) 
#' twoByTwo(c= 1,ab=2,ac=3,abcd=4)
#'
#' # not too difficult, but anoying to have to calculate by hand
#' twoByTwo(odds.ratio = 1,ab=2,ac=3,abcd=5)
#' twoByTwo(odds.ratio = 1.001,ab=2,ac=3,abcd=5)
#'
#' \dontrun{
#' 	# error 'n' is not a valid parameter, use 'abcd' instead
#' 	twoByTwo(a= 1,ab=2,ac=3,n=4) 
#'
#' 	# error: the parameter ab appears twice
#' 	twoByTwo(ab= 1,ab=2,ac=3,abcd=4) 
#'
#' 	# error: insufficient parameters
#' 	twoByTwo(odds.ratio = 1.001,c=2,ac=3,abcd=5) 
#' }

twoByTwo <- function(...){
	# a utility function: inversion of odds ratio parameters
	ABCD <- function(N,# total sample size (surgeries)
					 L,# total number of events (lesions)
					 E,# total number of individuals at elevated risk
					 Q)# the odds ratio
	{
		if(Q == 1){# special case
			D = L*E/N 
			return(list(A=N-L-E+D,
					 B=L-D,
					 C=E-D,
					 D=D))
		}
		quadraticDeterminant <- function(a,b,c)
			(b^2 - 4*a*c)
		if(quadraticDeterminant(c = Q*L*(E-N),
				 b = (1-Q)*(E-L) + Q*N,
				 a = 1-Q) <0)
			stop('no real roots')
		quadraticRoots <- function(a,b,c)
			c((-b+sqrt(b^2 - 4*a*c))/(2*a),
			  (-b-sqrt(b^2 - 4*a*c))/(2*a))
		roots <- quadraticRoots(c = Q*L*(E-N),
				 b = (1-Q)*(E-L) + Q*N,
				 a = 1-Q)
		if(all(roots <=0))
			stop('no positive roots')
		B = min(roots[roots>0])
		stopifnot(B < N)
		return(list( A=N-E-B,
					 B=B,
					 C=E+B-L,
					 D=L-B))
	}
	#-- 	# just checking
	#-- 	NLEQ <- function(A,B,C,D){
	#-- 		return(list( N = A + B + C + D,
	#-- 					 L = B + D,
	#-- 					 E = C + D,
	#-- 					 Q = (A*D)/(B*C)))
	#-- 	}
	#-- 	do.call(ABCD,NLEQ(A=100,B=10,C=200,D=5))
	# a list of valid parameter names
	paramNames <- c('a', 'b', 'c', 'd',
				'ab', 'ac', 'ad', 'bc', 'bd', 'cd',
				'abc', 'abd', 'acd', 'bcd',
				'abcd', 'odds.ratio')
	paramList <- list(...)
	if(length(paramList) != 4)
		stop('please provide exactly 4 parameters')
	if(length(unique(names(paramList))) != 4)
		stop('please provide exactly 4 unique parameters')
	if(!all(lu(sapply(paramList,length)) == 1))
		stop('all parameters must have the same length')
	if(any(sapply(paramList,is.na)))
		stop('missing values not allowed')
	if(!all(names(paramList) %in% paramNames)){
		if('n' %in% names(paramList))
			stop("'n' is not a valid paramter, use 'abcd' instead.",
				 if(sum(!names(paramList) %in% paramNames)>1)
					 paste('Invalid parameters include: ',
						   paste("'",names(paramList)[!names(paramList) %in% paramNames],"'",
								 collapse = ', ',sep = '')))
		stop(paste('Invalid parameters: ',
				   paste("'",names(paramList)[!names(paramList) %in% paramNames],"'",
						 collapse = ', ',sep = '')))
	}
	# GET A LIST OF ALL THE POSSIBLE COMBINATIONS OF PARAMETERS THAT CAN BE USEFUL
	mx <- t(outer(paramNames[1:15],
		  paramNames[1:15],
		  function(x,y){
			  out <- character(0)
			  for(i in 1:length(x))
				  out[i] <- gsub(paste('[',x[i],']',sep = ''), '',y[i])
			  return(out)
		  }))
	dimnames(mx) <-  list(paramNames[1:15],paramNames[1:15])
	mx[mx == paramNames[1:15]] <- ""
	mx[t(mx) != ""] <- ""
	wmx <- which(mx != "")
	mx <- t(mx)
	mx[nchar(mx) < nchar(paramNames[1:15])] <- ""
	mx <- t(mx)
	wmx <- which(mx != "")
	big = paramNames[1:15][((wmx-1) %% 15 ) + 1]
	little1 = paramNames[1:15][((wmx-1) %/% 15 ) + 1]
	little2 = mx[mx != ""]
	stopifnot(length(big) == 34)
	stopifnot(length(little1) == 34)
	stopifnot(length(little2) == 34)
	data.frame(big, little1, little2)
	# DO THE WORK
	while(TRUE){
		# IF THE SOLUTION IS AVAILABLE, RETURN IT
		if(all(letters[1:4] %in% names(paramList)))
			return(matrix(unlist(paramList[letters[1:4]]),2))
		if(all(c('odds.ratio','bd','cd','abcd') %in% names(paramList))){
			call.params <- paramList[c('abcd','bd','cd','odds.ratio')]
			names(call.params) <- c('N','L','E','Q')
			vals <- try(do.call(ABCD,call.params),TRUE)
			if(inherits(vals,'try-error'))
				stop('No Such Matrix')
			return(matrix(vals,2))
		}
		# THE SOLUTION IS NOT AVAILABLE, SO TRY AND MAKE SOME PROGRESS
		# TOWARD THE SOLUTION.
		for(i in 1:34){
			if((little1[i] %in% names(paramList))
			   &(little2[i] %in% names(paramList))
			   &(!big[i] %in% names(paramList))
			   ){
				paramList[[big[i]]] <- paramList[[little2[i]]] + paramList[[little2[i]]]
				break
			}
			if((little1[i] %in% names(paramList))
			   &(!little2[i] %in% names(paramList))
			   &(big[i] %in% names(paramList))
			   ){
				paramList[[little2[i]]] <- paramList[[big[i]]] - paramList[[little1[i]]]
				break
			}
			if((!little1[i] %in% names(paramList))
			   &(little2[i] %in% names(paramList))
			   &(big[i] %in% names(paramList))
			   ){
				paramList[[little1[i]]] <- paramList[[big[i]]] - paramList[[little2[i]]]
				break
			}
			# NO PROGRESS MADE = NO SOLUTION
			if(i == 34){
				if('odds.ratio' %in% names(paramList)){
					stop('Insufficient parameters: to use an odds ratio, you must provide one value from each margin.')
				}else stop('Insufficient parameters: hmmm... thats wierd.')
			}
		}
	}
}






