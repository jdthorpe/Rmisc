# --------------------------------------------------------------------------------
# Programmer: Jason Thorpe
# Date        Monday, August 12, 2013 6:23:36 AM
# Language:   R (.r) Version 2.15.0
# Purpose:    
# Comments:   
# --------------------------------------------------------------------------------

# --------------------------------------------------------------------------------
# main interface
# --------------------------------------------------------------------------------

#' Summarize balance of a variable between two groups
#' 
#' @export
#' @param x a variabile to be compared between levels of `ltwo` 
#' @param ltwo # a logical variable for the second dimension of the resulting table
#' @param variable.desc = deparse(substitute(x)) # name of the variable x
#'
#' @param digits.p.value precision of p-value (default=4)
#'
#' @param poisson = FALSE # should INTEGER values be treated as counts. If True, 
#' p-values are calculated using poisson regresson?
#' SPECIFIC TO NUMERIC, INTEGER VARIABLES 
#' 
#' @param digits.meansd=1 deciaml precision when the summary statistic is  
#' the mean and sd of x 
#' 
#' specific to logical variables
#' @param lyesonly if TRUE the number of false reponses is dropped from the output table 
#' 
#' @param na.as.category If TRUE, missing values are treated as if they are a 
#' separate caterory for variables that are compared as categorical variables 
#' NOT APPLICABLE FOR WHEN X IS NUMERIC OR  INTEGER.
#' 
#' @param na.inline If \code{TRUE}  missing values are reported in the bottom row, 
#' \strong{otherwise} in an additional column is added to the table with summaries
#'  of the missing values (na.inline=FALSE).  (default = TRUE)
#' 
#' @param na.always If \code{TRUE}, a count of the missing values always included in the summary table (default = FALSE)?
#' 
#' @param na.never If \code{TRUE}, missing values are ignored (default = FALSE)
#' 
#' @param ... # additional arguments to npercent methods
#' @seealso \code{\link{summarize}} for summarizeing a variable 
twobyn <- function(x, # x is any variabile to be compared between levels of `ltwo` 
				  ltwo, # a logical variable for the second dimension of the resulting table
				  variable.desc = deparse(substitute(x)), # name of the variable x
				  ...)
{
	# --------------------------------------------------
	# --------------------------------------------------
	argNames <- names(list(...))
	if('xlab' %in% argNames)
		stop('Arguments"xlab" deprecated. Use "variable.desc" instead')
	if('lname' %in% argNames)
		stop('Arguments "lname" and "desc" deprecated, and replaced with "variable.desc"')
	if('desc' %in% argNames)
		stop('Arguments "lname" and "desc" deprecated, and replaced with "variable.desc"')
	if('digits' %in% argNames)
		stop('Argument "digits" deprecated.  Use "digits.meansd" instead.')
	if('np' %in% argNames)
		stop('Argument "np" deprecated.  Use "digits.p.value" instead.')
	if('lof' %in% argNames)
		stop('Argument "lof" deprecated.  Use "of" instead.')
	if('mof' %in% argNames)
		stop('Argument "mof" deprecated.  Use "of" instead.')

	if(missing(ltwo)&missing(x))
		stop("'x' and 'ltwo' missing without defaults")
	if((!missing(x)) && default(poisson,inherits(x,'integer'),...) ){
		xname = "Argument 'x'"
		if(mode(match.call()$x) == 'name')
			try(xname <- paste("Variable '", as.character(mode(match.call()$x)),"'",sep = ''),TRUE)
		if((mode(match.call()$x) == 'call' )&&
		   identical(eval(match.call()$x[[1]]),`[`)){
			if(length(match.call()$x) == 4)
				try(xname <-  paste("Field '", 
								  as.character(eval((match.call()$x)[[4]])),
								  "'",
								  sep = ''),TRUE)
			else if(length(match.call()$x) == 3)
				try(xname <-  paste("Field '", as.character(eval((match.call()$x)[[3]])),"'",sep = ''),TRUE)
		}
		if((!(class(x)[1]%in%c('integer','numeric'))))
			stop("argument 'poisson=TRUE' incompatile with ",xname," of class:",paste(class(x),collapse = ', '))
		if(any((!is.na(x)) & (round(x) != x)))
			warning(paste('In twobyn(), not all values of',xname,'are integers'))
	}
	if(missing(ltwo)|missing(x)){
		# --------------------------------------------------
		# CREATE A HEADER (SAMPLE SIZE MATRIX)
		# --------------------------------------------------
		if(missing(ltwo) && (mode(x) != 'logical'))
			stop("Variable 'ltwo' is missing and x is not of mode 'logical'")
		if(missing(x) && (mode(ltwo) != 'logical'))
			stop("Variable 'ltwo' is not of mode 'logical'")

		na.inline <- default(na.inline,TRUE,...)
		na.always <- default(na.always,FALSE,...)
		na.never  <- default(na.never ,FALSE,...)

		# SET UP THE RETURN MATRIX

		if(!missing(variable.desc)) {
			False.desc <- paste(variable.desc,"False",sep = '.')
			True.desc <- paste(variable.desc,"True",sep = '.')
			False.missing.desc <- paste(variable.desc,"False.missing",sep = '.')
			True.missing.desc  <- paste(variable.desc,"True.missing",sep = '.') 
		}else{
			False.desc <- "False"
			True.desc <- "True"
			False.missing.desc <- "False.missing"
			True.missing.desc  <- "True.missing"
		}
		if(na.inline | na.never)
			out <- matrix("",nrow=1,ncol=6,
						  dimnames = list('Sample Size',
										  c("Variable", 
											"Value",
										   	False.desc,
										   	True.desc,
										   	"p.value",
										   	"test")))
		else
			out <- matrix("",nrow=1,ncol=8,
						  dimnames = list('Sample Size',
										  c("Variable",
										   	"Value",
										   	False.desc,
										   	True.desc,
										   	"p.value",
										   	"test",
											False.missing.desc ,
											True.missing.desc  )))
		out[1,'Variable'] <- variable.desc
		out[1,'Value'] <- 'Sample Size'

		# FILL IN THE DATA
		if(missing(x)){
			if(any(is.na(ltwo)))
				stop("Missing values in 'ltwo'")
			out[1,False.desc] <- npercent(!ltwo)
			out[1,True.desc ] <- npercent(ltwo)
		}else{
			if(any(is.na(x)))
				stop("Missing values in 'x'")
			out[1,False.desc] <- npercent(!x)
			out[1,True.desc ] <- npercent(x)
		}
		return(out)
	} else
		UseMethod('twobyn')
}

#' @export
twobynmat <- function(...)
	stop("twobynmat() is deprected: use twobyn(ltwo,variable.desc='') or twobyn(x,variable.desc='')")

# --------------------------------------------------------------------------------
# a helper function that fomrats the output matrix
# --------------------------------------------------------------------------------
twobyn_fmt <- function(variable.desc ,			# Field: Variable
					   lvls=names(tbl.true), 	# Field: Value
					   tbl.false, 				# Field: False
					   tbl.true, 				# Field: True
					   missing.false, 			# Field: False, (Missing)
					   missing.true, 			# Field: True, (Missing)
					   p.value, 				# Field: p.value
					   test,					# Field: test
					   ...
#-- 					   na.inline=TRUE, # setting: include missing values at the bottom (TRUE) or right (FALSE)
#-- 					   na.always=FALSE, # setting: Always include the missing values
#-- 					   na.never=FALSE,  # setting: Never include the missing values
					   ){

	stopifnot(identical(names(tbl.false),names(tbl.true)))
	if(is.null(variable.desc))
		variable.desc <- "x" 
	na.inline <- default(na.inline,TRUE,...)
	na.always <- default(na.always,FALSE,...)
	na.never  <- default(na.never ,FALSE,...)

	if(na.always & na.never )
		stop('Invalid paramters "na.always=TRUE" & "na.never=TRUE"')

	if(na.inline | na.never ){
		missing.values <- any(c(missing.false,missing.true)!= 'None')
		if(((!missing.values)| na.never) & (!na.always)){
			# like inline, but no missings
			blanks <- rep('',length(tbl.true)-1)
			out <- cbind('Variable'=c(variable.desc,blanks ),
						'Value'=lvls,
						'False'=tbl.false,
						'True'=tbl.true,
						'p.value'=c(paste(p.value), blanks),
						'test'=c(test, blanks))
		}else{
			blanks<- rep('',length(tbl.true))
			out <- cbind('Variable'=c(variable.desc, blanks),
						'Value'=c(lvls     ,'Missing'),
						'False'=c(tbl.false,'Missing'=missing.false),
						'True' =c(tbl.true ,'Missing'=missing.true),
						'p.value'=c(paste(p.value), blanks),
						'test'=c(test, blanks))
		}
	}else{
		blanks <- rep('',length(tbl.true)-1)
		out <- cbind('Variable'=c(variable.desc, rep('',length(tbl.true)-1)),
					'Value'=lvls,
					'False'=tbl.false,
					'True'=tbl.true,
					'p.value'=c(paste(p.value), blanks),
					'test'=c(test,blanks),
					'False.missing'=c(if(missing(missing.false)) "" else missing.false,
									  blanks),
					'True.missing'= c(if(missing(missing.true)) "" else missing.true,
									  blanks))
	}
	return(out)
}

# --------------------------------------------------------------------------------
# a helper function which pulls variables from the ... argument with an optional 
# default value
# --------------------------------------------------------------------------------
default <- function(arg,default,exact=FALSE,...){
	# for use in a function as in default('arg',default=3,...)
	arg <- deparse(substitute(arg))
	ARGS <- list(...)
	if(sum(names(ARGS) == arg)>1)
		stop(paste('Multiple arguments passed with name "',arg,'"',sep = ""))
	if(any(names(ARGS) == arg))
		return(ARGS[[arg]])
	if(exact){
		if(missing(default))
			stop(paste('Missing argument "',arg,'" and no default provided',sep = ""))
		else
			return(default)
	}
	m <- pmatch(names(ARGS),arg,nomatch=0,duplicates.ok=TRUE)
	if(!any(!!m)){
		if(missing(default))
			stop(paste('Missing argument "',arg,'" and no default provided',sep = ""))
		else 
			return(default)
	} else if(sum(!!m)>1){
		stop(paste('Multiple matches to "',arg,'", including: ',
				   paste(names(ARGS)[!!m],collapse = ', '),
				   sep = ""))
	}else
		return(ARGS[[which(!!m)]])
}

#-- default('sin')
#-- default('sin',pi)
#-- default('sin',pi,p.value=2,sin=3)
#-- default('sin',pi,p.value=2,sin=3,sin=4)
#-- default('sin',pi,p.value=2,si=3,si=4)
#-- default('sin',pi,p.value=2,si=3,s=4)
#-- default('sin',pi,p.value=2,sin=3,exact=TRUE)
#-- default('sin',pi,p.value=2,si=3,exact=TRUE)
#-- default('sin',p.value=2,si=3,exact=TRUE)



# --------------------------------------------------------------------------------
# the default for the generic (just a nice error message)
# --------------------------------------------------------------------------------
twobyn.default <- function(x,ltwo,variable.desc,...){
	if(missing(x)){
		if(missing(ltwo))
			stop('No summary variables specified')
		twobyn_fmt(variable.desc=if(missing(variable.desc)) deparse(substitute(ltwo)) else variable.desc ,
					lvls='N',
					tbl.false=npercent(!ltwo,na.rm = TRUE,...),
					tbl.true=npercent(ltwo,na.rm = TRUE,...),
					p.value="",
					test="",
					missing.false = npercent(missing(ltwo),na.rm = TRUE,...),
					missing.true  = "",
					)
	}else{
		cat('I dont know how to summarize a "',
			class(list(...)[[1]]),
			'" variable\n',file='')
	}
}

# --------------------------------------------------------------------------------
# Method: logical
# --------------------------------------------------------------------------------

#' @export
#' @method twobyn logical 
#' @family twobyn
#' @inheritParams twobyn
twobyn.logical <- function(x,ltwo,
				  variable.desc = deparse(substitute(x)), # name of the variable x
				  ...){
	force(variable.desc)
	ctestname <- 
		try({
			p.value <- tochar( fisher.test(table(ltwo,x))$p.value,
								  digits=default('digits.p.value',4,...))
			"Fisher's Exact Test"
		} ,silent = TRUE )
	if( inherits(ctestname,'try-error'))
		ctestname <- 
			try({
				p.value <- tochar( chisq.test(table(ltwo,x))$p.value,
									  digits=default('digits.p.value',4,...));
				'Chi-Square'
			} ,silent = TRUE )
	if( inherits(ctestname,'try-error')){
		p.value <- NA
		ctestname <- NA
	}
	lyesonly <- default(lyesonly,FALSE,...)
	na.inline <- default(na.inline,TRUE,...)
	na.as.category <- default(na.as.category,FALSE,...)

	if(na.as.category){
		# include the na's in the denominator explicitly
		tbl.false<- c('No'=if(!lyesonly) 
									npercent(sum(!x[!ltwo],na.rm=TRUE),
											 sum(!ltwo), ...),
					  'Yes'=npercent(sum(x[!ltwo],na.rm=TRUE),
										  sum(!ltwo), ...))
		tbl.true <- c('No'=if(!lyesonly)
									npercent(sum(!x[ ltwo],na.rm=TRUE),
											 sum( ltwo), ...),
					  'Yes'=npercent(sum(x[ ltwo],na.rm=TRUE),
										  sum( ltwo), ...))
	}else{
		tbl.false<- c('No'=if(!lyesonly) 
									npercent(!x[!ltwo], na.rm=default('na.rm',TRUE,...), ...),
					  'Yes'=npercent( x[!ltwo], na.rm = TRUE, ...))
		tbl.true <- c('No'=if(!lyesonly)
									npercent(!x[ ltwo], na.rm=default('na.rm',TRUE,...), ...),
					  'Yes'=npercent( x[ ltwo],na.rm = TRUE,...))
	}
	out <- twobyn_fmt(variable.desc=variable.desc,
					  tbl.false=tbl.false,
					  tbl.true =tbl.true,
					  p.value = p.value,
					  test=ctestname,
					  lvls = c(default(true.desc,'Yes',...),
							   if(!lyesonly)default(false.desc,'No',...)),
					  # if no missings, missing.true and missing.false *must* be 'None'
					  missing.false=ifelse(sum(is.na(x[!ltwo])),
											npercent( is.na(x[!ltwo]),...),
											'None'),
					  missing.true =ifelse(sum(is.na(x[ ltwo])),
											npercent( is.na(x[ ltwo]),...),
											'None'),
					  ...)
	return(out)
}

# --------------------------------------------------------------------------------
# Method: integer and numeric
# --------------------------------------------------------------------------------

#' @export 
#' @method twobyn Date 
#' @family twobyn
#' @inheritParams twobyn
twobyn.Date <- function(x,...){
	twobyn.numeric(unclass(x),...)
}

#' @export
twobyn.numeric <- function(x,ltwo
				  ,variable.desc = deparse(substitute(x)), # name of the variable x
						   ...){
	force(variable.desc)
	if(default(poisson,inherits(x,'integer'),...)){
		if((!inherits(x,'integer')) &&
		   (!all(as.integer(x) == x)))
			stop('argument "x" incompatible with poisson regression')
		x <- as.integer(x)
		lm1 <- glm(x ~ ltwo,data = data.frame(x=x,ltwo=ltwo),family = 'poisson')
		p.value <- coef(summary(lm1))["ltwoTRUE","Pr(>|z|)"]
		out <- twobyn.factor(x=as.factor(x), 
							 ltwo = ltwo,
							 variable.desc=variable.desc,
							 p.val=FALSE,
							 ...)
		out[1,'p.value'] <- tochar( p.value,
							 digits=default('digits.p.value',4,...))
		out[1,"test"] <- 'Poisson Regression'
	}else{
		if(inherits(x,'integer')){
			xname = "Argument 'x'"
			if(mode(match.call()$x) == 'name')
				try(xname <- paste("Variable '", as.character(mode(match.call()$x)),"'",sep = ''),TRUE)
			if((mode(match.call()$x) == 'call' )&&
			   identical(eval(match.call()$x[[1]]),`[`)){
				if(length(match.call()$x) == 4)
					try(xname <-  paste("Field '", 
									  as.character(eval((match.call()$x)[[4]])),
									  "'",
									  sep = ''),TRUE)
				else if(length(match.call()$x) == 3)
					try(xname <-  paste("Field '", as.character(eval((match.call()$x)[[3]])),"'",sep = ''),TRUE)
		   	}
			warning(paste('In twobyn(),',xname,'is an integer evaluated as continuous'))
		}
		worked <-  try({
			p.value  <- t.test(x=x[!ltwo],y=x[ ltwo],na.rm = TRUE)$p.value;
			ctestname <- "Student's tRUE-test"
		},silent = TRUE )
		if( inherits(worked,'try-error')){
			p.value <- NA
			ctestname <- "NA"
		}
		digits <- default(digits.meansd,1,...)
		if( length(digits)==1)
			digits <- c(digits,digits)

		out <- twobyn_fmt(variable.desc=variable.desc,
						  tbl.false=c('mean(sd)'=meansd( x[!ltwo],digits=digits,na.rm=TRUE)),
						  tbl.true =c('mean(sd)'=meansd( x[ ltwo],digits=digits,na.rm=TRUE)),
						  p.value = p.value,
						  test=ctestname,
						  # if no missings, missing.true and missing.false *must* be 'None'
						  missing.false=ifelse(sum(is.na(x[nvl(!ltwo,FALSE)])),
												npercent(is.na(x[nvl(!ltwo,FALSE)]),...),
												'None'),
						  missing.true =ifelse(sum(is.na(x[ nvl(ltwo,FALSE)])),
												npercent( is.na(x[ nvl(ltwo,FALSE)]),...),
												'None'),
						  ...)
	}
	return(out)
}

#' @export
twobyn.integer <- twobyn.numeric
# --------------------------------------------------------------------------------
# Method: factor and character
# --------------------------------------------------------------------------------

#' @export
twobyn.character <- function(x,ltwo
				  ,variable.desc = deparse(substitute(x)), # name of the variable x
				  p.val=TRUE,...){

	force(variable.desc)
	# ----------- p.value -----------
	if(p.val && lu(x,na.rm = TRUE)>1){#fisher.test throws an error otherwise
#-- 		expr_fisher_test <- parse(text="p.value <-tochar( fisher.test(table(ltwo,x))$p.value,digits=default('digits.p.value',4,...))")
		worked <- try({
			p.value <-tochar( fisher.test(table(ltwo,x))$p.value,
							 digits=default('digits.p.value',4,...));
			test <- "Fisher's Exact Test"
			}, TRUE )
		if( inherits(worked,'try-error')){
			p.value <-tochar( chisq.test(table(ltwo,x))$p.value,
							 digits=default('digits.p.value',4,...))
			test <- 'Chi-Square'
		}
	}else{
		p.value <- NA
		test <- NA
	}

	# ----------- table -----------
	na.inline <- default(na.inline,TRUE,...)
	na.as.category <- default(na.as.category,FALSE,...)
	if(na.as.category){
		tbl <- list('TRUE'=npercent(x[(!is.na(x))&ltwo],sum(ltwo)),
				   	'FALSE'=npercent(x[(!is.na(x))&!ltwo],sum(!ltwo)))
	}else{
		tbl <- tapply(x,ltwo,npercent,na.rm=TRUE)
	}

	# ----------- output -----------
	out <- twobyn_fmt(variable.desc=variable.desc,
					  tbl.true=tbl$'TRUE',
					  tbl.false=tbl$'FALSE',
					  p.value = p.value, 
					  test=test,
					  # if no missings, missing.true and missing.false *must* be 'None'
					  missing.false=ifelse(sum(is.na(x[!ltwo])),
											npercent( is.na(x[!ltwo]),...),
											'None'),
					  missing.true =ifelse(sum(is.na(x[ ltwo])),
											npercent( is.na(x[ ltwo]),...),
											'None'),
					  ...)
	return(out)
}

#' @export
twobyn.factor <- twobyn.character 



