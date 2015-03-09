# --------------------------------------------------------------------------------------
# Programmer: Jason Thorpe
# Date        02/28/2008
# Purpose:    these are the programs and variabiles that I leave in the R environment.
# Comments:   dont forget to add function names to h:\vimfiles\r-plugin\functions.vim
# --------------------------------------------------------------------------------------

# --------------------------------------------------
# handy constants
# --------------------------------------------------

#' @export
temp.pdf<- file.path(Sys.getenv("R_USER"),'temp\\temp.pdf')
#' @export
temp.csv<- file.path(Sys.getenv("R_USER"),'temp\\temp.csv')


#' @export
empty <- function (x) 
	(is.na(x) | grepl("^[ \t\n]*$", x))

# --------------------------------------------------------------------------------------
# miscelaneous functions
# --------------------------------------------------------------------------------------

#' Replacement of missing values 
#' 
#' @param x a vector, possibly with missing values
#' @param val replacement values for x
#' @keywords cats
#' @export
#' @examples
#' x <- letters 
#' (is.na(x) <- !! 1:26 %% 2)
#' nvl(x,'Hi')
#' nvl(x,LETTERS)
#' \notrun{ nvl(x,c('Hello','World')) }
nvl <- function(x,val){
	if(!any(is.na(x)))
		return(x)
	if(length(val) == 1){
		#coerse x to character if necessary
		if(inherits(x,'factor') && !(val %in% levels(x)))
			x <- as.character(x)
		x[is.na(x)] <- val
	}else {
		if(length(x)!=length(val))
			stop('length(val) must be in c(1,length(x))')
		#coerse x to character if necessary
		if(inherits(x,'factor') && !all(val[is.na(x)] %in% levels(x)))
			x <- as.character(x)
		x[is.na(x)] <- val[is.na(x)]
	}
	return(x)
}


#' The opposite of \%in\%
#'
#' lets face it \code{(!x \%in\% y} is just ugly syntax.
#' @param x vector or NULL: the values to be matched.  Long vectors are supported
#' @param table vector or NULL: the values to be matched against.  Long vectors are not supported.
#' @export
`%not in%` <- function(x,table)
	 match(x, table, nomatch = 0) == 0
	#return(!x%in%y) 

#' Number of unique elements of a vector 
#'
#' Number of unique elements of a vector 
#' @param x a vector
#' @param na.rm If \code{TRUE} missing values are considered a unique value (Default = \code{FALSE})
#' @export
lu<-function(x,na.rm = FALSE){
	x <- unique(x)
	if(na.rm)
		x <- x[!is.na(x)]
	length(x)
}

#' Trim leading and trailing white space from character vectors
#'
#' Trim leading and trailing white space from character vectors.
#' @param x A character vector
#' @return x, trimmed of leading and trailing white space
#' @export
#' @examples
#' trim(c('  hello world\t'))
trim <- function (x) {
        # inspired by trim from the package gdata
        x <- as.character(x)
        x <- sub(pattern = "^[ \t]+", replacement = "", x = x)
        x <- sub(pattern = "[ \t]+$", replacement = "", x = x)
        x
}

##########################################################################################
# squish a summary tabble into two dimensions
##########################################################################################

#' Squish
#'
#' Squishes a summary tabble nicely into two dimensions
#' @param x a summary table
#' @param rc indicates if the table should be squished column by column (rc=0, default) or row by row(rc=1)
#' @export
#' @examples
#' n = 1000
#' temp <- data.frame('A' = letters[1:5][ceiling(runif(n)*5)]
#' 				  ,'B' = LETTERS[1:3][ceiling(runif(n)*5)]
#' 				  ,'C' = LETTERS[1:5][ceiling(runif(n)*5)]
#' 				  ,'D' = ceiling(runif(n)*3)
#' 				  ,'E' = rnorm(n)
#' 				  )
#' tapply(temp$E,temp[,c('A','B','C')],sum)# ugly
#' squish(tapply(temp$E,temp[,c('A','B','C')],sum))
#' tapply(temp$E,temp[,c('A','B','C','D')],sum)# very ugly
#' squish(tapply(temp$E,temp[,c('A','B','C','D')],sum))

squish <- function(x,rc=0,AddNames = TRUE){
	# x is a table with (presumably) more than two dimensions as in "table(data[,c('a','b','c')])
	stopifnot(rc %in% 0:1)
	if(length(dim(x))<=2)return(x)

	#add the variable names to the dimanmes
	if(AddNames)
		for(i in 1:length(dim(x)) )
			dimnames(x)[[i]] <- paste( names(dimnames(x))[i],'="',dimnames(x)[[i]],'"',sep = '')

	#squish the sub-factors
	lsquished <- apply(x,
					   length(dim(x)),
					   function(x)
						   list(squish(x,rc=rc,AddNames=FALSE))) #lsquished is a list of 2x2 tables

	#put the sub factors togeather
	if(length(dim(x))%%2==rc ){ #we're going to squish the levels and then CBIND them toteather

		#side 2 is cbind specific
		side <-  2
		for(i in 1:length(lsquished))
		dimnames(lsquished[[i]][[1]])[[side]] <-
			paste(names(lsquished)[i],dimnames(lsquished[[i]][[1]])[[side]],sep = '\n')

		out <- lsquished[[1]][[1]]
		if(length(lsquished) == 1)
			return(out)

		for(i in 2:length(lsquished))
			out <- cbind(out,lsquished[[i]][[1]] )
	}else{#we're going to squish the levels and then RBIND them toteather

		#side 1 is rbind specific
		side = 1
		for(i in 1:length(lsquished))
			dimnames(lsquished[[i]][[1]])[[side]] <-
				paste(names(lsquished)[i],dimnames(lsquished[[i]][[1]])[[side]] )

		out <- lsquished[[1]][[1]]
		if(length(lsquished) == 1)
			return(out)

		for(i in 2:length(lsquished))
			out <- rbind(out,lsquished[[i]][[1]] )
	}
	return(out)
}



##########################################################################################
##########################################################################################

#' Reverse a vector
#'
#' Reverse a vector
#' @param x a vector
#' @export
reverse <- function(x)
	x[length(x):1]


#' The position (indicator) of the first or last value that evaluates to \code{TRUE}
#'
#' The position (indicator) of the first or last value that evaluates to \code{TRUE}.
#' @param x a vector to be searched
#' @param NONE a default return value if none of the values in X evaluate to \code{TRUE}
#' @export
first <- function(x,none=NA){
	x[is.na(x)] <- FALSE
	if(any(x))
		return(which.max(x))
	else 
		return(none)
}

#' @export
#' @rdname first
last <- function(x,none=NA){
	out <- first(reverse(x),none=none)
	if(identical(none,out)){return(none)}else{return(length(x) - out + 1)}
}



#' I can't really remember what this function does.
#'
#' I can't really remember what this function does
#' @export
matchapply <- function(x,xfactor,fkey,FUN,na,...){
	factored <- tapply(x,xfactor,FUN,...)
	link <- match(fkey,names(factored))
	out <- factored[link]
	if(!missing(na))
		out[is.na(link)] <- na
	out
}


############################################################
############################################################

#' Converts numbers into their string ordinals
#'
#' Converts numbers into their string ordinals.
#' @param x an integer vector
#' @export
#' @examples
#' cnum(1:5)
cnum<-function(x){
	stopifnot(inherits(x,'integer'))
	x<-floor(x)
	out<-character(0)
	for(i in 1:length(x))
	{
		if(((x[i] %% 100)>20) | ((x[i] %% 100)<10))
		{
			if( (x[i] %% 10) == 1){out[i]<-paste(x[i],'st',sep = '')}
			if( (x[i] %% 10) == 2){out[i]<-paste(x[i],'nd',sep = '')}
			if( (x[i] %% 10) == 3){out[i]<-paste(x[i],'rd',sep = '')}
			if( (x[i] %% 10) %in% c(0,4:9) ){out[i]<-paste(x[i],'th',sep = '')}
		}else{	out[i]<-paste(x[i],'th',sep = '')	}
	}
	out
}


#' Summary Statisics for an Anova Object
#'
#' Summary Statisics for an Anova Object.
#' @param x an object of class 'anova'
#' @export
#' @examples
#' summary(aov(y ~ x ,data=data.frame(y = rnorm(50),x=gl(5,10,50,LETTERS[1:5]))))
summary.anova <- function(x){# x, an object of class 'anova'
	#note this is only tested for anova(lm1,lm2) where both models are both of class "lm" or "glm"
	(ss1 = abs(diff(x[,2]))				)
	(df1 = abs(diff(x[,1]))				)
	(ss2 = min(x[,2])					)
	(df2 = min(x[,1])					)
	(f = ((ss1/df1)/(ss2/df2))			)
	(p = pf(f,df1,df2,lower.tail = FALSE)	)
	(b1 = 1-(min(x[,2])/max(x[,2]))		)
	cat('F-statistic:',f,'on',df1,'and',df2,'DF, p-value:',tochar(round(p,3),digits = 3),'\n')
	return(list(p=p,F=f,df1=df1,df2=df2,b1=b1))
}

