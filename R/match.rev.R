


#' Reverse a match index
#' 
#' Reverse a match index, which can save time when the index takes a long time to create
#' 
#' @param x A match index, often the result of calling `x <- match(a,b)`
#' @param len the length of the the retusulting index [i.e. `length(b)`]
#' 
#' @export match.rev
match.rev <- function(x,len){
	if(any(duplicated(x) & !is.na(x)))
		warning('Reversed match is not unique; using first match only.')
	stopifnot(all(x <= len,na.rm=TRUE))
	match(seq.int(len),x)
}


# unit testing ==================================================
#-- 
#-- i1 = sample(letters)
#-- i2 = sample(letters,50,TRUE)
#-- 
#-- mm <- match(i2,i1)
#-- MM <- suppressWarnings(match.rev(match.rev(mm,26),50))
#-- .mm <- mm
#-- .mm[duplicated(mm)] <- NA
#-- cbind(.mm,MM)
#-- identical(.mm,MM)
#-- 
#-- ss <- match(i1,i2)
#-- SS <- suppressWarnings(match.rev(match.rev(ss,50),26))
#-- .ss <- ss
#-- .ss[duplicated(ss)] <- NA
#-- cbind(.ss,SS)
#-- identical(.ss,SS)
#-- 
#-- 
#-- d1 <- rnorm(26)
#-- (d2 <- d1[mm])
#-- d3 <- d2[suppressWarnings(match.rev(mm,26))]
#-- identical(!is.na(d3),i1 %in% i2)
#-- identical(d3,ifelse(i1 %in% i2,d1,NA))
#-- 
#-- 
#-- D1 <- rnorm(50)
#-- (D2 <- D1[ss])
#-- D3 <- D2[suppressWarnings(match.rev(ss,50))]
#-- identical(!is.na(D3),!duplicated(i2))
#-- identical(D3,ifelse(!duplicated(i2),D1,NA))

