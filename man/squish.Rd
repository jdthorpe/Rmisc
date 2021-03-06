% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/my-r-programs.r
\name{squish}
\alias{squish}
\title{Squish}
\usage{
squish(x, rc = 0, AddNames = TRUE)
}
\arguments{
\item{x}{a summary table}

\item{rc}{indicates if the table should be squished column by column (rc=0, default) or row by row(rc=1)}

\item{AddNames}{logical. Should the varialbe names be aded to the dim names.}
}
\description{
Squishes a summary tabble nicely into two dimensions
}
\examples{
n = 1000
temp <- data.frame('A' = letters[1:5][ceiling(runif(n)*5)]
				  ,'B' = LETTERS[1:3][ceiling(runif(n)*5)]
				  ,'C' = LETTERS[1:5][ceiling(runif(n)*5)]
				  ,'D' = ceiling(runif(n)*3)
				  ,'E' = rnorm(n)
				  )
tapply(temp$E,temp[,c('A','B','C')],sum)# ugly
squish(tapply(temp$E,temp[,c('A','B','C')],sum))
tapply(temp$E,temp[,c('A','B','C','D')],sum)# very ugly
squish(tapply(temp$E,temp[,c('A','B','C','D')],sum))
}

