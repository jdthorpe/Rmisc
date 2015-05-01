



#' A weak sandbox in which to \code{source()} files.
#' 
#' A weak sandbox in which to \code{source()} files.  Arguments are 
#' passed on to \code{source()}, and \code{source()} is called with a 
#' new environment(\code{source(... , local = environment())}), in which
#' \code{source()} has been redfined to evaluate in the newly created 
#' environment.  
#' 
#' What this allows for is running scripts that won't clutter your current
#' or global environment, even if those scripts call \code{source()} [which
#' would ordinarliy be evaluated in the current environment (\code{source(... , local = TRUE)}) 
#' or the global environment (\code{source(... , local = FALSE)}) ]
#' 
#' In addition, some attempts are made at restoring global variables (e.g. 
#' \code{options()}) and any libraries that are loaded by scripts that are  
#' \code{source()}'ed will be unloaded, in order to restore the search path.
#' 
#' Note that \code{sandbox()} makes \emph{no} attemtp to reload libraries 
#' that are unloaded by a script that is \code{source()}'ed. 
#' 
#' This is \emph{NOT} a sandbox for evaluating potentially malicious scripts.
#' Specifically, this does not prevent scripts from manipulating the local environment
#' (e.g. via \code{file()},\code{read.table()},\code{write.table()}, and \emph{many}
#' other functions).  Also, this sandbox can be escaped by simply including
#' \code{base::source()} wihtin a script that is evaluated.  
#' 
#' @param file a connection or a character string giving the pathname of the file or 
#' URL to read from. "" indicates the connection stdin().
#' @param local an environment in which to evaluate the script \code{file}
#' @param ... Addiional arguments to \code{link[base]{source}}
#' @export

sandbox <- function(file,
					 local = eval.parent(quote(environment())),
					 ...){
	# note that this works b/c source(...,local=TRUE) evaluates in the current environment,
	# and we're taking advantage of hte fact that a new environment is created
	# for each funciton call

	stopifnot(is.environment(local))
	if(identical(.GlobalEnv,local))
	 
	# capture some sessoin info
	..ss.. <- sessionInfo()
	..op.. <- options()
	..envir.. <- local
	on.exit({
		options(..op..)
		packages = sessionInfo()$otherPkgs
		if(!is.null(packages) 
		   && length(ls(pattern='\\.\\.ss\\.\\.',all.names=TRUE))){
			packages <- names(packages)
			if(!is.null(..ss..$otherPkgs))
				packages<- setdiff(packages,names(..ss..$otherPkgs))
			for(pkg in packages)
				eval(parse(text=paste0("detach(package:",pkg,",unload=TRUE)")))
		}
	})
	source <- function(file, local, ...)
		base::source(file,
					 local=..envir..,
					 ...)
	# This is a very week sandbox!!!
	base::source(file,local=..envir..,...)
	
}

