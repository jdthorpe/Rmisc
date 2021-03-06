% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/ROC-plot.r
\name{plot.roc}
\alias{plot.roc}
\title{Plot ROC curves and calculate sensitivity / specificity statistics}
\usage{
plot.roc(x, cases, comparison.label = "", add = FALSE, color = "colorize",
  print = plot, print.auc = print, print.col = if (color == "colorize")
  "blue" else color, print.loc = c(0.5, 0.5), spec = c(1, 0.98, 0.95, 0.9),
  lwd = 3, plot = TRUE, summarize = FALSE, xlab = "False Positive Rate",
  ylab = "True Positive Rate", cex.text = 1.2, cex.label = cex.text,
  gridlines, threshold.args = NULL, units, digits = 3,
  digits.pvalue = digits, digits.auc = digits, conf.int = FALSE,
  conf.level = 0.95, ...)
}
\arguments{
\item{x}{A vector of marker values}

\item{cases}{A logical vector where TRUE indicates that the corresonding marker value in X
is taken from a 'Case', and FALSE otherwise.}

\item{comparison.label}{A Sting with a label for the comparison printed above the
included in the plot sensitivities and specifities}

\item{add}{If TRUE, the ROC curve is added to an existing plot. (default = FALSE)}

\item{color}{The color of the ROC curve, or 'colorize' if a color scale is used to color
the curve. (default = 'colorize')}

\item{print}{If TRUE, the comparison label, AUC, and/or sensitiviies are added to the fugure. (default = plot) # print a summary of the values}

\item{print.auc}{If TRUE, the AUC is added to the fugure. ,}

\item{print.col}{Color of the text added to the figure (default blue, or 'col' if specified')}

\item{print.loc}{location of the text added to the fugure. (default = c(0.5,0.5))}

\item{spec}{Specificities for which sensitivities should be included in the figure and in the
list that is returned (invisibly) by plot.roc (default = c(1.00,0.98,0.95,0.90))}

\item{lwd}{Line width of the ROC curve. (default = 3)}

\item{plot}{If TRUE, the ROC cuve is plotted. (default = TRUE) ('plot=FALSE' is handy for setting up a figure)}

\item{summarize}{If TRUE, a list of all possible sensitivities and specificities is included
in the list returned (invisibly) by plot.roc (default = FALSE)}

\item{xlab}{x-axis label (default = 'False Positive Rate')}

\item{ylab}{y-axis label (default = 'True Positive Rate')}

\item{cex.text}{Character expansion parameter for displayed text (default = 1.2)}

\item{cex.label}{Character expansion parameter for the 'label' parameter (default = cex.text) ,}

\item{gridlines}{If set to TRUE or a numeric vector, gridlines are displayed}

\item{threshold.args}{(optional) a list of arguments to \code{\link{threshold}}}

\item{units}{units of measurement of the marker (optional, printed in the right margin)}

\item{digits}{number of decimals in the AUC and P-Value (default = 3)}

\item{digits.pvalue}{number of decimals in P-Value (default = digits)}

\item{digits.auc}{number of decimals in the AUC (default = digits)}

\item{conf.int}{Should confidence intervals be included? (default = FALSE)}

\item{conf.level}{confidence level (default = 0.95)}

\item{...}{additional arguments plot()}
}
\value{
a list, returned invisibly, containing the AUC, numbers
of cases and controls, p-value for the AUC, the plot location of the
printed text, and the sensitivities and specifities included in the
figure, and optional a summary of the ROC curve (if summarize = TRUE).
}
\description{
Plot ROC curves and calculate sensitivity / specificity statistics.
}
\examples{
plot.roc( x = rnorm(100) + rep(c(1,0),50),
		case = rep(c(TRUE,FALSE),50),
		comparison.label = 'Cases v. Controls',
		main = 'My Favorite Bio-Marker',
		color = 'brown3',
		print = TRUE,
		print.col = 'green4',
		print.loc = c(0.5,0.32),
		lty = 3,
		lwd = 5,
		spec  = c(1.00,0.9,0.85,0.8),
		xlab = 'False Positive Rate (x axis)',
		ylab = 'True Positive Rate (y axis)',
		gridlines = 1:10/10)
}

