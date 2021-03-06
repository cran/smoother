\name{smth.options}
\alias{smth.options}
\title{Smoother Options}
\description{
Several Global Options have been declared, as described in
this help file.
}
\details{
The following global options can be modified, to alter the
default calculation behaviour. \tabular{lll}{ \strong{NAME}
\tab \strong{VALUE} \tab \strong{DESCRIPTION} \cr
\code{smoother.gaussianwindow.alpha} \tab \code{2.5} \tab
Alpha Value in Calculating Window \cr
\code{smoother.window} \tab \code{0.1} \tab Width of Window
\cr \code{smoother.method} \tab \code{'gaussian'} \tab
Default Smoothing Method \cr \code{smoother.tails} \tab
\code{FALSE} \tab Include tails in final vector \cr
\code{smoother.verbose} \tab \code{FALSE} \tab Verbose
Reporting \cr }
}
\examples{
#Tighten the alpha term for this session.
options('smoother.gaussianwindow.alpha' = 1)

#Include the Tails in Final Calculation
options('smoother.tails' = TRUE)
}

