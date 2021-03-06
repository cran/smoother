\name{smth.gaussian}
\alias{smth.gaussian}
\title{Smooth Using Gaussian Window}
\usage{
smth.gaussian(x = stop("Numeric Vector 'x' is Required"),
  window = getOption("smoother.window"),
  alpha = getOption("smoother.gaussianwindow.alpha"), ...,
  tails = getOption("smoother.tails"))
}
\arguments{
  \item{x}{numeric vector of values to smooth, error will
  be thrown if not provided.}

  \item{window}{the length of the smoothing window, if an
  integer, represents number of items, else, if a value
  between \code{0} and \code{1}, represents the proportion
  of the input vector}

  \item{alpha}{parameter to determine the breadth of the
  gaussian window, yielding more or less sensitive
  smoothing characteristics}

  \item{...}{not used}

  \item{tails}{Logical value as to whether the tail regions
  should be included or not.}
}
\description{
The specific function for smoothing using the gaussian
window function
}
\examples{
y  = runif(100)
  ys = smth.gaussian(y)
}

