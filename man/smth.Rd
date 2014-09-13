\name{smth}
\alias{smth}
\title{Smooth Numerical Data}
\usage{
smth(x, window = 0.1, method = "gaussian")
}
\arguments{
  \item{x}{numeric vector of values to smooth}

  \item{window}{the length of the smoothing window, if an
  integer, represents number of items, else, if a value
  between \code{0} and \code{1}, represents the proportion
  of the input vector. Default value is \code{0.1}}

  \item{method}{only accepts \code{'gaussian'} at this
  stage.}
}
\value{
a numeric vector of same length as input \code{'x'} vector
}
\description{
Function to smooth numerical data using methods specified
by the user.
}
\details{
At this moment in time, the only method is the
\code{'gaussian'} window function, similar to the Matlab
Gaussian Window Smoothing Function. This is a function
which allows the user to smooth an input vector, returning
vector of the same length as the input.
}
\examples{
\donttest{
#Prepare Data
n  = 1000
x  = seq(-pi,pi,length.out=n)
y  = sin(x) + (runif(length(x))*0.1) #NOISY DATA
ys = smth(y,window = 0.1,method = "gaussian") #SMOOTHING

#If ggplot2 is installed, plot data using ggplot2
if(require(ggplot2)){
  df = data.frame(X,y,ys)
  ggplot(data=df) +
    geom_path(aes(x,y),color="darkgray") +
    geom_path(aes(x,ys),color="red",size=1) +
    theme_bw() +
    labs(title="Example Smoothing of Noisy Data")

#Else use standard graphics
}else{
  plot(x,y,type="l",col="red")
  lines(x,ys,col="black",lwd=3)
  title("Example Smoothing of Noisy Data")
}
}
}
\references{
If the \code{'method'} argument is equal to
\code{'gaussian'}, then this function is a port of the
function described here: \url{http://goo.gl/HGM47U}, very
loosly based of code which has also been ported to c++
here: \url{http://goo.gl/NK79bJ}
}

