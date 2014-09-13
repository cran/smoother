#' Smooth Numerical Data
#' 
#' @description Function to smooth numerical data using methods specified by the user. 
#' 
#' @details At this moment in time, the only method is the \code{'gaussian'} window function, similar to the Matlab Gaussian Window Smoothing Function. This is a function which allows the 
#' user to smooth an input vector, returning vector of the same length as the input.
#' @param x numeric vector of values to smooth
#' @param window the length of the smoothing window, if an integer, represents
#' number of items, else, if a value between \code{0} and \code{1}, represents the proportion of the input
#' vector. Default value is \code{0.1}
#' @param method only accepts \code{'gaussian'} at this stage.
#' @return a numeric vector of same length as input \code{'x'} vector
#' @references If the \code{'method'} argument is equal to \code{'gaussian'}, then this function is a port of the function 
#' described here: \url{http://goo.gl/HGM47U}, very loosly based of code which has also been ported to c++ here:
#' \url{http://goo.gl/NK79bJ}
#' @exportMethod
#' @examples
#' \donttest{
#' #Prepare Data
#' n  = 1000
#' x  = seq(-pi,pi,length.out=n)
#' y  = sin(x) + (runif(length(x))*0.1) #NOISY DATA
#' ys = smth(y,window = 0.1,method = "gaussian") #SMOOTHING
#' 
#' #If ggplot2 is installed, plot data using ggplot2
#' if(require(ggplot2)){
#'   df = data.frame(X,y,ys)
#'   ggplot(data=df) + 
#'     geom_path(aes(x,y),color="darkgray") + 
#'     geom_path(aes(x,ys),color="red",size=1) + 
#'     theme_bw() +
#'     labs(title="Example Smoothing of Noisy Data")
#'     
#' #Else use standard graphics
#' }else{
#'   plot(x,y,type="l",col="red")
#'   lines(x,ys,col="black",lwd=3)
#'   title("Example Smoothing of Noisy Data")
#' }
#' }
smth <- function(x,window=0.1,method="gaussian"){
  bup <- x
  if(!is.numeric(x) | !is.numeric(window)){stop("Arguments 'x' and 'window' must be numeric")}
  
  #Determine the Window
  window = .determineWindow(window,length(x))
  
  #Check
  if(length(x) <= window | window < 2){
    warning("Window length is out of range, returning un-smoothed vector")
    return(bup)
  }
  if(method == 'gaussian')
    return(.smth.gaussian(x,window))
  else
    stop("Argument 'method' can only be gaussian at this momemnt in time.")
  
  #Default Return Vector
  return(bup)
}