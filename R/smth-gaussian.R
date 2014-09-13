#Hidden Gaussian Window Function
.makeGaussianWindow <- function(windowWidth,alpha=2.5){
  halfWidth = as.integer(abs(windowWidth/2.0))
  e = exp(1)
  sapply(c(0:(windowWidth-1)),function(x){
    n = x - halfWidth
    k = alpha*n/as.numeric(halfWidth)
    k = -0.5*k*k
    e^k
  })
}

#Hidden Normalization Function, sums weights to 1.
.normalize <- function(x){
  if(!is.numeric(x)){stop("argument 'x' must be numeric")}
  s = sum(x)
  if(s == 0){stop("argument 'x' must have positive sum")}
  x/s
}

#Hidden Convolve Function, taking numeric data, and window set
.convolve <- function(d,w){
  if(!is.numeric(d) | !is.numeric(w)){stop("argument 'd' and 'w' must be numeric")} #Do some basic checks.
  
  #Length of F & G Vectors
  sizeW = length(w)
  sizeD = length(d)
  
  #Normalize the window
  w = .normalize(w)
  
  #Prepare 
  hkwL    = as.integer(sizeW/2) #Half Kernel Width, for left
  hkwR    = sizeW - hkwL        #Half Kernel Width, rhs balance to total width
  
  #Forward Sweep
  sapply(c(1:sizeD),function(i){
    ix.d = c((i-hkwL):(i+hkwR-1))   #The ideal range
    ix.w = which(ix.d %in% 1:sizeD) #Suitable Window Values, index must be 1..n
    ix.d = ix.d[ix.w]               #Suitable data values for windows
    
    #Determine Normalized Final Window and Data
    W.nm = .ifthenelse(length(ix.w) != sizeW,.normalize(w[ix.w]),w) #Use existing window if possible, else re-normalize
    D.nm = d[ix.d] 
    
    #Aggregate
    sum(D.nm*W.nm) 
  })
}

#Internal function to smooth based on the matlab gaussian window smoothing function
.smth.gaussian <- function(x,window){.convolve(x,.makeGaussianWindow(window))}

