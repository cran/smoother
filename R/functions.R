#Function to determine the actual window length from possibly decimal (fractional) window length
.determineWindow <- function(w,l){
  if(w > 0 && w < 1){w = w*l} #If Fractional
  as.integer(max(abs(w[1]),1))#Positive Integer
}

#Ifthenelse function
.ifthenelse <- function(t,y,n){if(t){y}else{n}}