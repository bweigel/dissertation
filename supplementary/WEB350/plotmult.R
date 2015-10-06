plotmult <-function(msspecs){
  if(!is.list(msspecs)) stop("Supply a list with xy spectra!")
  msspecs <- lapply(msspecs, function(x){ 
    x[,2] <- x[,2]/max(x[,2])
    return(x)
    }) 
  plot(msspecs[[1]], type="h", xlab="m/z", ylab="rel. intensity")
  cols <- rainbow(length(msspecs)-1)
  mapply(function(x, col) {
    lines(x=x[,1]+0.5, y=x[,2], type="h", col=col)
    return(NULL)}, msspecs[2:length(msspecs)], cols)
}

library(data.table)
plotmult2 <-function(msspecs){
  if(!is.list(msspecs)) stop("Supply a list with xy spectra!")
  par(mfrow=c(length(msspecs), 1))
  
  msspecs <- lapply(msspecs, function(x){ 
    x[,2] <- x[,2]/max(x[,2])
    return(x)
  }) 
  
  tmp <- (rbindlist(lapply(msspecs, as.data.frame))) %T>% print
  xminmax <- c(min(tmp["V1"]), max(tmp["V1"]))
  rm(tmp)
  
  cols <- rainbow(length(msspecs))
  lapply(msspecs, FUN=function(xy){
    plot(x=xy[,1], y=xy[,2], type="h", xlab="m/z", ylab="rel. intensity", xlim=xminmax)
   # smooth.spline(xy,) %>% lines(., lty=2)
    return(NULL)})
}