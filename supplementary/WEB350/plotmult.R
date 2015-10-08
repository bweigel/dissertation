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
library(dplyr)
plotmult2 <-function(msspecs, names, tr, ...){
  if(!is.list(msspecs)) stop("Supply a list with xy spectra!")
  
  layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE))
  par(mar=c(2, 4, 2.5, 2), mgp=c(1.2,0.5,0))
  #par(mfrow=c(length(msspecs), 1))
  
  names <- factor(names, names, names, ordered = T)
  names <- factor(names, names[order(names, decreasing = T)], names[order(names, decreasing = T)], ordered = T)
  msspecs <- lapply(msspecs, function(x){ 
    x[,2] <- x[,2]/max(x[,2])
    return(x)
  }) 
  
  #tmp <- (rbindlist(lapply(msspecs, as.data.frame))) 
  tmp <- rbindlist(
            mapply(FUN=function(xy, nom){list(cbind(data.frame(X=xy[,1], Y=xy[,2]/max(xy[,2])), id=nom))}, xy=msspecs, nom=names)) 
  xminmax <- c(min(tmp[,1,with=F]), max(tmp[,1,with=F]))
  #rm(tmp)
  
  cols <- rainbow(length(msspecs))
  mapply(FUN=function(xy, names){
    plot(x=xy[,1], y=xy[,2], type="h", xlab="m/z", ylab="rel. intensity", xlim=xminmax, ...)
    mtext(names, side=3, line=0.5, cex=1.2, adj=0, srt=90)
    # smooth.spline(xy,) %>% lines(., lty=2)
    invisible(NULL)}, xy=msspecs, names=names)
  
  #tmp %>% group_by(id) %>% mutate(Z )
  #density()
  tmp <- tmp[which(tmp$Y>=tr/100),] 
  boxplot(X~id, data=tmp, horizontal = T, par=list(labels=F), ylim=xminmax)
  #text(seq(1,length(unique(tmp$id)),by = 1), par("usr")[2] - 0.2, labels = unique(tmp$id), srt = 45, pos = 1, xpd = TRUE)
  
#   lapply(msspecs, FUN=function(xy){
#     plot(x=xy[,1], y=xy[,2], type="h", xlab="m/z", ylab="rel. intensity", xlim=xminmax)
return(NULL)
}

plotmult2(mssy, c("A", "B", "C"), tr=2)
