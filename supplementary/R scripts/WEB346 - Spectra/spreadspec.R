library(magrittr)

spreadSpec <- function(spectra, cuts=0.001, cutoff=1000){
  ## calculate bins freom spectra and cutsize
  mzmax <- lapply(spectra, function(x)x[,1]) %>% unlist
  mzmax <- mzmax[!is.na(mzmax)]
  mzmin <- mzmax %>% min %>% floor
  mzmax %<>% max %>% ceiling
  breaks <- seq(from=mzmin, to=mzmax, cuts)
  
  ## bin spectra and combine into matrix
  mat <- lapply(spectra, function(x) {
    n <- .bincode(x[,1], breaks=breaks, include.lowest = T)
    
    tmp <- matrix(data = c(breaks, rep(NA, length(breaks))), ncol = 2, byrow = F)
    
    tmp[,2][n[!is.na(n)]] <- x[,2][!is.na(n)]
    return(tmp[,2])
  }) %>% unlist %>% matrix(., nrow=length(spectra), byrow = T)
  
  ## calculate column means
  #mat <- colMeans(mat, na.rm = T)
  mat <- apply(mat, c(1,2), function(x){
    if(!is.na(x)){
      if(x <= cutoff) return(NA) else return(x)
    } else  return(x)
  }
  )
  IND <- -which(colSums(apply(mat, 2, function(x)is.na(x))) == nrow(mat))
  mat <- mat[,IND]
  breaks <- breaks[IND]
  ## return spectrum, remove data that is NA
  return(rbind(breaks, mat))
}
