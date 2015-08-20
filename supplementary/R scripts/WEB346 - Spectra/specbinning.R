library(mzR)
library(dplyr)
load("peaks.Rda")

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


dat <- rbindlist(data) %>% as.data.frame

spec <- vector("list", length = 0)
for(fil in unique(dat$full.path)){
  MS <- openMSfile(fil)  
  spec <- append(spec, peaks(MS, subset(dat, full.path == fil, select = scan) %>% unlist))
}

rm(list = c("DIR", "LCdata", "MS", "data", "data.df", "pb", "tmp", "file", "fil", "i"))

## get a matrix of all spectra
spec <- spreadSpec(spec, cuts=0.1, cutoff = 5000)
spec.df <- t(spec) %>% as.data.frame
tmp <- is.na(spec.df) %>% colSums
spec.df <- spec.df[,-which(tmp == nrow(spec.df))]
spec.df[is.na(spec.df)] <- 0
pca <- prcomp(na.omit(t(spec.df[,-1])), center=T, scale=T)

layout(matrix(c(1,2,3,4),2,2))
biplot(pca, choices=c(1,2))
biplot(pca, choices=c(2,3))
biplot(pca, choices=c(1,4))
biplot(pca, choices=c(4,5))
summary(pca)
a <- na.omit(t(spec.df[,-1]))
kmeans(t(spec.df[,-1]), centers = 5)


CO <- 5000
x<-apply(spec.df, 2, function(x){
  var(x[which(x > mean(x, na.rm = T)/5)], na.rm = T)/var(x[which(x < mean(x, na.rm = T))/5], na.rm = T)
})
x[order(x)][1:20]


x<-apply(spec.df, 2, function(x){
  mean(x, na.rm = T)/var(x, na.rm = T)
})
x[order(x)][1:20]


keep <- vector()
for()
