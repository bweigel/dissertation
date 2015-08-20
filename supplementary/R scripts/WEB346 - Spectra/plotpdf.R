library(readMzXmlData)
library(MSnSpec)


######################################################
############## LCMS Sample runs
######################################################
DIR <- "/media/mori/Stuff/LCMS/WEB2015-II/MzXML/"
#files <- dir("/media/mori/Stuff/LCMS/WEB2015-II/MzXML", pattern = "(mzXML)$")

files <- dir(DIR, pattern = "(mzXML)$")

data <- vector("list", length = 0)
traces <- vector("list", length = 0)
pb <- txtProgressBar(min = 0, max = length(files), style = 3)
#pdf("WEB346_Chromatograms.pdf", paper = "a4", width = 10, height = 14.1)
i <- 0
for(file in files[]){
  setTxtProgressBar(pb, i)
  tmp <- tryCatch(expr = {readMzXmlFile(mzXmlFile = paste(DIR,file,sep=""))},
           error = function(e)return(NA))
  if(!is.na(tmp)) {
    tmp <- .uniqueTraces(tmp)
    tmp <- spectra.peaks(tmp, max.p = 4, )
    data <- append(data, list(tmp$peaks))
    traces <- append(traces, list(tmp$metascans$MS.2))
    layout(matrix(c(1,2,3,4),4,1))
    plot(tmp,sep=F,relative=T) 
    plot(tmp,sep=T,relative=F)
    plotSpectra.MSnX(tmp, relative = T, annotate = T, ylim=c(0,1.3), tresh=0.05)
  } else {
    plot.new()
    mtext(paste("Something went afoul with file:", file))
  }
  rm(tmp)
  i <- i + 1
}
dev.off()

rm(files)

