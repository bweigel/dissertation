###################################################
## Write *.MSP files for NIST import from spetra ##
###################################################
library(stringr)
library(data.table)
library(magrittr)
library(dplyr)

load("spectra.Rda")
load("peaks.Rda")

#### dictionary

msms <- list("Coumpound type" = "01", 
             "Instrument type" = "06",
             "Spectrum type" = "00",
             "Precursor type" = "03",
             "Precursor m/z" = "04",
             "Collision energy" = "05",
             "Instrument" = "07",
             "Sample inlet" = "09",
             "Ionization" = "10",
             "Ion mode" = "11",
             "Cone voltage" = "16"
)

####################################################
# spectrum - matrix (of spectrum (x/y))
# name - character vector (name of compound)
# meta - named list with key-value pairs (Formula, MW, CAS, Synonym)
# digits - round m/z to that many digits
####################################################
# relative abundances are rounded to the nearest tenth
####################################################
spec2MSP <- function(spectrum, name = NULL, ms2 = NULL, meta = NULL, comment=NULL, digits = 3){
  if(!is.matrix(spectrum)) stop("Spectrum supplied is not in matrix format!")
  if(is.null(name)) stop("Please supply a name!")
  if(!is.null(meta)){
    if(!is.list(meta)) message("'meta' needs to be a named list!")
    meta <- paste(str_c(names(meta), meta, sep = ": ", collapse = "\n"), "\n", sep="")
  }
  if(!is.null(ms2)){
    if(!is.list(ms2)) message("'ms2' needs to be a named list!")
    ms2 <- paste(str_c("Synon: $:", lapply(names(ms2), function(x)msms[[x]]), ms2, sep = "", collapse = "\n"), "\n", sep="")
  }
  
  num.peaks <- nrow(spectrum)
  
  # convert abundance to relative abunda<nce
  spectrum[,2] <- spectrum[,2]/max(spectrum[,2])*100
  
  
  # convert spectrum to MSP format
  spectrum <- paste(str_c(round(spectrum[,1], digits),
                          round(spectrum[,2], 1),
                          sep = " ", collapse = ";\n"), ";", sep="")
  
  
  comment <- paste(str_c(names(comment), comment, sep = "=", collapse = " "), "\n", sep="")
  
  
  # final MSP format including metadata
  MSP <- paste("Name: ", name, "\n",
               ms2,
               meta,
               "Comment: ", comment,
               "Num Peaks: ",  num.peaks, "\n",
               spectrum,
               sep="")
  
  return(MSP)
}


##############################################################
## Put all spectra in a library
##############################################################


spec.df <- data %>% rbindlist

msp <- vector()
for(i in 1:length(spec)){
  name <- spec.df[i,file]
  
  method <- str_split(spec.df[i,method], pattern = "[.]") %>% unlist
  comment <- list(method = method[1],
               eV = method[2],
               Parent = method[4],
               "ret. time (min)" = round(spec.df[i,rt]/60,2),
               "scan" = spec.df[i,scan]
                 )  
  
  mode <- spec.df[i,file] %>% str_extract(., "[np]MS2") %>% str_extract(., "[np]")
  ms2 <- list("Coumpound type" = "M", 
              "Instrument type" = "QTOF",
              "Spectrum type" = "ms2",
              "Precursor type" = ifelse(mode == "p", yes = "[M+H]+", "[M-H]-"),
              "Precursor m/z" = method[4],
              "Collision energy" = paste(method[2], "eV"),
              "Sample inlet" = "LC",
              "Ionization" = "ESI",
              "Ion mode" = mode)
  rm(mode)
  msp <- append(msp, spec2MSP(spec[[i]], name = paste(name, "---", spec.df[i,scan]), comment = comment, ms2=ms2))
}

msp <- str_c(msp, collapse = "\n")
cat(msp, file="~/.wine//drive_c//NISTDEMO//MSSEARCH/WEB346_flavonoids.msp")

##############################################################
## Use only spectra that we have identofied
##############################################################
library(mzR)

what <- read.csv("~/IPB/thesis/supplementary/WEB346_LCMS.csv", sep=";", stringsAsFactors=F)
what %<>% filter(ok) %>% filter(!is.na(scan))

DIR <- "/media/mori/Stuff/LCMS/WEB2015-II/MzXML/"

spec <- vector("list", length = 0)
for(fil in unique(what$file)){
  MS <- openMSfile(paste(DIR,fil, sep=""))
  spec <- append(spec, {p <- peaks(MS, subset(what, file == fil, scan) %>% unlist)
                        if(is.list(p)) p else list(p)
                        })
}

what <- data.table(what)
msp <- vector()
for(i in 1:length(spec)){
  name <- what[i,name]
  
  comment <- list(file = what[i,file],
                  method = what[i,method],
                  eV = what[i,eV],
                  Parent = what[i,precursor],
                  "ret. time (min)" = what[i,rt],
                  "scan" = what[i,scan]
  )  
  
  mode <- what[i,file] %>% str_extract(., "[np]MS2") %>% str_extract(., "[np]")
  ms2 <- list("Coumpound type" = "M", 
              "Instrument type" = "QTOF",
              "Spectrum type" = paste("ms2:",what[i,method],sep=""),
              "Precursor type" = ifelse(mode == "p", yes = "[M+H]+", "[M-H]-"),
              "Precursor m/z" = what[i,precursor],
              "Collision energy" = paste(what[i,eV], "eV"),
              "Sample inlet" = "LC",
              "Ionization" = "ESI",
              "Ion mode" = mode)
  
  meta <- list(Formula = what[i,formula], 
               MW = what[i,MW])
  rm(mode)
  msp <- append(msp, spec2MSP(spec[[i]], name = name, meta = meta, comment = comment, ms2=ms2))
}

msp <- str_c(msp, collapse = "\n")
#cat(msp, file="~/.wine//drive_c//NISTDEMO//MSSEARCH/WEB346_flavonoid_standards.msp")
rm(list=ls())