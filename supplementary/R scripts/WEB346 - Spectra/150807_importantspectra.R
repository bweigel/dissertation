library(mzR)
library(data.table)
library(magrittr)
library(dplyr)
source("~/Development/R/WEB346 - Spectra/spreadspec.R")

DIR <- "/media/mori/Stuff/LCMS/WEB2015-II/MzXML/"

load("peaks.Rda")
data <- rbindlist(data)
scans <- rbind(expand.grid(file = "WEB346_B_16_pMS2_303,317,331.mzXML", scan=c(3362,3363,3364,3413,3414)),
               expand.grid(file = "WEB346_B_17_pMS2_319,333,347,361.mzXML", scan=c(3090,3091,3092,3309,3310,3479,3472)),
               expand.grid(file = "WEB346_C_11_nMS2_163,177.mzXML", scan=c(3197,3678)),
               expand.grid(file = "WEB346_C_11_pMS2_179,165.mzXML", scan=c(3010,3399,3400))               ,
               expand.grid(file = "WEB346_D_10_pMS2_179,165.mzXML", scan=c(2886,3308)) ,
               expand.grid(file = "WEB346_D_17_pMS2_319,333,347,361.mzXML", scan=c(3090,3091,3092,3309,3302,3535,3536)),
               expand.grid(file = "WEB346_D_6_pMS2,HCD_301,287.mzXML", scan=c(3470,3467,3468)),
               expand.grid(file = "WEB346_E_14_pMS2_209,195.mzXML", scan=c(3102,3363,3571,3364,3572)),
               expand.grid(file = "WEB346_E_15_pMS2_287,301.mzXML", scan=c(3482,3251,3539,3252,3536)),
               expand.grid(file = "WEB346_E_16_pMS2_303,317,331.mzXML", scan=c(3362,2895,3363,3364,3780)),
               expand.grid(file = "WEB346_E_17_pMS2_319,333,347,361.mzXML", scan=c(3090,3091,3092,3301,3294,3463,3464)),
               expand.grid(file = "WEB346_E_5_pMS2,HCD_285,271.mzXML", scan=c(3142,3438,3139,3140,3760,3759)),
               expand.grid(file = "WEB346_E_7_pMS2,HCD_315,301.mzXML", scan=c(3466,3187,3631,3188,3632)),
               expand.grid(file = "WEB346_G_11_nMS2_163,177.mzXML", scan=c(3191,3600,3669)),
               expand.grid(file = "WEB346_G_11_pMS2_179,165.mzXML", scan=c(3014,3407,3471,3404,3468)),
               expand.grid(file = "WEB346_F_15_pMS2_287,301.mzXML", scan=c(3482,3534,3535,3536)),
               expand.grid(file = "WEB346_F_16_pMS2_303,317,331.mzXML", scan=c(3362,3363,3364,3413,3414)),
               expand.grid(file = "WEB346_G_13_nMS2_193,207.mzXML", scan=c(2960,3032,3816)),
               expand.grid(file = "WEB346_G_13_pMS2_209,195.mzXML", scan=c(3110,3038,3363,3575,3364,3572)),
               expand.grid(file = "WEB346_G_14_pMS2_209,195.mzXML", scan=c(3102,3363,3571,3364,3568)),
               expand.grid(file = "WEB346_H_12_nMS2_179,193.mzXML", scan=c(1895,2474,2988,3054,3237)),
               expand.grid(file = "WEB346_H_12_pMS2_195,181.mzXML", scan=c(3263,3343,3264,3340))
               
               
               #expand.grid(file = "", scan=c())
               
               
               
)
tresh <- 5

#scans <- expand.grid(file = "WEB346_B_17_pMS2_319,333,347,361.mzXML", scan=c(3090,3091,3092,3309,3310,3479,3472))

spec <- vector("list")
for(fil in unique(scans$file)){
  MS <- openMSfile(paste(DIR,fil, sep=""))
  spec <- peaks(MS, subset(scans, file == fil, scan) %>% unlist)
  
  #spec <- spreadSpec(spec, cuts = 0.001, cutoff = 1000)
  
  x1 <- subset(scans, file == fil, scan) %>% unlist
  scan <- data %>% filter(file == fil, scan %in% x1)
  rm(x1)
  
  
  pdf(paste("spectra_",fil,".pdf",sep=""), paper = "a4", width = 10, height = 14.1)
  layout(matrix(c(1:6), 6,1))
  par(mar=c(2, 4, 2.5, 2), mgp=c(1.2,0.5,0))
  #x <- spec[1,]
  
  for(i in 1:length(spec)){
    x <- spec[[i]][,1]
    y <- spec[[i]][,2]
    y[is.na(y)] <- 0
    y <- y / max(y)*100
    ind <- y != 0
    plot(x[ind], y[ind], type="h", xlab="m/z", ylab="rel. intensity", 
         main=NA)
    mtext(paste("MS.2 spectrum: ", scan$full.path[i], "\n", fil, ", time = ", round(scan$rt[i]/60,2), " min, scan ", scan$scan[i], " || ",scan$method[i], sep = ""), 
          side=3, line=0.5, cex=0.6, adj=0, srt=90)
    AN <- which(y >= tresh) 
    text(x[AN], y[AN], labels = round(x[AN],3), cex=0.6, adj=c(0.5,0), srt=0, col="blue")
  }
  
  dev.off()
}


## compare ring-opened?? flavonoids (chalcogens) of apigenin, diosmetin, chrysoeriol


comparespec <- function(scans, filename, tresh=5, rnd=3, range=c(80,310), ...){
  
  #tresh <- 5
  
  pdf(filename, paper = "a4", width = 10, height = 14.1)
  layout(matrix(c(1:6), 6,1))
  par(mar=c(2, 4, 2.5, 2), mgp=c(1.2,0.5,0))
  
  spec <- vector("list")
  for(fil in unique(scans$file)){
    MS <- openMSfile(paste(DIR,fil, sep=""))
    #spec <- list(peaks(MS, subset(scans, file == fil, scan) %>% unlist))
    
    spec<-{p <- peaks(MS, subset(scans, file == fil, scan) %>% unlist)
           if(is.list(p)) p else list(p)
    }
    
    #print(str(spec))
    x1 <- subset(scans, file == fil, scan) %>% unlist
    scan <- data %>% filter(file == fil, scan %in% x1)
    rm(x1)
    
    # if(!is.list(spec))spec<-as.list(spec)
    for(i in 1:length(spec)){
      x <- spec[[i]][,1]
      y <- spec[[i]][,2]
      y[is.na(y)] <- 0
      y <- y / max(y)*100
      ind <- y != 0
      plot(x[ind], y[ind], type="h", xlab="m/z", ylab="rel. intensity", 
           main=NA, xlim=range)
      
      mtext(paste("MS.2 spectrum: ", scan$full.path[i], "\n", fil, ", time = ", round(scan$rt[i]/60,2), " min, scan ", scan$scan[i], " || ",scan$method[i], sep = ""), 
            side=3, line=0.5, cex=0.6, adj=0, srt=90)
      
      AN <- which(y >= tresh) 
      if(length(list(...)) == 0){
        text(x[AN], y[AN], labels = round(x[AN],rnd), cex=0.6, adj=c(0.5,0), srt=0, col="blue")
      } else {
        text(x[AN], y[AN], labels = round(x[AN],rnd), cex=0.6, srt=0, col="blue", ...)
      }
    } 
  }
  dev.off()
}


scans <- data.frame(file = c("WEB346_E_5_pMS2,HCD_285,271.mzXML", "WEB346_E_7_pMS2,HCD_315,301.mzXML", "WEB346_E_8_pMS2,HCD_315,301.mzXML"),
                    scan = c(3139,3187,3171))
comparespec(scans, file="Compare_apigenin,diosmetin,luteolin-chalcogens.pdf")

scans <- data.frame(file = c("WEB346_E_5_pMS2,HCD_285,271.mzXML", "WEB346_E_7_pMS2,HCD_315,301.mzXML", "WEB346_E_8_pMS2,HCD_315,301.mzXML"),
                    scan = c(3760,3632,3636))
comparespec(scans, file="Compare_apigenin,diosmetin,luteolin_products.pdf")


scans <- data.frame(file = c("WEB346_S_1_pMS2.mzXML","WEB346_S_1_pMS2.mzXML", "WEB346_S_2_pMS2.mzXML", "WEB346_S_3_pMS2_303,317.mzXML", "WEB346_S_4_pMS2_303,317.mzXML", "WEB346_E_3_pMS2,HCD_317,303.mzXML"),
                    scan = c(3474, 3803, 3314, 3518, 3494, 3643))
comparespec(scans, file="Compare_flavanes_CID45.pdf", tresh=1, range=c(80,320))

scans <- data.frame(file = c("WEB346_S_1_pMS2.mzXML", "WEB346_S_2_pMS2.mzXML","WEB346_E_3_pMS2,HCD_317,303.mzXML"),
                    scan = c(3804, 3516, 3644))
comparespec(scans, file="Compare_flavanes_HCD75_100.pdf", tresh=2.5, range=c(50,320))

##########################################

scans <- data.frame(file = c("WEB346_S_5_pMS2.mzXML", "WEB346_S_6_pMS2.mzXML", "WEB346_S_7_pMS2.mzXML", "WEB346_S_8_pMS2.mzXML", "WEB346_E_7_pMS2,HCD_315,301.mzXML"),
                    scan = c(3454, 3314, 3486, 3482, 3631))
comparespec(scans, file="Compare_flavones_CID45.pdf", tresh = 1, range=c(80,320))

scans <- data.frame(file = c("WEB346_S_5_pMS2.mzXML","WEB346_E_5_pMS2,HCD_285,271.mzXML", "WEB346_S_6_pMS2.mzXML", "WEB346_D_6_pMS2,HCD_301,287.mzXML", "WEB346_E_7_pMS2,HCD_315,301.mzXML"),
                    scan = c(3780, 3760, 3484, 3468, 3632))
comparespec(scans, file="Compare_flavones_HCD75_100.pdf", tresh = 2.5, range=c(50,320), rnd=4, srt=90, adj=0)

##########################################
scans <- data.frame(file = c("WEB346_S_15_pMS2_287,301.mzXML", "WEB346_S_16_pMS2.mzXML", "WEB346_S_17_pMS2.mzXML", "WEB346_E_16_pMS2_303,317,331.mzXML", "WEB346_B_16_pMS2_303,317,331.mzXML", "WEB346_B_17_pMS2_319,333,347,361.mzXML", "WEB346_B_17_pMS2_319,333,347,361.mzXML"),
                    scan = c(3482, 3182, 2930, 3363, 3413,3091, 3309))
comparespec(scans, file="Compare_flavonoles_CID45.pdf", tresh = 1, range=c(80,350))


scans <- data.frame(file = c("WEB346_E_15_pMS2_287,301.mzXML", "WEB346_E_16_pMS2_303,317,331.mzXML", 
                             "WEB346_B_16_pMS2_303,317,331.mzXML", "WEB346_B_16_pMS2_303,317,331.mzXML", 
                             "WEB346_E_17_pMS2_319,333,347,361.mzXML","WEB346_E_17_pMS2_319,333,347,361.mzXML",
                             "WEB346_B_17_pMS2_319,333,347,361.mzXML", "WEB346_B_17_pMS2_319,333,347,361.mzXML"),
                    scan = c(3536, 3364, 3364, 3414, 3092, 3294, 3092, 3310))
comparespec(scans, file="Compare_flavonoles_HCD75_100.pdf", tresh = 2.5, range=c(50,350))


##########################################
scans <- data.frame(file = c("WEB346_S_9_nMS2_163,177.mzXML","WEB346_S_12_nMS2_179,193.mzXML", 
                             "WEB346_S_12_nMS2_179,193.mzXML", "WEB346_S_13_nMS2_193,207.mzXML", 
                             "WEB346_S_13_nMS2_193,207.mzXML", "WEB346_S_14_nMS2_193,207.mzXML"),
                    scan = c(2852, 1880, 2483, 2960, 3029, 3041))
comparespec(scans, file="Compare_cinnamicacids_negCID30.pdf", tresh = 1, range=c(50,210))

scans <- data.frame(file = c("WEB346_H_9_pMS2_179,165.mzXML", "WEB346_G_11_pMS2_179,165.mzXML", 
                             "WEB346_G_13_pMS2_209,195.mzXML", "WEB346_G_14_pMS2_209,195.mzXML", "WEB346_G_14_pMS2_209,195.mzXML", 
                             "WEB346_G_14_pMS2_209,195.mzXML"),
                    scan = c(2706, 3014, 3038,3102, 3363, 3571))
comparespec(scans, file="Compare_cinnamicacids_posCID40.pdf", tresh = 1, range=c(50,210))
#rm(list = ls())


