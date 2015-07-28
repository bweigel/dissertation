library(ChemoSpec)
library(ggplot2)
library(plyr)
library(signal)

#setwd("/media/IPB/IPB_Y/Experimente/WEB277/dx")
setwd("~/Dokumente/R Projects/Experimente/WEB277/dx")

CD2Ell <- function(mdeg, MW, resis, PathLen, conc) {
  Ell <- (mdeg * (MW/(resis-1)))/(PathLen * conc)
  return(Ell)
}

CD<-files2SpectraObject(gr.crit=c("SOMT"), gr.cols="auto", freq.unit="nm", int.unit="mdeg", format="dx", 
                        out.file="CDData.csv")

DATA <- data.frame(mdeg=matrix(t(CD$data)), nm=CD$freq, name=rep(CD$names, each=length(CD$freq)))
CDsmooth <- sgolayfilt(DATA$mdeg, n=15, p=3)
DATA<-cbind(DATA, CDsmooth)

DATA <- ddply(DATA, .(nm, mdeg, name, CDsmooth), function(X){
  c(Ell = CD2Ell(X$CDsmooth, 42589, 378, 1, 0.19),
    dE = round((CD2Ell(X$CDsmooth, 42589, 378, 1, 0.19) / 3.298), 2),
    dE2 = round((CD2Ell(X$CDsmooth, 42589, 378, 1, 0.19) / 3298), 2))
})

### for K2D3
K2D3 <- subset(DATA, nm >= 190 & nm <= 240)
K2D3 <- K2D3[round(seq(from=1, to=nrow(K2D3), by=nrow(K2D3)/51), 1),]

write.table(x=K2D3, file="K2D3.csv")


PL <- vector("list")

PL[[1]]<-ggplot(DATA, aes(x=nm, y=Ell)) + geom_line() +
  ylab(expression(paste("[", theta, "]", " deg cm² ", plain(dmol)^{-1})))+
  xlab("Wavelength, nm")+
  labs(title="CD-Spectrum of SOMT in 10 mM KPi pH 8.0 at 4°C\n[measured in 1 mm quartz cuvette]")+
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(strip.background=element_rect(color="transparent", fill = "#E6E6E6", size = 0), 
        strip.text.x = element_text(size = 14, hjust = 0.5, vjust = 0.5),
        legend.key= element_rect(color="transparent"),
        plot.title = element_text(vjust=2, size=16),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=14))

ggsave(filename="CD_SOMT_1mm.png", PL[[1]])
PL[[1]]
