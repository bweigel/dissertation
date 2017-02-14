## @knitr itc.ca
load("../usr/statistics/data/WEB338.itc.Rda")
cols <- c("black", "red")

names <- c("PFOMT/\\cf{Mg^2+} vs caffeic acid/\\cf{Mg^2+}", "PFOMT/SAH/\\cf{Mg^2+} vs caffeic acid/\\cf{Mg^2+}")
itc.compare(itcs[c(18,21)], ylim=c(-0.6, 0.05), xlim=c(0,1000),lwd=2, noms = names, cols = cols, main=F, cex.axis=1)


## @knitr itc.sah
#library(nwc.itc)

load("../usr/statistics/data/WEB338.itc.Rda")
cols <- c("black", "red")

names <- c("PFOMT vs. SAH", "PFOMT/caffeic acid/\\cf{Mg^2+} vs SAH/\\cf{Mg^2+}")
itc.compare(itcs[c(5, 16)], noms = names, ylim=c(-0.85, 0.05), lwd=2, cols = cols, main=F, cex.axis=1)

## @knitr itc.all
#library(nwc.itc)
# 
# load("../../statistics/data/WEB338.itc.Rda")
# cols <- c("black", "red", "blue")
# 
# names <- c("SAH", "SAM (65\\% L)", "D/L-SAE")
# par(mfrow=c(2,1))
# itc.compare(itcs[c(10,16,15)], ylim=c(-0.65, 0.05), xlim=c(0,1500), nom=names, lwd=1, cols = cols)
# 
# plot(itcs[[10]]$inj.program$program$MR, itcs[[10]]$integrate, type="p", pch=15, ylab="heat (kcal mol-1)", xlab="molar ratio", ylim=c(-18,1), main="Heat of injection", col="black")
# points(itcs[[16]]$inj.program$program$MR, itcs[[16]]$integrate, pch=15, col="red")
# points(itcs[[15]]$inj.program$program$MR, itcs[[15]]$integrate, pch=15, col="blue")
