library(magrittr)
library(dplyr)
library(nwc.chemsupport)



substances <- read.table("~/IPB/thesis/supplementary/WEB350/substances.csv", stringsAsFactors = F, sep = ",", header=T)

substances %<>% group_by(name) %>% mutate(formula=toupper(formula), MW=mol.mass(formula)$mass)


data <- read.csv("~/IPB/thesis/supplementary/fragment.table.csv")
frags <- data$fragment

data <- expand.grid(fragment=frags, substance=substances$name)
data <- merge(data, substances, by.x="substance", by.y="name") %>% 
  group_by(substance, fragment) %>% 
  mutate(mz = frag.mass(fragment, MW))



##########################################################
##########################################################
############annotate spectra
##########################################################
##########################################################

load("~/IPB/thesis/supplementary/R scripts/WEB350/spectra_WEB350.Rda")
load("~/IPB/thesis/supplementary/R scripts/WEB350/peaks_WEB350.Rda")

kaempferol_cid<-spec$scan1
kaempferol_hcd1<-spec$scan2

tmp <- data %>% filter(substance=="kaempferol")

a<-massmatch(tmp$mz, kaempferol_cid, fragnames=tmp$fragment, digits = 0)
a %<>% mutate(int = int/max(int)*100)
lines(x=a$mz, y=a$int, col="red", type = "h")
