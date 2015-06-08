enzymes <- c("PRMT1_P_wt", "PRMT1_P_var", "M.TaqI_D_wt", "M.TaqI_D_var", "M.HhaI_D_wt", 
                       "M.HhaI_D_var", "M.BcnIB_D_wt", "M.BcnIB_D_var", "RebM_S_wt", "RebM_S_var", 
                       "RapM_S_wt", "RapM_S_var", "NovO_S_wt", "CouO_S_wt", "COMT_S_wt",
                       "TPMT_S_wt", "HsMLL_P_wt", "TRM1_R_wt", "PKMT_S_wt", "Dim5_P_wt")

citekeys <- c("Thomsen2013", "Dalhoff2006", "Singh2014", "Law2015", "Stecher2009", "Lee2010",
              "Wang2011", "Wang2011a", "Wang2014", "Tengg2012a", "Islam2011", "Willnow2012", 
              "Lukinavicius2013", "Peters2010", "Bothwell2012", "Zhang2006")

dat <- read.csv("litreview.csv", na.strings = c("NA", ""))
rm(daf)


library(tidyr)
library(magrittr)
library(ggplot2)

for(i in 1:29){
  if(!exists("daf")){
    daf <- cbind(dat[,1:2], 
                 paste(
                   grepl(
                    pattern = paste("(((^",as.character(i),"|\\D",as.character(i),")\\D)|(",as.character(i),"$))", sep=""), 
                    dat$substrates) %>% ifelse(1,0),
                   grepl(
                     pattern = paste("(((^",as.character(i),"|\\D",as.character(i),")\\D)|(",as.character(i),"$))", sep=""), 
                     dat$tested) %>% ifelse(1,0),
                   grepl(
                     pattern = paste("(((^",as.character(i),"|\\D",as.character(i),")\\D)|(",as.character(i),"$))", sep=""), 
                     dat$converted) %>% ifelse(1,0),
                   sep=""
                 ))
                 
  }else{
    daf <- cbind(daf, 
                 paste(
                  grepl(pattern = paste("(((^",as.character(i),"|\\D",as.character(i),")\\D)|(",as.character(i),"$))", sep=""), 
                       dat$substrates) %>% ifelse(1,0),
                  grepl(
                    pattern = paste("(((^",as.character(i),"|\\D",as.character(i),")\\D)|(",as.character(i),"$))", sep=""), 
                    dat$tested) %>% ifelse(1,0),
                  grepl(
                    pattern = paste("(((^",as.character(i),"|\\D",as.character(i),")\\D)|(",as.character(i),"$))", sep=""), 
                    dat$converted) %>% ifelse(1,0),
                  sep=""
                  ))
  }
}

tmp<-expand.grid(A="S", B=1:29) 
names(daf)[-c(1,2)] <- paste(tmp[,1], tmp[,2], sep=".")

library(stringr)
daf <- apply(daf, 2, FUN = function(x){str_replace(string = x, pattern = "[01]{2}1$", "C")}) %>% as.data.frame
daf <- apply(daf, 2, FUN = function(x){str_replace(string = x, pattern = "[01]{1}1[01]$", "T")}) %>% as.data.frame
daf <- apply(daf, 2, FUN = function(x){str_replace(string = x, pattern = "[01]{1}00$", "NT")}) %>% as.data.frame

daf %<>% separate(enzymes, into = c("enzyme", "wt"), sep = "_", extra="drop")
daf$wt <- ifelse(daf$wt == "wt", T, F)

daf %<>% gather(substrate, key, S.1:S.29) 

daf %>% dplyr::filter(key != "NT") %>%
ggplot(data=.) + geom_bar(aes(x=substrate, fill=key))

daf %>% dplyr::filter(key != "NT") %>%
ggplot(data=., aes())

