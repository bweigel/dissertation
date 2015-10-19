## ----glossary_definitions, child='usr/subdocuments/options/open_science_glossary.Rnw', eval=T----



## ----chunk_global_r_options, child='usr/subdocuments/options/ost_global_r_options.Rnw', eval=T----

## ----ost_global_r_options, tidy=FALSE, highlight=FALSE, include=FALSE, cache=FALSE, eval=TRUE----
  ## Load packages 
  suppressPackageStartupMessages(library(ggplot2)) 
  suppressPackageStartupMessages(library(magrittr))
  suppressPackageStartupMessages(library(plyr))
  suppressPackageStartupMessages(library(dplyr)) 
  suppressPackageStartupMessages(library(rgbif))
  suppressPackageStartupMessages(library(gridExtra))
  suppressPackageStartupMessages(library(nwc.itc)) 

  ## Knitr 

  # messages 
  opts_chunk$set(warning=FALSE, error=TRUE, message=FALSE)
  # code evaluation 
  opts_chunk$set(eval=TRUE)
  # code decoration
  opts_chunk$set(tidy=TRUE, prompt=TRUE, comment='##', highlight=TRUE, size='scriptsize')
  # cache options
  opts_chunk$set(cache.path='usr/cache/')  
  # Caching on by default
  opts_chunk$set(cache=T)
  # plot options 
  if("tikzDevice" %in% rownames(installed.packages()) == TRUE) {
	 opts_chunk$set(fig.path='usr/graphics/dynamic/', dev='tikz', fig.keep='high', fig.show='hold')  
  } else {
	 opts_chunk$set(fig.path='usr/graphics/dynamic/', dev='pdf', fig.keep='high', fig.show='hold')  
  }

  # tune details of graphics appearance 
  knit_hooks$set(par=function(before, options, envir){
    if (before && options$fig.show!='none') par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
    }
  )  

# remove kframe environment, cause it causes compilation errors, when an R error is issued
knit_hooks$set(
    error = function(x, options) {
      x = gsub(pattern = '#', '\\\\#', x)
      x = sprintf('\n\n{\\ttfamily\\noindent\\scriptsize\\color{errorcolor}%s}',  x)
      paste('\\end{kframe}', x, '\n\\begin{kframe}', sep = '')
      })

# crop pdfs, if needed
knit_hooks$set(crop = hook_pdfcrop)
                                        
                                        #knit_hooks$set(chunk = function(x, options) {gsub('\\\\(begin|end)\\{kframe\\}', '', x)})
#knit_hooks$set(error = function(x, options) paste('\\scriptsize',x,sep=''))

  # escape latex if in line code chunk
  hook_inline = knit_hooks$get('inline')
  knit_hooks$set(inline = function(x) {
    if (is.character(x)) x = knitr:::escape_latex(x)
    hook_inline(x)
  }) 

  # pdf.options(family = "Times", width=15)
  # tikz options
  options(replace.assign=TRUE, width=60)
  options(tikzSanitizeCharacters = c('%','$','}','{','^', '_'))
  options(tikzReplacementCharacters = c('\\%','\\$','\\}','\\{','\\^{}', '\\textunderscore')) 

  ## ggplot2  

  # Plot theme for ggplot2 (use with "+ theme_ost()") 
  theme_ost <- function(base_size = 10, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
      theme(
	axis.text = element_text(size = rel(0.8)),
	axis.ticks = element_line(colour = "black"),
	legend.key = element_rect(colour = "grey80"),
	panel.background = element_rect(fill = "white", colour = NA),
	panel.border = element_rect(fill = NA, colour = "grey50"),
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),  
	strip.background = element_rect(fill = "grey80", colour = "grey50"),
	strip.background = element_rect(fill = "grey80", colour = "grey50")
      )
  }

  theme_ost_distmap <- function(base_size = 10, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
      theme(
	axis.text = element_blank(),
	axis.ticks = element_blank(),
	legend.title = element_text(hjust = 20),
	legend.key = element_rect(colour = "grey80"),  
	panel.background = element_rect(fill = "white", colour = NA),
	panel.border = element_rect(fill = NA, colour = "grey50"),
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),  
	panel.border = element_blank(), 
	legend.key = element_blank(),
	strip.background = element_rect(fill = "grey80", colour = "grey50"),
	strip.background = element_rect(fill = "grey80", colour = "grey50")
      )
  }

  ## Helpers functions

  ost_load_or_install <- function(library=NULL){
    # todo: Implement a check if package is installed otherwise install it
  }

  ost_distmap <- function(species = NULL, kingdom = NULL){
    keys <- sapply(species, function(x) name_backbone(name=x, kingdom = kingdom)$speciesKey, USE.NAMES=FALSE)
    dat <- occ_search(taxonKey=keys, limit=100, return='data', georeferenced=TRUE)
    datdf <- ldply(dat)
    gbifmap(datdf)
  } 



## ----subdoc_content_scripts, eval=T, echo=F, cache=F---------------------
read_chunk("usr/statistics/rproject/test.R")
read_chunk("usr/statistics/rproject/litSAM.R")
read_chunk("usr/statistics/rproject/struct.compare.R")
read_chunk("usr/statistics/rproject/itc.R")

## ----subdoc_content_header, child='usr/subdocuments/chapters/x_header.Rnw', eval=T----



## ----subdoc_content_abstract, child='usr/subdocuments/chapters/a_abstract_section.Rnw', eval=T----



## ----subdoc_content_introduction, child='usr/subdocuments/chapters/b_introduction_section.Rnw', eval=T----



## ----subdoc_content_material_methods, child='usr/subdocuments/chapters/c_material_methods_section.Rnw', eval=T----

## ----test, echo=F, tidy=TRUE, fig.height=3.25, fig.width=5.25, dev='tikz'----
load("../../statistics/data/WEB338.fa-ca.calibration.Rda")
ggplot(cali.curve, aes(x=FA, y=value, shape=as.factor(pH), color=as.factor(DES), group=buffer.no)) + 
 labs(x="ferulic acid/caffeic acid (0.4 mM)", y="$\\mathrm{Abs}^{595}$") +
 geom_point() +
 theme_ost()

# torm <- ls()
# rm(list = torm[!grepl(pattern = "(ost)", torm)])


## ----subdoc_content_results, child='usr/subdocuments/chapters/d_pfomt_section.Rnw', eval=T----

## ----struct.compare,  echo=F, tidy=TRUE, fig.height=2, fig.width=5.6, out.width='\\textwidth', dev='tikz', error=T----
load("../../statistics/data/structurecompare.Rda")

geom_arrow <- function(label, start=1, end=4, y=2, width=1, a=NULL, b=NULL, fill){
  if(is.null(b)) b <- 0.45*width
  if(is.null(a)) a <- 2.4
  enda <- end-a
  start[start>enda] <- enda[start>enda]
  polyx <- c(start, start, enda, enda, end, enda, enda)
  polyy <- rep(c(y-width/2, y+width/2, y+width/2, y+width/2+b, y, y-width/2-b, y-width/2), each=length(start))
  polydf <- data.frame(label = rep(label, 7), x = polyx, y = polyy)
  
  return(geom_polygon(data = polydf, aes(x=x, y=y, group=label), fill=fill))
}


ss$helix <- grepl("a", ss$ss.elem)
cols <- c("A"="palegreen", "B"="palegreen", "C"="plum2", "orange"="orange", "cornflowerblue"="cornflowerblue")

top <- 4.75
width <- top/22.5
bottom <- -0.25
ggplot() +
  theme_ost()+
  #theme(axis.text.y = element_blank(),
  #      axis.ticks.y = element_blank()) +
  labs(x="residue no.", y="residue distance [\\AA]") +
  ## binding site annotation
  geom_rect(data=anno, aes(xmax = xstop+0.5, xmin = xstart-0.5, ymax = 4.5, ymin = ymin, fill=region), alpha=0.8) +
  scale_fill_manual(values = cols) + 
  ## deviance annotation
  geom_rect(data=anno.2, aes(xmax = xstop+0.5, xmin = xstart-0.5, ymax = bottom+width/4, ymin = bottom-width/4, fill=col), alpha=0.8) +
  ## data
  geom_step(data=tmp, aes(x=as.numeric(q.resnum), y=distance.A), col="black") +
  ## secondary structure annotation
  geom_line(data=ss, aes(x=c(min(ss$start), max(ss$start)), y=top)) +
  geom_arrow(ss$ss.elem[!ss$helix], ss$start[!ss$helix], ss$stop[!ss$helix], y = top, width = width, fill="black") +
  geom_rect(data=ss[ss$helix,], aes(xmin=start, xmax=stop, ymin=top-width/2, ymax=top+width/2), fill="black") +
  geom_text(data=ss, aes(x=start+(stop-start)/2, y=top+(top*0.07), label=latexannotation), size=2.5)+
  theme(legend.position="none")

## ----itc.sah,  echo=F, tidy=TRUE, fig.height=4.5, fig.width=8, out.width='\\textwidth', dev='tikz', error=T----
#library(nwc.itc)

load("../../statistics/data/WEB338.itc.Rda")
cols <- c("black", "red")

names <- c("PFOMT vs. SAH", "PFOMT/caffeic acid/\\cf{Mg^2+} vs SAH/\\cf{Mg^2+}")
itc.compare(itcs[c(5, 16)], noms = names, ylim=c(-0.85, 0.05), lwd=2, cols = cols, main=F, cex.axis=1)

## ----itc.ca,  echo=F, tidy=TRUE, fig.height=4.5, fig.width=8, dev='tikz', error=T----
load("../../statistics/data/WEB338.itc.Rda")
cols <- c("black", "red")

names <- c("PFOMT/\\cf{Mg^2+} vs caffeic acid/\\cf{Mg^2+}", "PFOMT/SAH/\\cf{Mg^2+} vs caffeic acid/\\cf{Mg^2+}")
itc.compare(itcs[c(18,21)], ylim=c(-0.6, 0.05), xlim=c(0,1000),lwd=2, noms = names, cols = cols, main=F, cex.axis=1)


## ----subdoc_content_results, child='usr/subdocuments/chapters/f_mtscreening_section.Rnw', eval=T----



## ----subdoc_content_results, child='usr/subdocuments/chapters/e_noncatechol_section.Rnw', eval=T----



## ----subdoc_content_acknowledgement, child='usr/subdocuments/chapters/x_acknowledgements_section.Rnw', eval=T----



## ----subdoc_content_appendix, child='usr/subdocuments/chapters/x_appendix_section.Rnw', eval=T----

## ----litSAM, eval=T, echo=F, tidy=TRUE, fig.height=5, fig.width=5, out.width='0.7\\textwidth', dev='pdf', error=T, crop=T----
load("../../statistics/data/litSAMder.Rda")

#head(daf)
a <- daf %>% group_by(substrate, target.mol, name, type) %>%
  summarise(T = sum(T), C = sum(C)) %>%
  ungroup %>% 
  ggplot(data=.) + 
  geom_bar(stat="identity", aes(x = substrate, fill=target.mol, y=T)) +
  scale_y_continuous(expand=c(0.2, 0))  +
  theme(panel.background = element_blank(), 
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none") +
  geom_rect(data=nodes, aes(xmin=gpmin, xmax = gpmax, ymin=0, ymax=size), fill="black", color="white", alpha=0.2) +
  geom_bar(stat="identity", aes(x = substrate, fill=target.mol, y=T))  +
  geom_crossbar(data=tmp, aes(x=substrate, ymin=C, ymax=C, y=C), width=0.8, color="black") +
  geom_text(data=nodes, aes(x = mid, y=25, label=type, angle=angles)) 
a <- a + coord_polar() 

# b <- daf %>% group_by(substrate, target.atom, name, type) %>%
#   summarise(T = sum(T), C = sum(C)) %>%
#   ungroup %>% 
#   ggplot(data=.) + 
#   geom_bar(stat="identity", aes(x = substrate, fill=target.atom, y=T)) +
#   scale_y_continuous(expand=c(0.2, 0))  +
#   theme(panel.background = element_blank(), 
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         legend.position = "none") +
#   geom_rect(data=nodes, aes(xmin=gpmin, xmax = gpmax, ymin=0, ymax=size), fill="black", color="white", alpha=0.2) +
#   geom_bar(stat="identity", aes(x = substrate, fill=target.atom, y=T))  +
#   geom_crossbar(data=tmp, aes(x=substrate, ymin=C, ymax=C, y=C), width=0.8, color="black") +
#   geom_text(data=nodes, aes(x = mid, y=25, label=type, angle=angles)) 
# b <- b + coord_polar() 
a
#grid.arrange(a,b, nrow=1)

# torm <- ls()
# rm(list = torm[!grepl(pattern = "(ost)", torm)])

## ----struct.compare.appendix,  echo=F, tidy=TRUE, fig.height=2, fig.width=5.6, out.width='\\textwidth', dev='tikz', error=T----
load("../../statistics/data/structurecompare.Rda")

ss$helix <- grepl("a", ss$ss.elem)
cols <- c("A"="palegreen", "B"="palegreen", "C"="plum2", "orange"="orange", "cornflowerblue"="cornflowerblue")

geom_arrow <- function(label, start=1, end=4, y=2, width=1, a=NULL, b=NULL, fill){
  if(is.null(b)) b <- 0.45*width
  if(is.null(a)) a <- 2.4
  enda <- end-a
  start[start>enda] <- enda[start>enda]
  polyx <- c(start, start, enda, enda, end, enda, enda)
  polyy <- rep(c(y-width/2, y+width/2, y+width/2, y+width/2+b, y, y-width/2-b, y-width/2), each=length(start))
  polydf <- data.frame(label = rep(label, 7), x = polyx, y = polyy)
  
  return(geom_polygon(data = polydf, aes(x=x, y=y, group=label), fill=fill))
}


top <- 285
width <- top/22.5
bottom <- -7
ggplot() +
  theme_ost()+
  # theme(axis.text.y = element_blank(),
  #      axis.ticks.y = element_blank())+
  labs(x="residue no.", y="root squared difference") +
  ## binding site annotation
  geom_rect(data=anno, aes(xmax = xstop+0.5, xmin = xstart-0.5, ymax = ymax, ymin = ymin, fill=region), alpha=0.8) +
  scale_fill_manual(values = cols) + 
  ## deviance annotation
  geom_rect(data=anno.2, aes(xmax = xstop+0.5, xmin = xstart-0.5, ymax = bottom+width/4, ymin = bottom-width/4, fill=col), alpha=0.8) +
  ## data
  geom_step(data=struc.diff, aes(x=number, y=phi), col="black") +
  geom_step(data=struc.diff, aes(x=number, y=psi), col="red") +
  ## secondary structure annotation
  geom_line(data=ss, aes(x=c(min(ss$start), max(ss$start)), y=top)) +
  geom_arrow(ss$ss.elem[!ss$helix], ss$start[!ss$helix], ss$stop[!ss$helix], y = top, width = width, fill="black") +
  geom_rect(data=ss[ss$helix,], aes(xmin=start, xmax=stop, ymin=top-width/2, ymax=top+width/2), fill="black") +
  geom_text(data=ss, aes(x=start+(stop-start)/2, y=top+(top*0.07), label=latexannotation), size=2.5)+
  theme(legend.position="none")


## ----subdoc_content_introduction, child='usr/subdocuments/chapters/x_affidavit_section.Rnw', eval=T----



## ----options_bibliography, child='usr/subdocuments/chapters/x_bibliography_section.Rnw', eval=T----



## ----body_mainmatter_chapter_one, child='usr/subdocuments/chapters/x_glossaries_section.Rnw', eval=T----



