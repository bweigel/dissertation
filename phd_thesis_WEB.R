
## ----glossary_definitions, child='usr/subdocuments/options/open_science_glossary.Rnw', eval=T----




## ----chunk_global_r_options, child='usr/subdocuments/options/ost_global_r_options.Rnw', eval=T----


## ----ost_global_r_options, tidy=FALSE, highlight=FALSE, include=FALSE, cache=FALSE, eval=TRUE----
  ## Load packages 
  suppressPackageStartupMessages(library(ggplot2)) 
  suppressPackageStartupMessages(library(plyr))
  suppressPackageStartupMessages(library(rgbif))

  ## Knitr 

  # messages 
  opts_chunk$set(warning=FALSE)
  # code evaluation 
  opts_chunk$set(eval=TRUE)
  # code decoration
  opts_chunk$set(tidy=TRUE, prompt=TRUE, comment='##', highlight=TRUE, size='normalsize')
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





## ----subdoc_content_header, child='usr/subdocuments/chapters/x_header.Rnw', eval=T----




## ----subdoc_content_abstract, child='usr/subdocuments/chapters/a_abstract_section.Rnw', eval=T----




## ----subdoc_content_introduction, child='usr/subdocuments/chapters/b_introduction_section.Rnw', eval=T----




## ----subdoc_content_material_methods, child='usr/subdocuments/chapters/c_material_methods_section.Rnw', eval=T----


## ----test, echo=TRUE, tidy=TRUE------------------------------------------




## ----subdoc_content_results, child='usr/subdocuments/chapters/d_pfomt_section.Rnw', eval=T----




## ----subdoc_content_results, child='usr/subdocuments/chapters/e_noncatechol_section.Rnw', eval=T----




## ----subdoc_content_results, child='usr/subdocuments/chapters/f_mtscreening_section.Rnw', eval=T----




## ----subdoc_content_results, child='usr/subdocuments/chapters/g_des_section.Rnw', eval=T----




## ----subdoc_content_acknowledgement, child='usr/subdocuments/chapters/x_acknowledgements_section.Rnw', eval=T----




## ----subdoc_content_appendix, child='usr/subdocuments/chapters/x_appendix_section.Rnw', eval=T----


## ----test_plot_one_appendix, fig.width=3.415, fig.height=2.5, echo=FALSE----
   ggplot(movies, aes(x=rating)) +
   geom_density() +
   theme_ost() 




## ----subdoc_content_introduction, child='usr/subdocuments/chapters/x_affidavit_section.Rnw', eval=T----




## ----options_bibliography, child='usr/subdocuments/chapters/x_bibliography_section.Rnw', eval=T----




## ----body_mainmatter_chapter_one, child='usr/subdocuments/chapters/x_glossaries_section.Rnw', eval=T----




