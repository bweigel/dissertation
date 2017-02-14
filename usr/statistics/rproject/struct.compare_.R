## @knitr struct.compare
URL <- "../../statistics/data/structurecompare.Rda"
load(URL)

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
  geom_line(data=data.frame(x = c(min(ss$start), max(ss$start)), y= top), aes(x, y)) +
  #geom_line(data=ss, aes(x=max(ss$start), y=top)) +
  geom_arrow(ss$ss.elem[!ss$helix], ss$start[!ss$helix], ss$stop[!ss$helix], y = top, width = width, fill="black") +
  geom_rect(data=ss[ss$helix,], aes(xmin=start, xmax=stop, ymin=top-width/2, ymax=top+width/2), fill="black") +
  geom_text(data=ss, aes(x=start+(stop-start)/2, y=top+(top*0.07), label=latexannotation), size=2.5)+
  theme(legend.position="none")

## @knitr struct.compare.appendix
URL <- "../../statistics/data/structurecompare.Rda"
load(URL)

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
  geom_line(data=data.frame(x = c(min(ss$start), max(ss$start)), y= top), aes(x, y)) +
  geom_arrow(ss$ss.elem[!ss$helix], ss$start[!ss$helix], ss$stop[!ss$helix], y = top, width = width, fill="black") +
  geom_rect(data=ss[ss$helix,], aes(xmin=start, xmax=stop, ymin=top-width/2, ymax=top+width/2), fill="black") +
  geom_text(data=ss, aes(x=start+(stop-start)/2, y=top+(top*0.07), label=latexannotation), size=2.5)+
  theme(legend.position="none")


