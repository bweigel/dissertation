## @knitr litSAM
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
