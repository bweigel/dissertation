## @knitr test
load("../../statistics/data/WEB338.fa-ca.calibration.Rda")
ggplot(cali.curve, aes(x=FA, y=value, shape=as.factor(pH), color=as.factor(DES), group=buffer.no)) + 
 labs(x="ferulic acid/caffeic acid (0.4 mM)", y="$\\mathrm{Abs}^{595}$") +
 geom_point() +
 theme_ost()

