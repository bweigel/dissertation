#!/bin/bash

MIRROR="http://cran.us.r-project.org"

apt-get install -y libgeos-dev libxml2-dev libcurl4-openssl-dev libssl-dev 

Rscript -e "install.packages('devtools', repos='${MIRROR}')"
Rscript -e 
Rscript -e "install.packages('knitr', repos='${MIRROR}')"
Rscript -e "install.packages('ggplot2', repos='${MIRROR}')"
Rscript -e "install.packages('magrittr', repos='${MIRROR}')"
Rscript -e "install.packages('plyr', repos='${MIRROR}')"
Rscript -e "install.packages('dplyr', repos='${MIRROR}')"
Rscript -e "install.packages('gridExtra', repos='${MIRROR}')"
Rscript -e "install.packages('rgbif', repos='${MIRROR}')"
Rscript -e "install.packages('baseline', repos='${MIRROR}')"
Rscript -e "install.packages('gaussquad', repos='${MIRROR}')"
Rscript -e "install.packages('tikzDevice', repos='${MIRROR}')"

