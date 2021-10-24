#FROM debian:wheezy
FROM ubuntu:trusty


RUN apt-get update && apt-get install -y --force-yes texlive-full 
RUN apt-get autoclean

RUN echo "deb http://cran.uni-muenster.de/bin/linux/debian wheezy-cran3/" >> /etc/apt/sources.list
RUN apt-get update 
RUN apt-get  install -y --force-yes biber latexmk make r-base r-base-dev
RUN apt-get autoclean

COPY knit /usr/bin/knit
COPY install_packages.sh /tmp/.
RUN chmod +x /tmp/install_packages.sh 
RUN /tmp/install_packages.sh

COPY nwc.itc_0.0.1.tar.gz /tmp/nwc.itc.tar.gz
RUN Rscript -e "install.packages('/tmp/nwc.itc.tar.gz', repos=NULL, type='source')"
RUN mkdir -p /workd/thesis

COPY xindy-2.2-rc2-linux.tar.gz /tmp/.
RUN tar xzf /tmp/xindy-2.2-rc2-linux.tar.gz -C  /opt/

COPY xindy-2.3.tar.gz /tmp/.
RUN tar xzf /tmp/xindy-2.3.tar.gz -C  /opt/

COPY biber /usr/local/bin/.
#ENV PATH ${PATH}:/opt/xindy-2.2/bin
#COPY xindy_modules/modules/lang/english/utf8* /opt/xindy-2.2/modules/lang/english/
RUN apt-get install -y git less 

CMD ["/bin/bash"]
