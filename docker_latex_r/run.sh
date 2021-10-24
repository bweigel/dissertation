#!/bin/bash

CONTAINER="mytex"

docker rm -f ${CONTAINER}
docker run -ti --name ${CONTAINER} \
-v /home/bweigel/Documents/thesis:/workd/thesis \
-v /home/bweigel/Downloads:/workd/downloads mylatex
