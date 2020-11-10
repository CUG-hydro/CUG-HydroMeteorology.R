# !/usr/bin/Rscript
# Dongdong Kong ----------------------------------------------------------------
# source('scripts/main_pkgs.R')

library(ncdf4)
library(stringr)
library(lubridate)
library(magrittr)
library(plyr)
library(abind)
library(magrittr)
library(PCICt)

library(sp)
library(maptools)
library(mapproj)

library(RColorBrewer)
library(ggplot2)

library(metR)
library(cowplot)
library(scales)

windowsFonts(A = windowsFont("Arial"))

worldmap <- map_data("world")
worldmap[which(worldmap$long < 0), 1] <- worldmap$long[which(worldmap$long < 0)] + 360
