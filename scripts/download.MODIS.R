rm(list = ls())

library(gdalUtils)
library(raster)
library(dplyr)
library(lubridate)
library(ggplot2)

# curl 'https://e4ftl01.cr.usgs.gov/MOLT/MOD13C2.061/'|grep -Po '(?<=>)[^<]+' > list.txt
lines <- readLines('/home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/list.txt')
g<-seq(20,length(lines),3)
folders <- sapply(1:length(g), function(x){lines[g[x]]})

dest <- "/home/femeunier/Downloads/MOD/"
for (ifolder in seq(1,length(folders),1)){
  print(ifolder/length(folders))
  cfolder <- paste0("https://e4ftl01.cr.usgs.gov/MOLT/MOD13C2.061/",
                    folders[ifolder])
  system2("curl",
          paste0("'",cfolder,"'|grep -Po '(?<=>)[^<]+' > /home/femeunier/Documents/projects/Congo.vs.Amazon/test.out")
  )
  cfile <- paste0(cfolder,
                  readLines('/home/femeunier/Documents/projects/Congo.vs.Amazon/test.out')[26])

  dest.file <- file.path(dest,basename(cfile))

  if (file.exists(dest.file)){
    next
  }

  system2("wget",
          paste(cfile,
                "-P",dest))

}
