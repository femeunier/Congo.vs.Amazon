rm(list = ls())

library(raster)
library(dplyr)
library(lubridate)
library(ggplot2)

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/list.txt hpc:/data/gent/vo/000/gvo00074/felicien/R/
# curl 'https://e4ftl01.cr.usgs.gov/MOLT/MOD13C2.062/'|grep -Po '(?<=>)[^<]+' > list.txt
lines <- readLines('./list.txt')
g<-seq(20,length(lines),3)
folders <- sapply(1:length(g), function(x){lines[g[x]]})

# dest <- "/data/gent/vo/000/gvo00074/felicien/R/MOD/"
dest <- "~/Downloads/MOD/"

N = length(folders)
N = 12

for (ifolder in seq(1,N,1)){
  print(ifolder/length(folders))
  cfolder <- paste0("https://e4ftl01.cr.usgs.gov/MOLT/MOD13C2.061/",
                    folders[ifolder])
  system2("curl",
          paste0("'",cfolder,"'|grep -Po '(?<=>)[^<]+' > ./test.out")
  )
  cfile <- paste0(cfolder,
                  readLines('./test.out')[26])

  dest.file <- file.path(dest,basename(cfile))

  if (file.exists(dest.file)){
    next()
  }

  system2("wget",
          paste(cfile,
                "-P",dest))

}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Download.MODIS2.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
