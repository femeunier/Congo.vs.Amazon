rm(list = ls())

library(ncdf4)

# curl 'https://thredds-x.ipsl.fr/thredds/catalog/TROPOSIF-gridded/8-day/0.10-CF0.50-VZA40-SZA65/catalog.html'|grep -Po '(?<=>)[^<]+' > list.txt

dir <- "https://thredds-x.ipsl.fr/thredds/catalog/TROPOSIF-gridded/8-day/0.10-CF0.50-VZA40-SZA65/"

lines <- readLines('/home/femeunier/Documents/projects/Congo.vs.Amazon/list.txt')
g<-seq(23,length(lines),4)
files <- sapply(1:length(g), function(x){lines[g[x]]})
files <- files[grepl("TROPOSIF",files)]


for (ifile in seq(1,length(files))){
  cfile <- files[ifile]

  if (file.exists(file.path("/home/femeunier/Downloads/TROP/",cfile))){
    next()
  }

  system2("wget",
          paste0("https://thredds-x.ipsl.fr/thredds/fileServer/TROPOSIF-gridded/8-day/0.10-CF0.50-VZA40-SZA65/",cfile,
                 " ","/home/femeunier/Downloads/TROP/"))
}

library(terra)
A <- rast("/home/femeunier/Documents/projects/Congo.ED2/TROPOSIF_S5P-PAL-L2b__743__20250407_20250414__p13.nc")

plot(A[["SIF_743"]])
