rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

# system2("rsync",
#         paste("-avz","hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.monthly.tas.basin.*",
#               "./outputs/"))

l.files <- list.files("./outputs",pattern = "df.monthly.tas.basin*")
OP.files.no.ext <- tools::file_path_sans_ext((l.files))
all.attributes <- strsplit(OP.files.no.ext,split = "\\.")

scenars <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                        function(i){
                                                          data.frame(var = all.attributes[[i]][5])}))))
models <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                       function(i){
                                                         data.frame(var = all.attributes[[i]][6])}))))
data.sum <- data.sum.sum <- data.frame()
for (ifile in seq(1,length(l.files))){

  print(ifile)
  cscenario <- scenars[ifile]

  cdata <- readRDS(paste0("./outputs/",l.files[ifile])) %>%
    mutate(model = models[ifile]) %>%
    mutate(basin = case_when(lon < -30 ~ "Amazon",
                             TRUE ~ "Congo"))


  data.sum <- bind_rows(list(data.sum,
                             cdata %>%
                               group_by(basin,year,month,scenario,model) %>%
                               summarise(tas.m = mean(tas,na.rm = TRUE),
                                         .groups = "keep")))

}

data.sum <- data.sum %>%
  filter(tas.m <= 400)

ggplot(data.sum %>%
         filter(year %in% c(1970:2000,2071:2100))) +
  geom_boxplot(aes(fill = scenario,
                   y = tas.m - 273.15)) +
  facet_wrap(~ basin) +
  theme_bw()

saveRDS(data.sum,
        "./outputs/CMIP6.RDS")


