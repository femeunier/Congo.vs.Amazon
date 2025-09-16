rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(reldist)
library(Hmisc)

cfile <- "~/Downloads/Table_S1_CMIP6_models.csv"  # Past warming trend constrains future warming in CMIP6 models
cfile2 <- "~/Downloads/Table_2_CMIP6_ECS_TCR.csv"  #https://www.science.org/doi/epdf/10.1126/sciadv.aba1981
# cfile2 <- "~/Downloads/Table.completed.csv"  #https://www.science.org/doi/epdf/10.1126/sciadv.aba1981


cdata <- bind_rows(read.csv(cfile) %>%
  rename(model = Model) %>%
    mutate(source = "1"),
  read.csv(cfile2) %>%
    rename(model = Model) %>%
    mutate(source = "2")) %>%
  dplyr::select(model,ECS,TCR,source) %>%
  filter(source == "2")

weights <- readRDS("./outputs/CMIP6.weights.Tropics.RDS") %>%
  filter(model != "CRUJRA")

data.vs.weight <- weights %>%
  right_join(cdata,
            by = "model") %>%
  arrange(model) %>%
  arrange(desc(w))

wtd.iqr <- function(x, na.rm = FALSE, weight=FALSE) {
  reldist::wtd.quantile(x, q=0.75, na.rm = na.rm, weight=weight)-
    reldist::wtd.quantile(x, q=0.25, na.rm = na.rm, weight=weight)
}



data.vs.weight %>%
  ungroup() %>%
  filter(!is.na(w)) %>%
  # group_by(source) %>%
  summarise(TCR.wm = mean(TCR,w,na.rm = TRUE),
            ECS.wm = mean(ECS,w,na.rm = TRUE),

            TCR.wsd = sqrt(Hmisc::wtd.var(TCR,w,na.rm = TRUE,normwt=TRUE)),
            ECS.wsd = sqrt(Hmisc::wtd.var(ECS,w,na.rm = TRUE,normwt=TRUE)),

            # TCR.IQR.wm = wtd.iqr(TCR,na.rm = TRUE,weight = w),
            # ECS.IQR.wm = wtd.iqr(ECS,na.rm = TRUE,weight = w),
            #
            # total.w.TCR = sum(w[!is.na(TCR)],na.rm = TRUE),
            # total.w.ECS = sum(w[!is.na(ECS)],na.rm = TRUE),
            # TCR.m = mean(TCR,na.rm = TRUE),
            # ECS.m = mean(ECS,na.rm = TRUE),

            TCR.sd = sqrt(var(TCR,na.rm = TRUE)),
            ECS.sd = sqrt(var(ECS,na.rm = TRUE)),

            TCR.IQR.m = IQR(TCR,na.rm = TRUE),
            ECS.IQR.m = IQR(ECS,na.rm = TRUE))





data.vs.weight %>%
  filter(!is.na(w)) %>%
  group_by(model) %>%
  summarise(ECS = mean(ECS,na.rm = TRUE),
            TCR = mean(TCR,na.rm = TRUE),
            w = unique(w)) %>%
  ungroup() %>%
  summarise(TCR.wm = weighted.mean(TCR,w,na.rm = TRUE),
            ECS.wm = weighted.mean(ECS,w,na.rm = TRUE),
            total.w.TCR = sum(w[!is.na(TCR)],na.rm = TRUE),
            total.w.ECS = sum(w[!is.na(ECS)],na.rm = TRUE),
            TCR.m = mean(TCR,na.rm = TRUE),
            ECS.m = mean(ECS,na.rm = TRUE))


hist(data.vs.weight$TCR)

data.vs.weight.all <- bind_rows(data.vs.weight %>%
                                  mutate(type = "weighted"),

                                data.vs.weight %>%
                                  # filter(!is.na(w)) %>%
                                  mutate(w = 1) %>%
                                  mutate(type = "naive all"),

                                data.vs.weight %>%
                                  filter(!is.na(w)) %>%
                                  mutate(w = 1) %>%
                                  mutate(type = "naive selected"))


data.vs.weight.all.long <- data.vs.weight.all %>%
  pivot_longer(cols = c(ECS,TCR),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = factor(variable,
                           levels = c("TCR","ECS")))


ggplot(data = data.vs.weight.all.long %>%
         filter(!is.na(w)),
       aes(x = type, y = value,
           weight = w, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, color = "black", size = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() +
  labs(x="", y = "") +
  scale_fill_manual(values = c("#66c2a5",'#fc8d62','#8da0cb')) +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 20),strip.background = element_blank(),
        strip.text = element_blank())


weights %>%
  left_join(cdata,
             by = "model") %>%
  filter(!is.na(TCR)) %>%
  arrange(desc(w)) %>%
  pull(w) %>%
  sum()

weights %>%
  left_join(cdata,
            by = "model") %>%
  filter(!is.na(ECS)) %>%
  arrange(desc(w)) %>%
  pull(w) %>%
  sum()

