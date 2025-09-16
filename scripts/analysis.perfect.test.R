rm(list = ls())

system2("scp",
        paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.summ.RDS",
              "./outputs/"))

# ggplot(data = model.combin %>%
#          mutate(present = 1) %>%
#          ungroup() %>%
#          complete(ref.model = unique(model.combin$ref.model),
#                   model = models,
#                   fill = list(present = 0))) +
#   geom_tile(aes(x = ref.model, y = model, fill = as.factor(present)),
#             color = "black") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

df.summ <- readRDS("./outputs/df.summ.RDS") %>%
  group_by(variable,model) %>%
  filter(r[sigma_D == max(sigma_D)] > 0.5)

df.summ.m <- df.summ %>%
  group_by(sigma_D,sigma_S) %>%
  summarise(r.m = mean(r,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.summ) +
  geom_line(aes(x = sigma_D,
                y = 1 - r,
                color = variable,
                group = interaction(model,variable)),
            size = 0.1) +
  geom_line(data = df.summ.m,
            aes(x = sigma_D,
                y = 1 - r.m),
            color = "black") +
  geom_hline(yintercept = 1 - 0.7,linetype = 2) +
  geom_vline(xintercept = 0.85,
             linetype = 2) +
  # geom_segment(x = 0.85,xend = 0.85,
  #              y = -Inf, yend = 0.3) +
  labs(x = "",y = "") +
  # facet_wrap(~ ) +


  # facet_wrap( ~ variable,scales = "free") +
  guides(color = "none") +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  theme(text = element_text(size = 20))

S.M <- readRDS("./outputs/intermodel.distance.RDS")


df.summ.m
