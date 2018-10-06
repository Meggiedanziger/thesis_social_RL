rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

library(readr)
library(tidyverse)

#read in data
fit_RLW_6_social <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_6blocks_30trials.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RLW_6_social)[1] <- "LL_RLW"
names(fit_RLW_6_social)[2] <- "alpha_fit_RLW"
names(fit_RLW_6_social)[3] <- "beta_fit_RLW"
names(fit_RLW_6_social)[4] <- "weight_fit_RLW"
names(fit_RLW_6_social)[5] <- "BIC_RLW"
names(fit_RLW_6_social)[6] <- "AIC_RLW"
names(fit_RLW_6_social)[7] <- "id_RLW"
names(fit_RLW_6_social)[8] <- "alpha_sim_RLW"
names(fit_RLW_6_social)[9] <- "beta_sim_RLW"
names(fit_RLW_6_social)[10] <- "weight_sim_RLW"


fit_RL_6_social <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standardtoweight_6blocks_30trials.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL_6_social)[1] <- "LL"
names(fit_RL_6_social)[2] <- "alpha_fit"
names(fit_RL_6_social)[3] <- "beta_fit"
names(fit_RL_6_social)[4] <- "BIC"
names(fit_RL_6_social)[5] <- "AIC"
names(fit_RL_6_social)[6] <- "id"
names(fit_RL_6_social)[7] <- "alpha_sim"
names(fit_RL_6_social)[8] <- "beta_sim"
names(fit_RL_6_social)[9] <- "weight_sim"

df_fit_social <- cbind(fit_RL_6_social, fit_RLW_6_social)

df_fit_social$delta_BIC <- df_fit_social$BIC - df_fit_social$BIC_RLW

df_fit_social$weight_sign <- ifelse(df_fit_social$weight_sim < 0, -1, 1)

ggplot(aes(x = id, y = delta_BIC, fill = weight_sim), data = df_fit_social) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "dodgerblue3", high = "sienna1", expression(paste("Simulated ", omega, " values"))) +
  ylab(expression(paste(Delta, "BIC (RL - RLW)"))) +
  xlab("Simulated agent") +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text = element_text(size = 13, colour = "black")) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 13))

df_fit_social$group <- ifelse(df_fit_social$delta_BIC < 0, -1, 1)

sum(df_fit_social$group == -1)


df_fit_compare <-
  df_fit_social %>% 
  select(alpha_fit, alpha_fit_RLW, id, group)


library(Rcpp)
library(ez)

df_fit_compare$id <- factor(df_fit_compare$id)
df_fit_compare$group <- factor(df_fit_compare$group)
df_fit_compare$alpha_fit <- as.numeric(df_fit_compare$alpha_fit)
df_fit_compare$alpha_fit_RLW <- as.numeric(df_fit_compare$alpha_fit_RLW)


aov <- ezANOVA(data = df_fit_compare, dv = .(alpha_fit_RLW), wid = .(id), between = .(group), detailed = T, type = 2, return_aov = T)
aov
summary(aov)
ezStats(data = df_fit_compare, dv = .(alpha_fit_RLW), wid = .(id), between = .(group), type = 2)
ezPlot(data = df_fit_compare, dv = .(alpha_fit_RLW), wid = .(id), between = .(group), type = 2, x= .(group), do_lines=F, split= .(group))


