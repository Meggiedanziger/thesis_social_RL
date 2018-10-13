rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

library(readr)
library(tidyverse)
library(Rcpp)
library(ez)
library(MASS)

#########TEST IF FITTED LEARNING RATES AND TEMPERATURES OF THE BEST FITTING MODEL TO THE SOCIAL DATA (RLW)
#########DIFFERS IN THE GROUPS AS CATEGORIZED BY MEANS OF COMPUTATIONAL PHENOTYPING
###########################################################################################################

#read in data
fit_RLW_6_social <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_12_30_new.txt",
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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standardtoweight_12_30_new.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL_6_social)[1] <- "LL"
names(fit_RL_6_social)[2] <- "alpha_fit"
names(fit_RL_6_social)[3] <- "beta_fit"
names(fit_RL_6_social)[4] <- "BIC"
names(fit_RL_6_social)[5] <- "AIC"
names(fit_RL_6_social)[6] <- "id"
names(fit_RL_6_social)[7] <- "alpha_sim"
names(fit_RL_6_social)[8] <- "beta_sim"
#names(fit_RL_6_social)[9] <- "weight_sim"

df_fit_social <- cbind(fit_RL_6_social, fit_RLW_6_social)

df_fit_social$delta_BIC <- 
  df_fit_social$BIC - df_fit_social$BIC_RLW #calculate difference in BIC

df_fit_social$weight_sign <- 
  as.factor(ifelse(df_fit_social$weight_sim_RLW < 0, -1, 1)) #categorize by omega (positive/negative)

#plot shows that agents can be categorized by best fitting model
#BIC varies as a function of omega
#negative omega values --> RLW
#positive omega values --> RL
ggplot(aes(x = id, y = delta_BIC, fill = weight_sign), data = df_fit_social) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = c("dodgerblue3", "sienna1"), expression(paste("Simulated ", omega, " values")), 
                    labels = c("< 0", "> 0")) +
  ylab(expression(paste(Delta, "BIC (RL - RLW)"))) +
  xlab("Simulated agent") +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13, colour = "black")) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 13))


#allows computational phenotyping
#1  = RLW
#-1 = RL
df_fit_social$group <- as.factor(ifelse(df_fit_social$delta_BIC > 0, 1, -1))

sum(df_fit_social$group == -1)

#compare fitted learning rates and temperatures of both groups
df_fit_compare <-
  df_fit_social %>% 
  select(alpha_fit, alpha_fit_RLW, id, group, beta_fit_RLW)

#compare fitted temperatures of both groups
ggplot(aes(y = alpha_fit_RLW, color = group, x = group), data = df_fit_compare) +
  geom_boxplot() +
  scale_color_manual(values = c("sienna1", "dodgerblue3")) +
  scale_x_discrete(labels = c("RL", "RLW")) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  theme_classic()

ggplot(aes(y = beta_fit_RLW, x = group, color = group), data = df_fit_compare) +
  geom_boxplot() +
  scale_color_manual(values = c("sienna1", "dodgerblue3")) +
  scale_x_discrete(labels = c("RL", "RLW")) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  theme_classic()


df_fit_compare$id <- factor(df_fit_compare$id)
df_fit_compare$group <- factor(df_fit_compare$group)
df_fit_compare$alpha_fit <- as.numeric(df_fit_compare$alpha_fit)
df_fit_compare$alpha_fit_RLW <- as.numeric(df_fit_compare$alpha_fit_RLW)
df_fit_compare$beta_fit_RLW <- as.numeric(df_fit_compare$beta_fit_RLW)

df <-
  df_fit_compare %>% 
  select(id, group, beta_fit_RLW, alpha_fit_RLW)

hist(df$beta_fit_RLW)
hist(df$alpha_fit_RLW)

# df2 <-
#   df %>% 
#   spread(key = group, value = alpha_fit_RLW, sep = "_")

#like a paired t test for not normally distributed data
wilcox.test(alpha_fit_RLW ~ group, data = df)

wilcox.test(beta_fit_RLW ~ group, data = df)

#plot BIC_RL as a function of BIC_RLW
#shows that RLW better fits the social condiion data if omega is < 0
#if omega > 0 RL and RLW fit the data almost equally well 
ggplot(aes(x = BIC_RLW, y = BIC, shape = group, color = weight_fit_RLW), data = df_fit_social) +
  geom_jitter(width = 5, height = 5, size = 4, alpha = 0.8) +
  scale_color_gradient(low = "dodgerblue3", high = "sienna1", expression(paste("Fitted ", omega, " values")),
                       limits = c(-1, 1), breaks = c(-.75, -.25, .25, .75)) +
  guides(color = guide_colorbar(barwidth = 0.7, barheight = 10)) +
  scale_shape_manual(values = c(16, 17), labels = c("RL \nn = 26 \n", "RLW \nn = 26"), "Computational \nphenotype") +
  ylab("BIC RL") +
  xlab("BIC RLW") +
  scale_x_continuous(breaks = seq(100, 500, 100)) +
  scale_y_continuous(breaks = seq(100, 500, 100)) +
  geom_abline(color = "gray31") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 12, colour = "black")) +
  theme(legend.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) 
  
# aov_alpha_fit <- ezANOVA(data = df_fit_compare, dv = .(alpha_fit_RLW), wid = .(id), between = .(group), detailed = T, type = 2, return_aov = T)
# aov_alpha_fit
# summary(aov_alpha_fit)
# ezStats(data = df_fit_compare, dv = .(alpha_fit_RLW), wid = .(id), 
#         between = .(group), type = 2)
# ezPlot(data = df_fit_compare, dv = .(alpha_fit_RLW), wid = .(id), 
#        between = .(group), type = 2, x= .(group), do_lines=F, split= .(group))
# 
# 
# aov_beta_fit <- ezANOVA(data = df_fit_compare, dv = .(beta_fit_RLW), wid = .(id), between = .(group), detailed = T, type = 2, return_aov = T)
# aov_beta_fit
# summary(aov_alpha_fit)
# ezStats(data = df_fit_compare, dv = .(beta_fit_RLW), wid = .(id), 
#         between = .(group), type = 2)
# ezPlot(data = df_fit_compare, dv = .(beta_fit_RLW), wid = .(id), 
#        between = .(group), type = 2, x= .(group), do_lines=F, split= .(group))
