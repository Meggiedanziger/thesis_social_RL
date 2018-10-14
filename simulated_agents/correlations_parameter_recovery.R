rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

#load required packages
library(tidyverse) 
library(readr)

################################ PARAMETER RECOVERY COEFFICIENTS STANDARD RL MODEL
##################################################################################
#read in data

# fit_RL_4 <-
#   read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standard_RL_4_30.txt",
#              " ", col_names = F, trim_ws = TRUE)
# 
# names(fit_RL_4)[1] <- "LL"
# names(fit_RL_4)[2] <- "alpha_fit"
# names(fit_RL_4)[3] <- "beta_fit"
# names(fit_RL_4)[4] <- "BIC"
# names(fit_RL_4)[5] <- "AIC"
# names(fit_RL_4)[6] <- "id"
# names(fit_RL_4)[7] <- "alpha_sim"
# names(fit_RL_4)[8] <- "beta_sim"
# 
# corr_alpha4 <- cor.test(fit_RL_4$alpha_sim, fit_RL_4$alpha_fit)
# corr_alpha4
# 
# corr_beta4 <- cor.test(fit_RL_4$beta_sim, fit_RL_4$beta_fit)
# corr_beta4

fit_RL_6 <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standard_RL_6_30.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL_6)[1] <- "LL"
names(fit_RL_6)[2] <- "alpha_fit"
names(fit_RL_6)[3] <- "beta_fit"
names(fit_RL_6)[4] <- "BIC"
names(fit_RL_6)[5] <- "AIC"
names(fit_RL_6)[6] <- "id"
names(fit_RL_6)[7] <- "alpha_sim"
names(fit_RL_6)[8] <- "beta_sim"

corr_alpha6 <- cor.test(fit_RL_6$alpha_sim, fit_RL_6$alpha_fit)
corr_alpha6

corr_beta6 <- cor.test(fit_RL_6$beta_sim, fit_RL_6$beta_fit)
corr_beta6

fit_RL_12 <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standard_RL_12_30.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL_12)[1] <- "LL"
names(fit_RL_12)[2] <- "alpha_fit"
names(fit_RL_12)[3] <- "beta_fit"
names(fit_RL_12)[4] <- "BIC"
names(fit_RL_12)[5] <- "AIC"
names(fit_RL_12)[6] <- "id"
names(fit_RL_12)[7] <- "alpha_sim"
names(fit_RL_12)[8] <- "beta_sim"

corr_alpha12 <- cor.test(fit_RL_12$alpha_sim, fit_RL_12$alpha_fit)
corr_alpha12

corr_beta12 <- cor.test(fit_RL_12$beta_sim, fit_RL_12$beta_fit)
corr_beta12


fit_RL_18 <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standard_RL_18_30.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL_18)[1] <- "LL"
names(fit_RL_18)[2] <- "alpha_fit"
names(fit_RL_18)[3] <- "beta_fit"
names(fit_RL_18)[4] <- "BIC"
names(fit_RL_18)[5] <- "AIC"
names(fit_RL_18)[6] <- "id"
names(fit_RL_18)[7] <- "alpha_sim"
names(fit_RL_18)[8] <- "beta_sim"

corr_alpha18 <- cor.test(fit_RL_18$alpha_sim, fit_RL_18$alpha_fit)
corr_alpha18

corr_beta18 <- cor.test(fit_RL_18$beta_sim, fit_RL_18$beta_fit)
corr_beta18

fit_RL_24 <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standard_RL_24_30.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL_24)[1] <- "LL"
names(fit_RL_24)[2] <- "alpha_fit"
names(fit_RL_24)[3] <- "beta_fit"
names(fit_RL_24)[4] <- "BIC"
names(fit_RL_24)[5] <- "AIC"
names(fit_RL_24)[6] <- "id"
names(fit_RL_24)[7] <- "alpha_sim"
names(fit_RL_24)[8] <- "beta_sim"

corr_alpha24 <- cor.test(fit_RL_24$alpha_sim, fit_RL_24$alpha_fit)
corr_alpha24

corr_beta24 <- cor.test(fit_RL_24$beta_sim, fit_RL_24$beta_fit)
corr_beta24

corr_RL <- data.frame(alpha_est = double(),
                 beta_est = double())

corr_RL[1, 1] <- corr_alpha6$estimate
corr_RL[1, 2] <- corr_beta6$estimate
corr_RL[2, 1] <- corr_alpha12$estimate
corr_RL[2, 2] <- corr_beta12$estimate
corr_RL[3, 1] <- corr_alpha18$estimate
corr_RL[3, 2] <- corr_beta18$estimate
corr_RL[4, 1] <- corr_alpha24$estimate
corr_RL[4, 2] <- corr_beta24$estimate

corr_RL <-
  corr_RL %>% 
  gather(key = parameter, value = corr)

corr_RL$blocks <- rep(c(6, 12, 18, 24))

ggplot(aes(x = blocks, y = corr, color = parameter), data = corr_RL) +
  geom_point(size = 2) +
  geom_line(size = 1, alpha = 0.7) +
  scale_color_manual(labels = c(expression(paste("", alpha, "")), expression(paste("", beta, ""))),
                     values = c("red", "limegreen")) +
  scale_y_continuous(breaks = seq(0.1, .9, 0.2)) +
  scale_x_continuous(breaks = c(6, 12, 18, 24)) +
  xlab("Blocks") +
  ylab("Correlation coefficient value") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13)) + 
  theme(axis.title.y = element_text(size = 13))+
  theme(axis.text = element_text(size = 12, colour = "black")) +
  theme(legend.text = element_text(size = 13)) +
  theme(legend.title = element_blank())
  


#-----------------------------------------------------------------------------------------------------------------------------------
################################ PARAMETER RECOVERY COEFFICIENTS WEIGHT RL MODEL
################################################################################

#read in data

# fit_RLW_4 <-
#   read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_4_30.txt",
#              " ", col_names = F, trim_ws = TRUE)
# 
# names(fit_RLW_4)[1] <- "LL"
# names(fit_RLW_4)[2] <- "alpha_fit"
# names(fit_RLW_4)[3] <- "beta_fit"
# names(fit_RLW_4)[4] <- "weight_fit"
# names(fit_RLW_4)[5] <- "BIC"
# names(fit_RLW_4)[6] <- "AIC"
# names(fit_RLW_4)[7] <- "id"
# names(fit_RLW_4)[8] <- "alpha_sim"
# names(fit_RLW_4)[9] <- "beta_sim"
# names(fit_RLW_4)[10] <- "weight_sim"
# 
# 
# corr_alpha4 <- cor.test(fit_RLW_4$alpha_sim, fit_RLW_4$alpha_fit)
# corr_alpha4
# 
# corr_beta4 <- cor.test(fit_RLW_4$beta_sim, fit_RLW_4$beta_fit)
# corr_beta4
# 
# corr_weight4 <- cor.test(fit_RLW_4$weight_sim, fit_RLW_4$weight_fit)
# corr_weight4

fit_RLW_6 <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_6_30_new.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RLW_6)[1] <- "LL"
names(fit_RLW_6)[2] <- "alpha_fit"
names(fit_RLW_6)[3] <- "beta_fit"
names(fit_RLW_6)[4] <- "weight_fit"
names(fit_RLW_6)[5] <- "BIC"
names(fit_RLW_6)[6] <- "AIC"
names(fit_RLW_6)[7] <- "id"
names(fit_RLW_6)[8] <- "alpha_sim"
names(fit_RLW_6)[9] <- "beta_sim"
names(fit_RLW_6)[10] <- "weight_sim"

corr_alpha6 <- cor.test(fit_RLW_6$alpha_sim, fit_RLW_6$alpha_fit)
corr_alpha6

corr_beta6 <- cor.test(fit_RLW_6$beta_sim, fit_RLW_6$beta_fit)
corr_beta6

corr_weight6 <- cor.test(fit_RLW_6$weight_sim, fit_RLW_6$weight_fit)
corr_weight6

fit_RLW_12 <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_12_30_new.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RLW_12)[1] <- "LL"
names(fit_RLW_12)[2] <- "alpha_fit"
names(fit_RLW_12)[3] <- "beta_fit"
names(fit_RLW_12)[4] <- "weight_fit"
names(fit_RLW_12)[5] <- "BIC"
names(fit_RLW_12)[6] <- "AIC"
names(fit_RLW_12)[7] <- "id"
names(fit_RLW_12)[8] <- "alpha_sim"
names(fit_RLW_12)[9] <- "beta_sim"
names(fit_RLW_12)[10] <- "weight_sim"

corr_alpha12 <- cor.test(fit_RLW_12$alpha_sim, fit_RLW_12$alpha_fit)
corr_alpha12

corr_beta12 <- cor.test(fit_RLW_12$beta_sim, fit_RLW_12$beta_fit)
corr_beta12

corr_weight12 <- cor.test(fit_RLW_12$weight_sim, fit_RLW_12$weight_fit)
corr_weight12

fit_RLW_18 <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_18_30_new.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RLW_18)[1] <- "LL"
names(fit_RLW_18)[2] <- "alpha_fit"
names(fit_RLW_18)[3] <- "beta_fit"
names(fit_RLW_18)[4] <- "weight_fit"
names(fit_RLW_18)[5] <- "BIC"
names(fit_RLW_18)[6] <- "AIC"
names(fit_RLW_18)[7] <- "id"
names(fit_RLW_18)[8] <- "alpha_sim"
names(fit_RLW_18)[9] <- "beta_sim"
names(fit_RLW_18)[10] <- "weight_sim"

corr_alpha18 <- cor.test(fit_RLW_18$alpha_sim, fit_RLW_18$alpha_fit)
corr_alpha18

corr_beta18 <- cor.test(fit_RLW_18$beta_sim, fit_RLW_18$beta_fit)
corr_beta18

corr_weight18 <- cor.test(fit_RLW_18$weight_sim, fit_RLW_18$weight_fit)
corr_weight18

fit_RLW_24 <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_24_30_new.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RLW_24)[1] <- "LL"
names(fit_RLW_24)[2] <- "alpha_fit"
names(fit_RLW_24)[3] <- "beta_fit"
names(fit_RLW_24)[4] <- "weight_fit"
names(fit_RLW_24)[5] <- "BIC"
names(fit_RLW_24)[6] <- "AIC"
names(fit_RLW_24)[7] <- "id"
names(fit_RLW_24)[8] <- "alpha_sim"
names(fit_RLW_24)[9] <- "beta_sim"
names(fit_RLW_24)[10] <- "weight_sim"

corr_alpha24 <- cor.test(fit_RLW_24$alpha_sim, fit_RLW_24$alpha_fit)
corr_alpha24

corr_beta24 <- cor.test(fit_RLW_24$beta_sim, fit_RLW_24$beta_fit)
corr_beta24

corr_weight24 <- cor.test(fit_RLW_24$weight_sim, fit_RLW_24$weight_fit)
corr_weight24

corr_RLW <- data.frame(alpha_est_RLW = double(),
                      beta_est_RLW = double(),
                      weight_est_RLW = double())

corr_RLW[1, 1] <- corr_alpha6$estimate
corr_RLW[1, 2] <- corr_beta6$estimate
corr_RLW[1, 3] <- corr_weight6$estimate
corr_RLW[2, 1] <- corr_alpha12$estimate
corr_RLW[2, 2] <- corr_beta12$estimate
corr_RLW[2, 3] <- corr_weight12$estimate
corr_RLW[3, 1] <- corr_alpha18$estimate
corr_RLW[3, 2] <- corr_beta18$estimate
corr_RLW[3, 3] <- corr_weight18$estimate
corr_RLW[4, 1] <- corr_alpha24$estimate
corr_RLW[4, 2] <- corr_beta24$estimate
corr_RLW[4, 3] <- corr_weight24$estimate
# corr_RLW[5, 1] <- corr_alpha4$estimate
# corr_RLW[5, 2] <- corr_beta4$estimate
# corr_RLW[5, 3] <- corr_weight4$estimate

############## PLOT HOW RECOVERY COEFFICIENTS CHANGE AS DATA SIZE INCREASES

corr_RLW <-
  corr_RLW %>% 
  gather(key = parameter, value = corr)


corr_RLW$blocks <- rep(c(6, 12, 18, 24))

ggplot(aes(x = blocks, y = corr, color = parameter), data = corr_RLW) +
  geom_point(size = 2) +
  geom_line(size = 1, alpha = 0.7) +
  scale_color_manual(labels = c(expression(paste("", alpha, "")), expression(paste("", beta, "")), 
                                expression(paste("", omega, ""))),
                     values = c("red", "limegreen", "sienna1")) +
  scale_x_continuous(breaks = c(6, 12, 18, 24)) +
  xlab("Blocks") +
  ylab("Correlation coefficient value") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13)) + 
  theme(axis.title.y = element_text(size = 13))+
  theme(axis.text = element_text(size = 12, colour = "black")) +
  theme(legend.text = element_text(size = 13)) +
  theme(legend.title = element_blank())
  

