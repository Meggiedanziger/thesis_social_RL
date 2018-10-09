rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

#load required packages
library(tidyverse) 
library(readr)

################################ PARAMETER RECOVERY COEFFICIENTS STANDARD RL MODEL
##################################################################################
#read in data
fit_RL_6 <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standard_RL_6blocks_30trials.txt",
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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standard_RL_12blocks_30trials.txt",
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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standard_RL_18blocks_30trials.txt",
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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standard_RL_24blocks_30trials.txt",
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

#-----------------------------------------------------------------------------------------------------------------------------------
################################ PARAMETER RECOVERY COEFFICIENTS WEIGHT RL MODEL
################################################################################

#read in data
fit_RLW_6 <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_6blocks_30trials.txt",
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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_12blocks_30trials.txt",
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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_18blocks_30trials.txt",
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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_24blocks_30trials.txt",
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

corr_RLW <- data.frame(alpha_est = double(),
                      beta_est = double(),
                      weight_est = double())

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


############## PLOT HOW RECOVERY COEFFICIENTS CHANGE AS DATA SIZE INCREASES

corr_RLW <-
  corr_RLW %>% 
  gather(key = parameter, value = corr)

corr_RL <-
  corr_RL %>% 
  gather(key = parameter, value = corr)

corr_RL$blocks <- rep(c(6, 12, 18, 24))

corr_RLW$blocks <- rep(c(6, 12, 18, 24))

ggplot(aes(x = blocks, y = corr, color = parameter), data = corr_RLW) +
  geom_point()

ggplot(aes(x = blocks, y = corr, color = parameter), data = corr_RL) +
  geom_point()
