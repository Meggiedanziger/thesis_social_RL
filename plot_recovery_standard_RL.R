rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")

########################### PARAMETER RECOVERY STANDARD RL MODEL 4 BLOCKS 30 TRIALS
###################################################################################
library(tidyverse) 
library(readr)

#read in ex ante simulated data
sim_data <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulation_standard_RL_4blocks_30trials.txt", 
             " ", col_names = F, trim_ws = TRUE)

names(sim_data)[1] <- "id"
names(sim_data)[2] <- "block"
names(sim_data)[3] <- "trial"
names(sim_data)[4] <- "chosen_option"
names(sim_data)[5] <- "feedback"

sim_data <-
  sim_data %>% 
  arrange(id)


#read in ex ante fitted data
modelfit <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_modelfit_standard_RL_4blocks_30trials.txt", 
             " ", col_names = F, trim_ws = TRUE)

names(modelfit)[1] <- "LL"
names(modelfit)[2] <- "alpha_fit"
names(modelfit)[3] <- "beta_fit"  
names(modelfit)[4] <- "BIC"
names(modelfit)[5] <- "AIC"
names(modelfit)[6] <- "id"
names(modelfit)[7] <- "alpha_sim"
names(modelfit)[8] <- "beta_sim"


corr_alpha <- cor.test(modelfit$alpha_sim, modelfit$alpha_fit)

recovery_alpha <-
  ggplot(aes(x = alpha_sim, y = alpha_fit, color = alpha_sim), data = modelfit) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1.0, 0.2)) +
  xlab("Simulated alpha values") +
  ylab("Estimated alpha values") +
  theme_classic()


corr_beta <- cor.test(modelfit$beta_sim, modelfit$beta_fit)

recovery_beta <-
  ggplot(aes(x = beta_sim, y = beta_fit, color = beta_sim), data = modelfit) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1.0, 0.2)) +
  xlab("Simulated beta values") +
  ylab("Estimated beta values") +
  theme_classic()

########################### PARAMETER RECOVERY STANDARD RL MODEL 8 BLOCKS 30 TRIALS
###################################################################################

#read in ex ante fitted data
modelfit <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_modelfit_standard_RL_8blocks_30trials.txt", 
             " ", col_names = F, trim_ws = TRUE)

names(modelfit)[1] <- "LL"
names(modelfit)[2] <- "alpha_fit"
names(modelfit)[3] <- "beta_fit"  
names(modelfit)[4] <- "BIC"
names(modelfit)[5] <- "AIC"
names(modelfit)[6] <- "id"
names(modelfit)[7] <- "alpha_sim"
names(modelfit)[8] <- "beta_sim"


corr_alpha <- cor.test(modelfit$alpha_sim, modelfit$alpha_fit)

recovery_alpha <-
  ggplot(aes(x = alpha_sim, y = alpha_fit, color = alpha_sim), data = modelfit) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1.0, 0.2)) +
  xlab("Simulated alpha values") +
  ylab("Estimated alpha values") +
  theme_classic()


corr_beta <- cor.test(modelfit$beta_sim, modelfit$beta_fit)

recovery_beta <-
  ggplot(aes(x = beta_sim, y = beta_fit, color = beta_sim), data = modelfit) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1.0, 0.2)) +
  xlab("Simulated beta values") +
  ylab("Estimated beta values") +
  theme_classic()
