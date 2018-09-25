rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")

########################### PARAMETER RECOVERY 2 LEARNING RATES RL MODEL 4 BLOCKS 30 TRIALS
###########################################################################################
library(tidyverse) 
library(readr)

#read in ex ante simulated data

sim_data <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulation_2lrates_4blocks_30trials.txt", 
             " ", col_names = F, trim_ws = TRUE)

names(sim_data)[1] <- "id"
names(sim_data)[2] <- "block"
names(sim_data)[3] <- "trial"
names(sim_data)[4] <- "chosen_option"
names(sim_data)[5] <- "feedback"

#id       <- c(1:125)
sim_data$alpha_ex <- rep(seq(1, 9, 2), each = 120)/10
sim_data$alpha_in <- rep(seq(1, 9, 2), each = 600)/10
sim_data$beta     <- rep(seq(1, 9, 2), each = 3000)/10

sim_data <-
  sim_data %>% 
  arrange(id)


sim_data_sum <- 
  sim_data %>% 
  group_by(id) %>% 
  summarize(accuracy = mean(chosen_option))


ggplot(aes(x = id, y = accuracy), data = sim_data_sum) +
  geom_jitter(size = 2, width = 0.8, height = 0.11, alpha = 0.6) +
  geom_smooth(color = "red", se = T, fill = "red", alpha = 0.4) +
  ylab("Accuracy") + 
  xlab("Virtual subject") +
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  theme_classic() 

sim_data_acc <- 
  sim_data %>% 
  group_by(id, trial, alpha_ex, alpha_in, beta) %>% 
  summarize(accuracy = mean(chosen_option))


ggplot(aes(x = trial, y = accuracy), data = sim_data_acc) +
  geom_jitter(color = "darkblue", size = 1.5, width = 0.8, height = 0.15, alpha = 0.4) +
  geom_smooth(color = "darkred", se = T, fill = "red", alpha = 0.4) +
  ylab("Accuracy") + 
  xlab("Trials") +
  scale_y_continuous(breaks = seq(0, 1.0, 0.25)) +
  theme_classic()


# plot_data <-  
#   sim_data %>% 
#   group_by(id, trial, alpha_ex, alpha_in, beta) %>% 
#   summarize(accuracy = mean(chosen_option))

ggplot(aes(x = trial, y = accuracy), data = sim_data_acc) +
  geom_jitter(color = "darkblue", size = 1, width = 0.8, height = 0.15, alpha = 0.4) +
  geom_smooth(color = "darkred", se = T, fill = "red", alpha = 0.4) +
  ylab("Accuracy") + 
  xlab("Trials") +
  scale_y_continuous(breaks = seq(0, 1.0, 0.25)) +
  theme_classic() + 
  facet_grid(alpha_in ~ alpha_ex)


#read in ex ante fitted data
modelfit <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_modelfit_2lrates_4blocks_30trials.txt", 
             " ", col_names = F, trim_ws = TRUE)

names(modelfit)[1]  <- "LL"
names(modelfit)[2]  <- "alpha_ex_fit"
names(modelfit)[3]  <- "alpha_in_fit"
names(modelfit)[4]  <- "beta_fit"
names(modelfit)[5]  <- "BIC"
names(modelfit)[6]  <- "AIC"
names(modelfit)[7]  <- "id"
names(modelfit)[8]  <- "alpha_ex_sim"
names(modelfit)[9]  <- "alpha_in_sim"
names(modelfit)[10] <- "beta_sim"


corr_alpha_ex <- cor.test(modelfit$alpha_ex_sim, modelfit$alpha_ex_fit)

recovery_alpha_ex <-
  ggplot(aes(x = alpha_ex_sim, y = alpha_ex_fit, color = alpha_ex_sim), data = modelfit) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red", "Simulated alpha \nvalues excluder") +
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  xlab("Simulated alpha values excluder") +
  ylab("Estimated alpha values excluder") +
  theme_classic()


corr_alpha_in <- cor.test(modelfit$alpha_in_sim, modelfit$alpha_in_fit)

recovery_alpha_in <-
  ggplot(aes(x = alpha_in_sim, y = alpha_in_fit, color = alpha_in_sim), data = modelfit) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  xlab("Simulated alpha values includer") +
  ylab("Estimated alpha values includer") +
  theme_classic()


corr_beta <- cor.test(modelfit$beta_sim, modelfit$beta_fit)

recovery_beta <-
  ggplot(aes(x = beta_sim, y = beta_fit, color = beta_sim), data = modelfit) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  xlab("Simulated beta values") +
  ylab("Estimated beta values") +
  theme_classic()


