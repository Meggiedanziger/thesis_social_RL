rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")

########################### PARAMETER RECOVERY WEIGHT RL MODEL 4 BLOCKS 30 TRIALS
#################################################################################
library(tidyverse) 
library(readr)

#read in ex ante simulated data
sim_data <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulation_weight_model_4blocks_30trials.txt", 
             " ", col_names = F, trim_ws = TRUE)

names(sim_data)[1] <- "id"
names(sim_data)[2] <- "block"
names(sim_data)[3] <- "trial"
names(sim_data)[4] <- "chosen_option"
names(sim_data)[5] <- "feedback"

sim_data <-
  sim_data %>% 
  arrange(id)

sim_data_sum <- 
  sim_data %>% 
  group_by(id) %>% 
  summarize(accuracy = mean(chosen_option)) %>% 
  mutate(weight = ifelse(id <= 125, -1, 1))

sim_data_sum$weight <- as.factor(sim_data_sum$weight)

colorweight <- (ifelse(sim_data_sum$weight == 1, "blue1", "purple"))

colfunc <- colorRampPalette(c("red","magenta","purple","royalblue"))
colfunc2 <-colorRampPalette(c("red","yellow","springgreen","royalblue"))
colfunc3 <-colorRampPalette(c("blue", "royalblue", "cyan","turquoise", "seagreen"))

plot(rep(1, 250), col = (colfunc(250)), pch = 19, cex = 3)

ggplot(aes(x = id, y = accuracy, color = accuracy), data = sim_data_sum) +
  geom_jitter(size = 1.5, width = 0.8, height = 0.11, alpha = 0.9, col = colfunc(250)) +
  geom_smooth(color = "red", se = T, fill = "red", alpha = 0.4) +
  ylab("Accuracy") + 
  xlab("Virtual subject") +
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  theme_classic() +
  geom_vline(xintercept = 125, linetype = "dashed", size = .5)


plot_data <-  
  sim_data %>% 
  group_by(id) %>% 
  mutate(weight = ifelse(id <= 125, -1, 1))

ggplot(aes(x = trial, y = chosen_option), data = plot_data) +
  geom_jitter(size = 1, width = 0.8, height = 0.11, alpha = 0.2) +
  geom_smooth(color = "red", se = T, fill = "red", alpha = 0.2) +
  ylab("Selected option") + 
  xlab("Trials") +
  scale_y_continuous(breaks = seq(0, 1)) +
  theme_classic() + 
  facet_grid(block ~ weight)


#read in ex ante fitted data
modelfit <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_modelfit_weight_model_8blocks_30trials.txt", 
             " ", col_names = F, trim_ws = TRUE)

names(modelfit)[1]  <- "LL"
names(modelfit)[2]  <- "alpha_fit"
names(modelfit)[3]  <- "beta_fit"
names(modelfit)[4]  <- "weight_fit"
names(modelfit)[5]  <- "BIC"
names(modelfit)[6]  <- "AIC"
names(modelfit)[7]  <- "id"
names(modelfit)[8]  <- "alpha_sim"
names(modelfit)[9]  <- "beta_sim"
names(modelfit)[10] <- "weight_sim"


corr_alpha <- cor.test(modelfit$alpha_sim, modelfit$alpha_fit)

recovery_alpha <-
  ggplot(aes(x = alpha_sim, y = alpha_fit, color = alpha_sim), data = modelfit) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  xlab("Simulated alpha values") +
  ylab("Estimated alpha values") +
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


corr_weight <- cor.test(modelfit$weight_sim, modelfit$weight_fit)

recovery_weight <-
  ggplot(aes(x = weight_sim, y = weight_fit, color = weight_sim), data = modelfit) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(-0.9, 0.9, 0.2)) +
  scale_x_continuous(breaks = seq(-0.9, 0.9, 0.2)) +
  xlab("Simulated weight values") +
  ylab("Estimated weight values") +
  theme_classic()
