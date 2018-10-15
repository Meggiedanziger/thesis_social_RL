rm(list=ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")

library(tidyverse)

#read in data
library(readr)

sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulations/simulation_standard_RL_12blocks_60trials.txt", 
                       " ", escape_double = FALSE, col_names = F, 
                       trim_ws = TRUE)

names(sim_data)[1] <- "id"
names(sim_data)[2] <- "block"
names(sim_data)[3] <- "trial"
names(sim_data)[4] <- "chosen_option"
names(sim_data)[5] <- "feedback"


sim_data <-
  sim_data %>% 
  arrange(id)

#add columns for temperature (beta) and learning rate (alpha)
temp  <- rep(c(1, 4, 7, 10, 30, 50, 70, 100), each = 7200)/10
lrate <- rep(1:10, each = 720)/10

df <- cbind(lrate, temp)
sim_data <- cbind(sim_data, df)

sim_data_sum <- 
  sim_data %>% 
  group_by(id) %>% 
  summarize(accuracy = mean(chosen_option))

ggplot(data = sim_data_sum, aes(x = id, y = accuracy)) +
  geom_jitter(size = 2, alpha = 0.6) +
  xlab("simulated agent") +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.1)) +
  expand_limits(y = 1.0) +
  scale_x_continuous(breaks = seq(50, 250, 50)) +
  theme_classic()

mean(sim_data_sum$accuracy)


#plot accuracy by lrate + temp
plot_data1 <-
  sim_data %>%
  group_by(id, lrate, temp) %>%
  summarize(mean_acc = mean(chosen_option))

#plot accuracy as a function of alpha
ggplot(plot_data1, aes(x = lrate, y = mean_acc)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth() +
  xlab("learning rate") +
  ylab("accuracy") +
  scale_x_continuous(breaks = seq(0, 1.0, 0.1))

#plot accuracy as a function of beta
ggplot(plot_data1, aes(x = temp, y = mean_acc)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth() +
  xlab("temperature") +
  ylab("accuracy") +
  scale_x_continuous(breaks = seq(0, 10, 2))

#plot accuracy collapsed over all ids over time (= trials)
plot_data2 <-
  sim_data %>% 
  group_by(id, trial, lrate, temp) %>% 
  summarize(accuracy = mean(chosen_option))

ggplot(plot_data2, aes(x = trial, y = accuracy)) +
  geom_jitter(size = 2, width = 0.5, height = 0.03, color = "steelblue4", alpha = 0.4) +
  geom_smooth(size = 1.2, color = "tomato", se = T, fill = "sienna", alpha = 0.5) +
  ylab("accuracy") + 
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  theme_classic()

#converting alpha to factor for plotting facets
plot_data2$lrate <- 
  factor(plot_data2$lrate, labels = c("alpha = 0.1", "alpha = 0.2", "alpha = 0.3", "alpha = 0.4", "alpha = 0.5", 
                                     "alpha = 0.6", "alpha = 0.7", "alpha = 0.8", "alpha = 0.9", "alpha = 1"))

#plot accuracy as function of alpha in facets
ggplot(plot_data2, aes(x = trial, y = accuracy)) +
  geom_jitter(size = 1, width = 0.5, height = 0.1, color = "steelblue4", alpha = 0.4) +
  #geom_point(color = "steelblue4", alpha = 0.5) +
  geom_smooth(size = 0.9, color = "tomato", se = F, fill = "red", alpha = 0.2) +
  facet_wrap(~ lrate, ncol = 2, nrow = 5) +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.5)) +
  scale_x_continuous(breaks = seq(10, 50, 20)) +
  ylab("Accuracy") +
  xlab("Trials") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.title.x = element_text(size = 12)) + 
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 11, colour = "black"))

#converting beta to factor for plotting facets
plot_data2$temp <- 
  factor(plot_data2$temp, labels = c("beta = 0.1", "beta = 0.4", "beta = 0.7", "beta = 1",
                                     "beta = 3", "beta = 5", "beta = 7", "beta = 10"))

#plot accuracy as function of beta in facets
ggplot(plot_data2, aes(x = trial, y = accuracy)) +
  geom_jitter(size = 1, width = 0.5, height = 0.1, color = "steelblue4", alpha = 0.4) +
  #geom_point(color = "steelblue4", alpha = 0.5) +
  geom_smooth(size = 0.9, color = "tomato", se = F, fill = "red", alpha = 0.2) +
  facet_wrap(~ temp, ncol = 2, nrow = 4) +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.5)) +
  scale_x_continuous(breaks = seq(10, 50, 20)) +
  ylab("Accuracy") +
  xlab("Trials") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.title.x = element_text(size = 12)) + 
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 11, colour = "black"))

#plot accuracy as function of alpha and beta in facets
ggplot(plot_data2, aes(x = trial, y = accuracy)) +
  geom_jitter(size = 1, width = 0.5, height = 0.1, color = "steelblue4", alpha = 0.7) +
  #geom_point(color = "steelblue4", alpha = 0.5) +
  geom_smooth(size = 0.9, color = "tomato", se = F, fill = "red", alpha = 0.2) +
  facet_grid(temp ~ lrate) +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.5)) +
  scale_x_continuous(breaks = seq(10, 50, 20)) +
  ylab("Accuracy") +
  xlab("Trials") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 12, colour = "black")) +
  theme(strip.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 11, colour = "black"))

