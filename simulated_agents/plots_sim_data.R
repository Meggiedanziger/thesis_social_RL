rm(list=ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

library(tidyverse)

#read in data
library(readr)

#read in simulated agents data standard RL model
sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_standard_RL_24blocks_30trials.txt", 
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



sim_data_sum <- 
  sim_data %>% 
  group_by(id) %>% 
  summarize(accuracy = mean(chosen_option))

ggplot(data = sim_data_sum, aes(x = id, y = accuracy)) +
  geom_jitter(size = 2, alpha = 0.6) +
  xlab("simulated agent") +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.1)) +
  expand_limits(y = 1.0) +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  theme_classic()

mean(sim_data_sum$accuracy)


#plot accuracy collapsed over all ids over time (= trials)
plot_data2 <-
  sim_data %>% 
  group_by(id, trial) %>% 
  summarize(accuracy = mean(chosen_option))

ggplot(plot_data2, aes(x = trial, y = accuracy)) +
  geom_jitter(size = 1.5, width = 0.5, height = 0.1, color = "steelblue4", alpha = 0.4) +
  geom_smooth(size = 1, color = "tomato", se = T, fill = "sienna", alpha = 0.6) +
  xlab("Trials") + 
  ylab("Accuracy") +
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  scale_y_continuous(breaks = seq(0.1, 1.0, 0.2)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13)) + 
  theme(axis.title.y = element_text(size = 13))+
  theme(axis.text = element_text(size = 11, colour = "black"))

ggplot(plot_data2, aes(x = trial, y = accuracy)) +
  geom_point(size = 1.2, color = "steelblue4", alpha = 0.7) +
  geom_smooth(size = 0.7, color = "tomato", se = F, fill = "sienna1", alpha = 0.6) +
  xlab("Trials") + 
  ylab("Accuracy") + 
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13)) + 
  theme(axis.title.y = element_text(size = 13))+
  theme(axis.text = element_text(size = 9, colour = "black")) +
  facet_wrap(~ id)


#plot frequencies of choices as barplot
require(scales)

sim_data$chosen_option <- sim_data$chosen_option + 1

plot_bar <-
  sim_data %>% 
  group_by(id, chosen_option)

plot_bar$chosen_option <- as.factor(plot_bar$chosen_option)

ggplot(plot_bar, aes(x = chosen_option, fill = chosen_option)) +
  geom_bar(aes(y = (..count..)/sum(..count..) * 100), color = "darkgrey", alpha = 0.8) +
  #facet_wrap( ~ id, nrow = 5, ncol = 10) +
  scale_fill_manual(values = c("tomato", "steelblue")) +
  xlab("Chosen option") +
  ylab("Percentage of choices") +
  scale_x_discrete(labels = c("bad", "good")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text = element_text(size = 13, colour = "black")) +
  theme(legend.position = "none")

#--------------------------------------------------------------------------------------------------------------------------------------

#read in simulated agents data weight RL model
sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_weight_6blocks_30trials.txt", 
                       " ", escape_double = FALSE, col_names = F, 
                       trim_ws = TRUE)


names(sim_data)[1] <- "id"
names(sim_data)[2] <- "block"
names(sim_data)[3] <- "trial"
names(sim_data)[4] <- "chosen_option"
names(sim_data)[5] <- "feedback"


param_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_weight_6blocks_30trials_parameters.txt", 
                       " ", escape_double = FALSE, col_names = F, 
                       trim_ws = TRUE)

names(param_data)[1] <- "id"
names(param_data)[2] <- "alpha_sim"
names(param_data)[3] <- "beta_sim"
names(param_data)[4] <- "weight_sim"


sim_data <-
  sim_data %>% 
  arrange(id)

sim_data_sum <- 
  sim_data %>% 
  group_by(id) %>% 
  summarize(accuracy = mean(chosen_option))

ggplot(data = sim_data_sum, aes(x = id, y = accuracy)) +
  geom_jitter(size = 2, alpha = 0.6) +
  xlab("simulated agent") +
  scale_y_continuous(breaks = seq(0.1, 1., 0.3)) +
  expand_limits(y = 1.0) +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  theme_classic()

mean(sim_data_sum$accuracy)


sim_data <- merge(sim_data, param_data, by = "id")

#plot accuracy collapsed over all ids over time (= trials)
plot_data2 <-
  sim_data %>% 
  group_by(id, trial, weight_sim) %>% 
  summarize(accuracy = mean(chosen_option))


ggplot(plot_data2, aes(x = trial, y = accuracy, color = weight_sim)) +
  geom_jitter(size = 1.5, width = 0.5, height = 0.1, alpha = 0.4) +
  geom_smooth(size = 1, color = "tomato", se = T, fill = "sienna", alpha = 0.4) +
  scale_color_gradient(low = "dodgerblue3", high = "sienna1", expression(paste("Simulated ", omega, " values"))) +
  xlab("Trials") + 
  ylab("Accuracy") + 
  scale_y_continuous(breaks = seq(0, 1.0, 0.25)) +
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13)) + 
  theme(axis.title.y = element_text(size = 13))+
  theme(axis.text = element_text(size = 11, colour = "black"))

ggplot(plot_data2, aes(x = trial, y = accuracy, color = weight_sim)) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_smooth(size = 0.7, color = "tomato", se = F, fill = "sienna1", alpha = 0.4) +
  scale_color_gradient(low = "dodgerblue3", high = "sienna1", expression(paste("Simulated ", omega, " values"))) +
  xlab("Trials") + 
  ylab("Accuracy") + 
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 12, colour = "black")) +
  facet_wrap(~ id)


#plot frequencies of choices as barplot
require(scales)

sim_data$chosen_option <- sim_data$chosen_option + 1

plot_bar <-
  sim_data %>% 
  group_by(id, chosen_option)

plot_bar$chosen_option <- as.factor(plot_bar$chosen_option)

ggplot(plot_bar, aes(x = chosen_option, fill = chosen_option)) +
  geom_bar(aes(y = (..count..)/sum(..count..) * 100), color = "darkgrey", alpha = 0.8) +
  #facet_wrap( ~ id, nrow = 5, ncol = 10) +
  scale_fill_manual(values = c("tomato", "steelblue")) +
  xlab("Chosen option") +
  ylab("Percentage of choices") +
  scale_x_discrete(labels = c("bad", "good")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text = element_text(size = 13, colour = "black")) +
  theme(legend.position = "none")
  
