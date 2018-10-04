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
  geom_smooth(color = "tomato", se = T, fill = "sienna1", alpha = 0.6) +
  xlab("Trials") + 
  ylab("Accuracy") + 
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  #scale_x_continuous(breaks = seq(1, , 1)) +
  theme_classic()



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

#read in simulated agents data standard RL model
sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_weight_24blocks_30trials.txt", 
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
  scale_y_continuous(breaks = seq(0.1, 1., 0.3)) +
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
  geom_jitter(size = 1.5, width = 0.5, height = 0.05, color = "steelblue4", alpha = 0.4) +
  geom_smooth(color = "tomato", se = T, fill = "sienna1", alpha = 0.4) +
  xlab("Trials") + 
  ylab("Accuracy") + 
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  #scale_x_continuous(breaks = seq(1, , 1)) +
  theme_classic()



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
  
