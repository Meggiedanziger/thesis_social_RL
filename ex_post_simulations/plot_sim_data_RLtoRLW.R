rm(list=ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_post_simulations")

library(tidyverse)

########### 18 BLOCKS

#read in data
library(readr)

#read in simulated agents data standard RL model
sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_weight_18_30_new.txt", 
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
  scale_y_continuous(breaks = seq(0, 1.0, 0.3)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 13, colour = "black"))

# ggplot(plot_data2, aes(x = trial, y = accuracy)) +
#   geom_jitter(size = 1.5, width = 0.5, height = 0.1, color = "steelblue4", alpha = 0.4) +
#   geom_smooth(size = 0.7, color = "tomato", se = F, fill = "sienna1", alpha = 0.6) +
#   xlab("Trials") + 
#   ylab("Accuracy") + 
#   scale_x_continuous(breaks = seq(5, 30, 10)) +
#   scale_y_continuous(breaks = seq(0, 1.0, 0.5)) +
#   theme_classic() +
#   theme(axis.title.x = element_text(size = 14)) + 
#   theme(axis.title.y = element_text(size = 14))+
#   theme(axis.text = element_text(size = 13, colour = "black")) +
#   facet_wrap(~ id, ncol = 10, nrow = 5)



#read in ex post simulated data standard RL model
sim_post <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_post_simulations/simulation_standardtoweight_18_30.txt", 
                       " ", escape_double = FALSE, col_names = F, 
                       trim_ws = TRUE)

names(sim_post)[1] <- "id_post"
names(sim_post)[2] <- "block_post"
names(sim_post)[3] <- "trial_post"
names(sim_post)[4] <- "chosen_option_post"
names(sim_post)[5] <- "feedback_post"


sim_post <-
  sim_post %>% 
  arrange(id_post)

sim_post_sum <- 
  sim_post %>% 
  group_by(id_post) %>% 
  summarize(accuracy_post = mean(chosen_option_post))

ggplot(data = sim_post_sum, aes(x = id_post, y = accuracy_post)) +
  geom_jitter(size = 2, alpha = 0.6) +
  xlab("simulated agent") +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.1)) +
  expand_limits(y = 1.0) +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  theme_classic()

mean(sim_post_sum$accuracy_post)


#plot accuracy collapsed over all ids over time (= trials)
plot_data_post <-
  sim_post %>% 
  group_by(id_post, trial_post) %>% 
  summarize(accuracy_post = mean(chosen_option_post))

ggplot(plot_data_post, aes(x = trial_post, y = accuracy_post)) +
  geom_jitter(size = 1.5, width = 0.5, height = 0.1, color = "steelblue4", alpha = 0.4) +
  geom_smooth(size = 1, color = "tomato", se = T, fill = "sienna", alpha = 0.6) +
  xlab("Trials") + 
  ylab("Accuracy") +
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.3)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 13, colour = "black"))

# ggplot(plot_data_post, aes(x = trial_post, y = accuracy_post)) +
#   geom_point(size = 1.2, color = "steelblue4", alpha = 0.7) +
#   geom_smooth(size = 0.7, color = "tomato", se = F, fill = "sienna1", alpha = 0.6) +
#   xlab("Trials") + 
#   ylab("Accuracy") + 
#   scale_x_continuous(breaks = seq(5, 30, 10)) +
#   scale_y_continuous(breaks = seq(0, 1.0, 0.5)) +
#   theme_classic() +
#   theme(axis.title.x = element_text(size = 14)) + 
#   theme(axis.title.y = element_text(size = 14))+
#   theme(axis.text = element_text(size = 13, colour = "black")) +
#   facet_wrap(~ id_post, ncol = 10, nrow = 5)

dat <- cbind(plot_data2, plot_data_post)

ggplot(dat, aes(x = trial_post, y = accuracy), color = accuracy) +
  #geom_point(size = 1.2, color = "steelblue4", alpha = 0.7) +
  geom_smooth(aes(x = trial_post, y = accuracy), size = 0.7, color = "tomato", se = T, fill = "sienna", alpha = 0.4) +
  geom_smooth(aes(x = trial_post, y = accuracy_post), size = 0.7, color = "steelblue", se = T, fill = "sienna", alpha = 0.4) +
  xlab("Trials") + 
  ylab("Accuracy") + 
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 13, colour = "black"))


ggplot(dat, aes(x = trial_post, y = accuracy), color = accuracy) +
  #geom_point(size = 1.2, color = "steelblue4", alpha = 0.7) +
  geom_smooth(aes(x = trial_post, y = accuracy), size = 0.9, color = "tomato", se = F, fill = "sienna", alpha = 0.2) +
  geom_smooth(aes(x = trial_post, y = accuracy_post), size = 0.9, color = "steelblue", se = F, fill = "sienna", alpha = 0.2) +
  xlab("Trials") + 
  ylab("Accuracy") + 
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 13, colour = "black")) +
  facet_wrap(~ id, ncol = 10, nrow = 5)



#--------------------------------------------------------------------------------------------------------------------------------------

########### 24 BLOCKS

#read in simulated agents data standard RL model
sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_weight_24_30_new.txt", 
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
  scale_y_continuous(breaks = seq(0, 1.0, 0.3)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 13, colour = "black"))

# ggplot(plot_data2, aes(x = trial, y = accuracy)) +
#   geom_jitter(size = 1.5, width = 0.5, height = 0.1, color = "steelblue4", alpha = 0.4) +
#   geom_smooth(size = 0.7, color = "tomato", se = F, fill = "sienna1", alpha = 0.6) +
#   xlab("Trials") + 
#   ylab("Accuracy") + 
#   scale_x_continuous(breaks = seq(5, 30, 10)) +
#   scale_y_continuous(breaks = seq(0, 1.0, 0.5)) +
#   theme_classic() +
#   theme(axis.title.x = element_text(size = 14)) + 
#   theme(axis.title.y = element_text(size = 14))+
#   theme(axis.text = element_text(size = 13, colour = "black")) +
#   facet_wrap(~ id, ncol = 10, nrow = 5)



#read in ex post simulated data standard RL model
sim_post <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_post_simulations/simulation_weight_24_30.txt", 
                       " ", escape_double = FALSE, col_names = F, 
                       trim_ws = TRUE)

names(sim_post)[1] <- "id_post"
names(sim_post)[2] <- "block_post"
names(sim_post)[3] <- "trial_post"
names(sim_post)[4] <- "chosen_option_post"
names(sim_post)[5] <- "feedback_post"


sim_post <-
  sim_post %>% 
  arrange(id_post)

sim_post_sum <- 
  sim_post %>% 
  group_by(id_post) %>% 
  summarize(accuracy_post = mean(chosen_option_post))

ggplot(data = sim_post_sum, aes(x = id_post, y = accuracy_post)) +
  geom_jitter(size = 2, alpha = 0.6) +
  xlab("simulated agent") +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.1)) +
  expand_limits(y = 1.0) +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  theme_classic()

mean(sim_post_sum$accuracy_post)


#plot accuracy collapsed over all ids over time (= trials)
plot_data_post <-
  sim_post %>% 
  group_by(id_post, trial_post) %>% 
  summarize(accuracy_post = mean(chosen_option_post))

ggplot(plot_data_post, aes(x = trial_post, y = accuracy_post)) +
  geom_jitter(size = 1.5, width = 0.5, height = 0.1, color = "steelblue4", alpha = 0.4) +
  geom_smooth(size = 1, color = "tomato", se = T, fill = "sienna", alpha = 0.6) +
  xlab("Trials") + 
  ylab("Accuracy") +
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.3)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 13, colour = "black"))

# ggplot(plot_data_post, aes(x = trial_post, y = accuracy_post)) +
#   geom_point(size = 1.2, color = "steelblue4", alpha = 0.7) +
#   geom_smooth(size = 0.7, color = "tomato", se = F, fill = "sienna1", alpha = 0.6) +
#   xlab("Trials") + 
#   ylab("Accuracy") + 
#   scale_x_continuous(breaks = seq(5, 30, 10)) +
#   scale_y_continuous(breaks = seq(0, 1.0, 0.5)) +
#   theme_classic() +
#   theme(axis.title.x = element_text(size = 14)) + 
#   theme(axis.title.y = element_text(size = 14))+
#   theme(axis.text = element_text(size = 13, colour = "black")) +
#   facet_wrap(~ id, ncol = 10, nrow = 5)

dat <- cbind(plot_data2, plot_data_post)

ggplot(dat, aes(x = trial_post, y = accuracy), color = accuracy) +
  #geom_point(size = 1.2, color = "steelblue4", alpha = 0.7) +
  geom_smooth(aes(x = trial_post, y = accuracy), size = 0.7, color = "tomato", se = T, fill = "sienna", alpha = 0.4) +
  geom_smooth(aes(x = trial_post, y = accuracy_post), size = 0.7, color = "steelblue", se = T, fill = "sienna", alpha = 0.4) +
  xlab("Trials") + 
  ylab("Accuracy") + 
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 13, colour = "black"))


ggplot(dat, aes(x = trial_post, y = accuracy), color = accuracy) +
  #geom_point(size = 1.2, color = "steelblue4", alpha = 0.7) +
  geom_smooth(aes(x = trial_post, y = accuracy), size = 0.7, color = "tomato", se = T, fill = "sienna", alpha = 0.2) +
  geom_smooth(aes(x = trial_post, y = accuracy_post), size = 0.7, color = "steelblue", se = T, fill = "sienna", alpha = 0.2) +
  xlab("Trials") + 
  ylab("Accuracy") + 
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 13, colour = "black")) +
  facet_wrap(~ id, ncol = 10, nrow = 5)

