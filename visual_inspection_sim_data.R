rm(list=ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git")

library(tidyverse)

#read in data
library(readr)
sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/simulation_beta_0.5.txt", 
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

#add columns for alpha and beta
obs <- c(1:4000)
temp  <- rep(5)/10
lrate <- rep(1:10, each = 400)/10

df <- cbind(obs, lrate, temp)
sim_data$obs <- obs
sim_data <- merge(sim_data, df)

#sim_data$chosen_option <- sim_data$chosen_option -1

sim_data_sum <- 
  sim_data %>% 
  group_by(id, lrate, temp) %>% 
  summarize(accuracy = mean(chosen_option))

ggplot(data = sim_data_sum, aes(x = id, y = accuracy)) +
  geom_jitter(size = 2, alpha = 0.6) +
  xlab("simulated agent") +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.1)) +
  expand_limits(y = 1.0) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_classic()

mean(sim_data_sum$accuracy)


#plot accuracy by lrate + temp
plot_data1 <-
  sim_data %>%
  group_by(id, lrate, temp) %>%
  summarize(mean_acc = mean(chosen_option))

#each parameter combination is one ppt with 48 trials
ggplot(plot_data1, aes(x = lrate, y = mean_acc)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth() +
  xlab("learning rate") +
  ylab("accuracy") +
  scale_x_continuous(breaks = seq(0, 1.0, 0.1))

#plot accuracy collapsed over all ids over time (= trials)
plot_data2 <-
  sim_data %>% 
  group_by(id, trial) %>% 
  summarize(accuracy = mean(chosen_option))

ggplot(plot_data2, aes(x = trial, y = accuracy)) +
  geom_jitter(size = 1.5, width = 0.6, height = 0.11, color = "blue", alpha = 0.6) +
  geom_smooth(color = "red", se = T, fill = "red", alpha = 0.3) +
  ylab("accuracy") + 
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  #scale_x_continuous(breaks = seq(1, , 1)) +
  theme_classic()


# ggplot(plot_data2, aes(x = trial, y = accuracy)) +
#   geom_point(color = "blue") +
#   geom_smooth(color = "red", se = T, fill = "red", alpha = 0.2) +
#   facet_wrap(~ id, nrow = 10, ncol = 10) +
#   scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
#   scale_x_continuous(breaks = seq(1, 12, 1)) +
#   theme_classic()


#plot frequencies of choices as barplot

require(scales)

sim_data$chosen_option <- sim_data$chosen_option +1

plot_bar <-
  sim_data %>% 
  group_by(id, chosen_option)
  # summarize(choice_cases = n()) %>% 
  # mutate(choices_per = choice_cases/sum(choice_cases))


ggplot(plot_bar, aes(x = chosen_option)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_wrap( ~ id, nrow = 2, ncol = 5) +
  xlab("chosen option") +
  ylab("percentage of choices") +
  scale_x_continuous(breaks = seq(1, 2, 1)) +
  #scale_y_continuous(breaks = seq(0, 100, 20)) +
  theme_classic()
