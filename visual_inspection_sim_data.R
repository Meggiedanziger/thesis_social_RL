rm(list=ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")

library(tidyverse)

#read in data
library(readr)

sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulations/simulation_weight_model_24blocks_30trials.txt", 
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
#obs <- c(1:4000)
# temp  <- rep(c(1, 4, 7, 10, 30, 50, 70, 100), each = 7200)/10
# lrate <- rep(1:10, each = 720)/10
# 
# df <- cbind(lrate, temp)
# #sim_data$obs <- obs
# sim_data <- cbind(sim_data, df)


temp   <- rep(c(1, 5, 10, 50, 100), each = 3600)/10
lrate  <- rep(seq(1, 9, 2), each = 720)/10
weight <- rep(seq(from = -9, to = 9, 2), each = 18000)/10

df <- cbind(lrate, temp, weight)
sim_data <- cbind(sim_data, df)

#sim_data$chosen_option <- sim_data$chosen_option -1

sim_data_sum <- 
  sim_data %>% 
  group_by(id) %>% 
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
  group_by(id, lrate, temp, weight) %>%
  summarize(mean_acc = mean(chosen_option))

#each parameter combination is one ppt with 48 trials
ggplot(plot_data1, aes(x = lrate, y = mean_acc)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth() +
  xlab("learning rate") +
  ylab("accuracy") +
  scale_x_continuous(breaks = seq(0, 1.0, 0.1))

ggplot(plot_data1, aes(x = temp, y = mean_acc)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth() +
  xlab("temperature") +
  ylab("accuracy") +
  scale_x_continuous(breaks = seq(0, 10, 2))

ggplot(plot_data1, aes(x = weight, y = mean_acc)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth() +
  xlab("weight") +
  ylab("accuracy") +
  scale_x_continuous(breaks = seq(-.9, .9, .2))

#plot accuracy collapsed over all ids over time (= trials)
plot_data2 <-
  sim_data %>% 
  group_by(id, trial, lrate, temp, weight) %>% 
  summarize(accuracy = mean(chosen_option))

ggplot(plot_data2, aes(x = trial, y = accuracy)) +
  geom_jitter(size = 2, width = 0.5, height = 0.03, color = "steelblue4", alpha = 0.4) +
  geom_smooth(size = 1.2, color = "tomato", se = T, fill = "sienna", alpha = 0.5) +
  ylab("accuracy") + 
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  #scale_x_continuous(breaks = seq(1, , 1)) +
  theme_classic()


plot_data2$weight_f <- factor(plot_data2$weight, levels = c(-.9, -.7, -.5, -.3, -.1, .9, .7, .5, .3, .1))

plot_data2$weight_f2 <- factor(plot_data2$weight, levels = c(-.9, .9, -.7, .7, -.5, .5, -.3, .3, -.1, .1))


ggplot(plot_data2, aes(x = trial, y = accuracy)) +
  geom_jitter(size = 1, width = 0.5, height = 0.1, color = "steelblue4", alpha = 0.4) +
  #geom_point(color = "steelblue4", alpha = 0.5) +
  geom_smooth(size = 0.9, color = "tomato", se = F, fill = "red", alpha = 0.2) +
  facet_wrap(~ weight_f2, ncol = 2, nrow = 5) +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.5)) +
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  ylab("Accuracy") +
  xlab("Trials") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.title.x = element_text(size = 12)) + 
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 11, colour = "black"))


# alpha_names <- c(
#   '0.9' = "lala",
#   '0.7' = alpha, "= 0.7",
#   '0.5' = alpha, "= 0.5",
#   '0.3' = alpha, "= 0.3",
#   '0.1' = alpha, "= 0.1"
# )

plot_data2$lrate <- factor(plot_data2$lrate, labels = c("alpha = 0.1", 
                                                        "alpha = 0.3", "alpha = 0.5", "alpha = 0.7", "alpha = 0.9"))

ggplot(plot_data2, aes(x = trial, y = accuracy)) +
  geom_jitter(size = 1, width = 0.5, height = 0.1, color = "steelblue4", alpha = 0.4) +
  #geom_point(color = "steelblue4", alpha = 0.5) +
  geom_smooth(size = 0.9, color = "tomato", se = F, fill = "red", alpha = 0.2) +
  facet_wrap(~ lrate, ncol = 1) +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.5)) +
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  ylab("Accuracy") +
  xlab("Trials") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.title.x = element_text(size = 12)) + 
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 11, colour = "black"))


ggplot(plot_data2, aes(x = trial, y = accuracy)) +
  geom_jitter(size = 1, width = 0.5, height = 0.1, color = "steelblue4", alpha = 0.4) +
  #geom_point(color = "steelblue4", alpha = 0.5) +
  geom_smooth(size = 0.9, color = "tomato", se = F, fill = "red", alpha = 0.2) +
  facet_wrap(~ temp, ncol = 1, nrow = 5) +
  scale_y_continuous(breaks = seq(0.5, 1.0, 0.5)) +
  scale_x_continuous(breaks = seq(5, 30, 10)) +
  ylab("Accuracy") +
  xlab("Trials") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.title.x = element_text(size = 12)) + 
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 11, colour = "black"))
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
  #facet_wrap( ~ id, nrow = 2, ncol = 5) +
  xlab("chosen option") +
  ylab("percentage of choices") +
  scale_x_continuous(breaks = seq(1, 2, 1)) +
  #scale_y_continuous(breaks = seq(0, 100, 20)) +
  theme_classic()
