rm(list=ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_post_simulations")

library(tidyverse)
library(lme4)
library(reshape)

#read in data
library(readr)

#read in simulated agents data standard RL model
sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_post_simulations/simulation_weight_18_30.txt", 
                       " ", escape_double = FALSE, col_names = F, 
                       trim_ws = TRUE)

names(sim_data)[1] <- "id"
names(sim_data)[2] <- "block"
names(sim_data)[3] <- "trial"
names(sim_data)[4] <- "chosen_option"
names(sim_data)[5] <- "feedback"

sim_data$condition <- 1

sim_data <-
  sim_data %>% 
  arrange(id)

sim_data_sum <- 
  sim_data %>% 
  group_by(id, trial, condition) %>% 
  summarize(accuracy = mean(chosen_option))

ggplot(data = sim_data_sum, aes(x = id, y = accuracy)) +
  geom_jitter(size = 2, alpha = 0.3) +
  xlab("simulated agent") +
  scale_y_continuous(breaks = seq(0.1, 1.0, 0.2)) +
  expand_limits(y = 1.0) +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  theme_classic()

ggplot(aes(x = id, group = id, y = accuracy), data = sim_data_sum) +
  geom_boxplot()

mean(sim_data_sum$accuracy)

sim_data_social <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_post_simulations/simulation_standard_18_30.txt", 
                              " ", escape_double = FALSE, col_names = F, 
                              trim_ws = TRUE)

names(sim_data_social)[1] <- "id"
names(sim_data_social)[2] <- "block"
names(sim_data_social)[3] <- "trial"
names(sim_data_social)[4] <- "chosen_option"
names(sim_data_social)[5] <- "feedback"

sim_data_social$condition <- 2

sim_data_social <-
  sim_data_social %>% 
  arrange(id)



sim_data_social_sum <- 
  sim_data_social %>% 
  group_by(id, trial, condition) %>% 
  summarize(accuracy_social = mean(chosen_option))

ggplot(data = sim_data_social_sum, aes(x = id, y = accuracy_social)) +
  geom_jitter(size = 2, alpha = 0.3) +
  xlab("simulated agent") +
  scale_y_continuous(breaks = seq(0.1, 1.0, 0.2)) +
  expand_limits(y = 1.0) +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  theme_classic()

ggplot(aes(x = id, group = id, y = accuracy_social), data = sim_data_social_sum) +
  geom_boxplot()

mean(sim_data_social_sum$accuracy_social)

dat <- cbind(sim_data_sum, sim_data_social_sum)

hist(dat$accuracy)
hist(dat$accuracy_social)

#run Wilcoxon signed rank test (like paired t-test for non-normally dsitributed data)
nonpartest <- wilcox.test(dat$accuracy, dat$accuracy_social, paired = TRUE, exact = T)
nonpartest

#calculate Z statistic
Z <- qnorm(0.00000000000000011/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(100) 
effect

median(dat$accuracy)
median(dat$accuracy_social)
