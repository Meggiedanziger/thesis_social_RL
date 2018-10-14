rm(list=ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")


############ LOGISTIC REGRESSION #############


library(tidyverse)
library(lme4)
library(arm)

#read in data
library(readr)

#read in simulated agents data standard RL model
sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_standard_RL_6_30.txt", 
                       " ", escape_double = FALSE, col_names = F, 
                       trim_ws = TRUE)

names(sim_data)[1] <- "id"
names(sim_data)[2] <- "block"
names(sim_data)[3] <- "trial"
names(sim_data)[4] <- "chosen_option"
names(sim_data)[5] <- "feedback"

#z-transform trial
#z-transform block
#0 = excluder
#1 = includer

#z-transformation of trial
sim_data_trial_max <-
  sim_data %>%
  group_by(id, block) %>% 
  mutate(trial_max = max(trial))

sim_data_trial_mean <-
  sim_data_trial_max %>%
  group_by(id) %>% 
  mutate(mean_trial = mean(trial))

sim_data_trial_sd <-
  sim_data_trial_mean %>%
  group_by(id) %>% 
  mutate(sd_trial = sd(trial))

sim_data_trial_z <-
  sim_data_trial_sd %>%
  group_by(id) %>% 
  mutate(trial_z = (trial_max - mean_trial) / sd_trial)

#z-transformation of block
sim_data_trial_z <-
  sim_data_trial_z %>%
  group_by(id) %>% 
  mutate(mean_block = mean(block))

sim_data_trial_z <-
  sim_data_trial_z %>%
  group_by(id) %>% 
  mutate(sd_block = sd(block))

sim_data_trial_z <-
  sim_data_trial_z %>%
  group_by(id) %>% 
  mutate(block_z = (block - mean_block) / sd_block) 

regr_data <- sim_data_trial_z

regr_data$chosen_option <- as.factor(regr_data$chosen_option)


regr_data$id <- as.numeric(regr_data$id)
is.factor(regr_data$id)

#perform logistic regression 
#AV (1 = includer, 0 = excluder) 
#UV: block, trial, gender_excluder, gender_ppt
fit <- glmer(chosen_option ~ block_z + 
            (1|id), 
            data = regr_data, 
            family = binomial)

summary(fit)

