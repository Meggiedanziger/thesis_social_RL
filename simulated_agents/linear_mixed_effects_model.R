rm(list=ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

library(tidyverse)
library(lme4)

#read in data
library(readr)

#read in simulated agents data standard RL model
sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_standard_RL_24_30.txt", 
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

sim_data_social <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_weight_24_30.txt", 
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
  summarize(accuracy = mean(chosen_option))

ggplot(data = sim_data_social_sum, aes(x = id, y = accuracy)) +
  geom_jitter(size = 2, alpha = 0.3) +
  xlab("simulated agent") +
  scale_y_continuous(breaks = seq(0.1, 1.0, 0.2)) +
  expand_limits(y = 1.0) +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  theme_classic()

ggplot(aes(x = id, group = id, y = accuracy), data = sim_data_social_sum) +
  geom_boxplot()

mean(sim_data_social_sum$accuracy)

model <- 
  lmer(accuracy ~ trial + (1|id), data = sim_data_sum, REML = F)

model_null <- 
  lmer(accuracy ~ 1 + (1|id), data = sim_data_sum, REML = F)

model_ranslope <- 
  lmer(accuracy ~ trial + (1 + trial|id), data = sim_data_sum, REML = F)

summary(model)
coef(model)

summary(model_null)
coef(model_null)

aov <- anova(model_null, model)
summary(aov)

summary(model_ranslope)
coef(model_ranslope)






#dat <- cbind(sim_data_sum, sim_data_social_sum)

