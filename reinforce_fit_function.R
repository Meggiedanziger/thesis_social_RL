rm(list=ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git")
source("reinforcement_function.R")

#read in data
library(readr)

sim_data <- read_delim("~/Dropbox/___MA/RL_model/simulation_beta_0.5.txt", 
                       " ", col_names = F, 
                       trim_ws = TRUE)

names(sim_data)[1] <- "id"
names(sim_data)[2] <- "block"
names(sim_data)[3] <- "trial"
names(sim_data)[4] <- "chosen_option"
names(sim_data)[5] <- "feedback"

library(tidyverse)  

sim_data <-
  sim_data %>% 
  arrange(id)

sim_data$chosen_option <- sim_data$chosen_option + 1

data <- sim_data
data <- as.data.frame(data)

subj = c(1:10)
FIT2 <- matrix(0, 10, 5)
#start a simplex search for finding the best parameter values
for (id in subj) {  # cycle through ids 1 to n
  startParm <- c(0.1, 0.1)
  names(startParm) <- c("alpha", "theta")
  out <- optim(startParm, reinforce, subj = id, method = "L-BFGS-B", lower = c(.001, .499), upper = c(1, .5), data = data)
  FIT2[id, 1] <- out$value
  FIT2[id, 2:3] <- out$par
}


# determine Model comparison criterion
# BIC deviance + parameters*log(N) #N = number of trials
FIT2[, 4] <- FIT2[, 1] + 2*log(48);

# AIC: deviance + 2 * #parameters
FIT2[, 5] <- FIT2[, 1] + 2 * 2;


# sum of BIC values
sum(FIT2[, 4])

#recovery alpha
alpha_sim <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
alpha_fit <- FIT2[, 2]

df <- as.data.frame(cbind(alpha_fit, alpha_sim))
cor.test(alpha_sim, alpha_fit)

ggplot(aes(x = alpha_sim, y = alpha_fit), data = df) +
  geom_point() +
  geom_smooth(method = "glm")



#check reward probabilities from simulation function
sum(sim_data$chosen_option == 2) # good option
sum(sim_data$chosen_option == 2 & sim_data$feedback == 10) # good option with positive feedback
sum(sim_data$chosen_option == 2 & sim_data$feedback == -10) # good option with negative feedback

sum(sim_data$chosen_option == 1) # bad option
sum(sim_data$chosen_option == 1 & sim_data$feedback == 10) # bad option with positive feedback
sum(sim_data$chosen_option == 1 & sim_data$feedback == -10) # bad option with negative feedback

