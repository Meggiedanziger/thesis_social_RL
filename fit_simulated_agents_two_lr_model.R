rm(list = ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")
source("reinforce_two_learning_rates.R")

#read in data
library(readr)


sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulation_10_agents_2lr_model.txt", 
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
FIT2 <- matrix(0, 10, 6)
#start a simplex search for finding the best parameter values
for (id in subj) {  # cycle through ids 1 to n
  startParm <- c(0.1, 0.1, 0.1)
  names(startParm) <- c("alpha_ex", "alpha_in", "theta")
  out <- optim(startParm, reinforce2lrates, subj = id, method = "L-BFGS-B", 
               lower = c(.001, .001, .001), upper = c(1, 1, 1), data = data)
  FIT2[id, 1] <- out$value
  FIT2[id, 2:4] <- out$par
  print(id)
}


# determine Model comparison criterion
# BIC deviance + parameters*log(N) #N = number of trials from all blocks
FIT2[, 5] <- FIT2[, 1] + 3*log(400);

# AIC: deviance + 2 * #parameters
FIT2[, 6] <- FIT2[, 1] + 2 * 3;


# sum of BIC values
sum(FIT2[, 5])


#create data frame for parameter recovery
modelfit_weight <- as.data.frame(FIT2)
names(modelfit_weight)[1] <- "LL"
names(modelfit_weight)[2] <- "alpha_ex_fit"
names(modelfit_weight)[3] <- "alpha_in_fit"  
names(modelfit_weight)[4] <- "beta_fit" 
names(modelfit_weight)[5] <- "BIC"
names(modelfit_weight)[6] <- "AIC"

#read in parameter data from simulation
parameter_sim <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/parameter_values_simulation_2lr_model.txt", 
                            " ", col_names = F, 
                            trim_ws = TRUE)

names(parameter_sim)[1] <- "id"
names(parameter_sim)[2] <- "alpha_ex_sim"
names(parameter_sim)[3] <- "alpha_in_sim"
names(parameter_sim)[4] <- "beta_sim"

recovery_df <- cbind(modelfit_weight, parameter_sim)



cor.test(recovery_df$alpha_ex_sim, recovery_df$alpha_ex_fit)

ggplot(aes(x = alpha_ex_sim, y = alpha_ex_fit), data = recovery_df) +
  geom_point() +
  geom_smooth(method = "glm")


cor.test(recovery_df$alpha_in_sim, recovery_df$alpha_in_fit)

ggplot(aes(x = alpha_in_sim, y = alpha_in_fit), data = recovery_df) +
  geom_point() +
  geom_smooth(method = "glm")


cor.test(recovery_df$beta_sim, recovery_df$beta_fit)

ggplot(aes(x = beta_sim, y = beta_fit), data = recovery_df) +
  geom_point() +
  geom_smooth(method = "glm")
