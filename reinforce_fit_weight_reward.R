rm(list = ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")
source("reinforce_weight_reward.R")

#read in data
library(readr)


sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/simulation_test_weight.txt", 
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
  names(startParm) <- c("alpha", "theta", "weight")
  out <- optim(startParm, reinforce_weight, subj = id, method = "L-BFGS-B", 
               lower = c(.001, .001, .001), upper = c(.3, .5, 1), data = data)
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


#recovery alpha
# alpha_sim <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
# alpha_fit <- FIT2[, 2]
# 
# df <- as.data.frame(cbind(alpha_fit, alpha_sim))
# cor.test(alpha_sim, alpha_fit)
# 
# ggplot(aes(x = alpha_sim, y = alpha_fit), data = df) +
#   geom_point() +
#   geom_smooth(method = "glm")

#recovery weight
weight_sim <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
weight_fit <- FIT2[, 4]

df <- as.data.frame(cbind(weight_fit, weight_sim))
cor.test(weight_sim, weight_fit)

ggplot(aes(x = weight_sim, y = weight_fit), data = df) +
  geom_point() +
  geom_smooth(method = "glm")


#write.table(FIT2, file = "modelfit_alpha_ex_0.4.txt", row.names = FALSE, col.names = FALSE)

