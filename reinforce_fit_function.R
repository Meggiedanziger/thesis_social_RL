rm(list=ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")
source("reinforcement_function.R")

#read in data
library(readr)


sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/simulation_beta_0.5.txt", 
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
  out <- optim(startParm, reinforce, subj = id, method = "L-BFGS-B", 
               lower = c(.001, .001), upper = c(1, .5), data = data)
  FIT2[id, 1] <- out$value
  FIT2[id, 2:3] <- out$par
  print(id)
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

write.table(FIT2, file = "modelfit_beta_0.5.txt", row.names = FALSE, col.names = FALSE)

modelfit <- as.data.frame(FIT2)

fit <- write.table(modelfit, file = "fit_beta_0.5.txt", 
                        row.names = FALSE, col.names = FALSE)
