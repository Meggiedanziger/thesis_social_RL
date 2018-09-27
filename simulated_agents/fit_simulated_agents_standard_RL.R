rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")
source("reinforcement_function.R")

#load required packages
library(tidyverse) 
library(readr)

#read in data
sim_data <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/.txt", 
             " ", col_names = F, 
             trim_ws = TRUE)

names(sim_data)[1] <- "id"
names(sim_data)[2] <- "block"
names(sim_data)[3] <- "trial"
names(sim_data)[4] <- "chosen_option"
names(sim_data)[5] <- "feedback"


sim_data <-
  sim_data %>% 
  arrange(id)


sim_data$chosen_option <- sim_data$chosen_option + 1

data <- sim_data
data <- as.data.frame(data)

subj = c(1:50)
FIT2 <- matrix(0, 50, 5)
#start a simplex search for finding the best parameter values
for (id in subj) {  #cycle through ids 1 to n
  startParm <- c(0.1, 0.1)
  names(startParm) <- c("alpha", "theta")
  out <- optim(startParm, reinforce, subj = id, method = "L-BFGS-B", 
               lower = c(.001, .001), upper = c(1, 1), data = data)
  FIT2[id, 1] <- out$value #column 1 contains log likelihood
  FIT2[id, 2:3] <- out$par #colomns 2 and 3 contain parameter values (alpha, beta)
  print(id)
}


# determine Model comparison criterion
# BIC deviance + parameters*log(N) #N = number of trials from all blocks
FIT2[, 4] <- FIT2[, 1] + 3*log(120);

#determine model comparison criterion
#BIC deviance + #parameters*log(N) #N = number of trials from all blocks
FIT2[, 4] <- FIT2[, 1] + 2*log(120);

#AIC: deviance + 2 * #parameters
FIT2[, 5] <- FIT2[, 1] + 2 * 2;


#sum of BIC values
sum(FIT2[, 4])


#create data frame for parameter recovery
modelfit_standard <- as.data.frame(FIT2)
names(modelfit_standard)[1] <- "LL"
names(modelfit_standard)[2] <- "alpha_fit"
names(modelfit_standard)[3] <- "beta_fit"  
names(modelfit_standard)[4] <- "BIC"
names(modelfit_standard)[5] <- "AIC"

#read in parameter data from simulation
parameter_sim <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/.txt", 
             " ", col_names = F, 
             trim_ws = TRUE)

names(parameter_sim)[1] <- "id"
names(parameter_sim)[2] <- "alpha_sim"
names(parameter_sim)[3] <- "beta_sim"


recovery_df <- cbind(modelfit_standard, parameter_sim)


cor.test(recovery_df$alpha_sim, recovery_df$alpha_fit)

ggplot(aes(x = alpha_fit, y = alpha_sim), data = recovery_df) +
  geom_point() +
  geom_smooth(method = "glm")


ggplot(aes(x = id, y = alpha_fit), data = recovery_df) +
  geom_point()

ggplot(aes(x = id, y = alpha_sim), data = recovery_df) +
  geom_point()

ggplot(aes(y = alpha_fit), data = recovery_df) +
  geom_boxplot()

ggplot(aes(y = alpha_sim), data = recovery_df) +
  geom_boxplot()


cor.test(recovery_df$beta_sim, recovery_df$beta_fit)

ggplot(aes(x = beta_sim, y = beta_fit), data = recovery_df) +
  geom_point() +
  geom_smooth(method = "glm")


ggplot(aes(x = id, y = beta_fit), data = recovery_df) +
  geom_point()

ggplot(aes(x = id, y = beta_sim), data = recovery_df) +
  geom_point()

ggplot(aes(y = beta_fit), data = recovery_df) +
  geom_boxplot()

ggplot(aes(y = beta_sim), data = recovery_df) +
  geom_boxplot()