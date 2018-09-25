rm(list = ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")
source("reinforce_weight_reward.R")

#read in data
library(readr)


sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulation_weight_model_4blocks_30trials.txt", 
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

subj = c(1:250)
FIT2 <- matrix(0, 250, 6)

#start a simplex search for finding the best parameter values
for (id in subj) {  # cycle through ids 1 to n
  startParm <- c(0.1, 0.1, -0.9)
  names(startParm) <- c("alpha", "theta", "weight")
  out <- optim(startParm, reinforce_weight, subj = id, method = "L-BFGS-B", 
               lower = c(.001, .001, -0.9), upper = c(.9, .9, .9), data = data)
  FIT2[id, 1] <- out$value
  FIT2[id, 2:4] <- out$par
  print(id)
}


#determine model comparison criterion
#BIC deviance + parameters*log(N) #N = number of trials from all blocks
FIT2[, 5] <- FIT2[, 1] + 3*log(240);

#AIC: deviance + 2 * #parameters
FIT2[, 6] <- FIT2[, 1] + 2 * 3;


#sum of BIC values
sum(FIT2[, 5])


#create data frame for parameter recovery
modelfit_weight <- as.data.frame(FIT2)
names(modelfit_weight)[1] <- "LL"
names(modelfit_weight)[2] <- "alpha_fit"
names(modelfit_weight)[3] <- "beta_fit" 
names(modelfit_weight)[4] <- "weight_fit" 
names(modelfit_weight)[5] <- "BIC"
names(modelfit_weight)[6] <- "AIC"

#read in parameter data from  ex ante simulation
parameter_sim <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulation_parameters_weight_model_4blocks_30trials.txt", 
             " ", col_names = F, 
             trim_ws = TRUE)

names(parameter_sim)[1] <- "id"
names(parameter_sim)[2] <- "alpha_sim"
names(parameter_sim)[3] <- "beta_sim"
names(parameter_sim)[4] <- "weight_sim"


recovery_df <- cbind(modelfit_weight, parameter_sim)

corr_alpha <- cor.test(recovery_df$alpha_sim, recovery_df$alpha_fit)

recovery_alpha <-
  ggplot(aes(x = alpha_sim, y = alpha_fit, color = alpha_sim), data = recovery_df) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  xlab("Simulated alpha values") +
  ylab("Estimated alpha values") +
  theme_classic()


corr_beta <- cor.test(recovery_df$beta_sim, recovery_df$beta_fit)

recovery_beta <-
  ggplot(aes(x = beta_sim, y = beta_fit, color = beta_sim), data = recovery_df) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  xlab("Simulated beta values") +
  ylab("Estimated beta values") +
  theme_classic()


corr_weight <- cor.test(recovery_df$weight_sim, recovery_df$weight_fit)

recovery_weight <-
  ggplot(aes(x = weight_sim, y = weight_fit, color = weight_sim), data = recovery_df) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(-0.9, 0.9, 0.2)) +
  scale_x_continuous(breaks = seq(-0.9, 0.9, 0.2)) +
  xlab("Simulated weight values") +
  ylab("Estimated weight values") +
  theme_classic()

modelfit_weight <- write.table(recovery_df, file = "ex_ante_modelfit_weight_model_4blocks_30trials.txt", 
                                 row.names = FALSE, col.names = FALSE)


