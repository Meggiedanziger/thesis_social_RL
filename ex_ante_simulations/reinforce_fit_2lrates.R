rm(list = ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")
source("reinforce_2lrates.R")

#read in data
library(readr)


sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulations/ex_ante_simulation_2lrates_12blocks_30trials.txt", 
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

subj = c(1:125)
FIT2 <- matrix(0, 125, 6)
#start a simplex search for finding the best parameter values
for (id in subj) {  # cycle through ids 1 to n
  startParm <- c(0.1, 0.1, 0.1)
  names(startParm) <- c("alpha_ex", "alpha_in", "theta")
  out <- optim(startParm, reinforce2lrates, subj = id, method = "L-BFGS-B", 
               lower = c(.001, .001, .001), upper = c(.9, .9, .9), data = data)
  FIT2[id, 1] <- out$value
  FIT2[id, 2:4] <- out$par
  print(id)
}


#determine model comparison criterion
#BIC: deviance + parameters*log(N) #N = number of trials from all blocks
FIT2[, 5] <- FIT2[, 1] + 3*log(360);

#AIC: deviance + 2 * #parameters
FIT2[, 6] <- FIT2[, 1] + 2 * 3;


# sum of BIC values
sum(FIT2[, 5])

#create data frame for parameter recovery
modelfit_2lrates <- as.data.frame(FIT2)
names(modelfit_2lrates)[1] <- "LL"
names(modelfit_2lrates)[2] <- "alpha_ex_fit"
names(modelfit_2lrates)[3] <- "alpha_in_fit" 
names(modelfit_2lrates)[4] <- "beta_fit" 
names(modelfit_2lrates)[5] <- "BIC"
names(modelfit_2lrates)[6] <- "AIC"

#read in parameter data from  ex ante simulation
parameter_sim <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulations/ex_ante_simulation_parameters_2lrates_12blocks_30trials.txt", 
             " ", col_names = F, 
             trim_ws = TRUE)

names(parameter_sim)[1] <- "id"
names(parameter_sim)[2] <- "alpha_ex_sim"
names(parameter_sim)[3] <- "alpha_in_sim"
names(parameter_sim)[4] <- "beta_sim"


recovery_df <- cbind(modelfit_2lrates, parameter_sim)

corr_alpha_ex <- cor.test(recovery_df$alpha_ex_sim, recovery_df$alpha_ex_fit)

recovery_alpha_ex <-
  ggplot(aes(x = alpha_ex_sim, y = alpha_ex_fit, color = alpha_ex_sim), data = recovery_df) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  xlab("Simulated alpha values excluder") +
  ylab("Estimated alpha values excluder") +
  theme_classic()


corr_alpha_in <- cor.test(recovery_df$alpha_in_sim, recovery_df$alpha_in_fit)

recovery_alpha_in <-
  ggplot(aes(x = alpha_in_sim, y = alpha_in_fit, color = alpha_in_sim), data = recovery_df) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  xlab("Simulated alpha values includer") +
  ylab("Estimated alpha values includer") +
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


modelfit_2lrates <- write.table(recovery_df, file = "ex_ante_modelfit_2lrates_12blocks_30trials.txt", 
                                row.names = FALSE, col.names = FALSE)



