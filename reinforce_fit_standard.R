rm(list=ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")
source("reinforce_standard.R")

library("colorspace") 

#read in data
library(readr)


sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulation_standard_RL_12blocks_30trials_unbounded_beta.txt", 
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

subj = c(1:80)
FIT2 <- matrix(0, 80, 5)
#start a simplex search for finding the best parameter values
for (id in subj) {  # cycle through ids 1 to n
  startParm <- c(0.1, 0.1)
  names(startParm) <- c("alpha", "theta")
  out <- optim(startParm, reinforce, subj = id, method = "L-BFGS-B", 
               lower = c(.001, .001), upper = c(1, 10), data = data)
  FIT2[id, 1] <- out$value
  FIT2[id, 2:3] <- out$par
  print(id)
}


#determine model comparison criterion
#BIC deviance + parameters*log(N) #N = number of trials
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

#read in parameter data from  ex ante simulation
parameter_sim <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulation_parameters_standard_RL_12blocks_30trials_unbounded_beta.txt", 
             " ", col_names = F, trim_ws = TRUE)

names(parameter_sim)[1] <- "id"
names(parameter_sim)[2] <- "alpha_sim"
names(parameter_sim)[3] <- "beta_sim"


recovery_df <- cbind(modelfit_standard, parameter_sim)

corr_alpha <- cor.test(recovery_df$alpha_sim, recovery_df$alpha_fit)

recovery_alpha <-
  ggplot(aes(x = alpha_sim, y = alpha_fit, color = alpha_sim), data = recovery_df) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1.0, 0.2)) +
  xlab("Simulated alpha values") +
  ylab("Estimated alpha values") +
  theme_classic()


corr_beta <- cor.test(recovery_df$beta_sim, recovery_df$beta_fit)

recovery_beta <-
  ggplot(aes(x = beta_sim, y = beta_fit, color = beta_sim), data = recovery_df) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0, 15, 3)) +
  scale_x_continuous(breaks = seq(0, 15, 3)) +
  xlab("Simulated beta values") +
  ylab("Estimated beta values") +
  theme_classic()


#modelfit_standard <- write.table(recovery_df, file = "ex_ante_modelfit_standard_RL_12blocks_30trials.txt", 
 #                               row.names = FALSE, col.names = FALSE)

modelfit_unbounded_beta <- write.table(recovery_df, file = "ex_ante_modelfit_standard_RL_12blocks_30trials_unbounded_beta.txt", 
                                       row.names = FALSE, col.names = FALSE)
