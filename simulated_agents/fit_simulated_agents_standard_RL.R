rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")
source("reinforce_standard.R")

#load required packages
library(tidyverse) 
library(readr)

#read in data
sim_data <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_weight_18_30_40ppts.txt", 
             " ", col_names = F, trim_ws = TRUE)

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

subj = c(1:40)
FIT2 <- matrix(0, 40, 5)
#start a simplex search for finding the best parameter values
for (id in subj) {  #cycle through ids 1 to n
  startParm <- c(0.1, 0.1)
  names(startParm) <- c("alpha", "theta")
  out <- optim(startParm, reinforce, subj = id, method = "L-BFGS-B", 
               lower = c(.001, .001), upper = c(1, 10), data = data)
  FIT2[id, 1] <- out$value #column 1 contains log likelihood
  FIT2[id, 2:3] <- out$par #colomns 2 and 3 contain parameter values (alpha, beta)
  print(id)
}


#determine model comparison criterion
#BIC deviance + #parameters*log(N) #N = number of trials from all blocks
FIT2[, 4] <- FIT2[, 1] + 2*log(540);

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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_weight_parameters_18_30_40ppts.txt", 
             " ", col_names = F, 
             trim_ws = TRUE)

names(parameter_sim)[1] <- "id"
names(parameter_sim)[2] <- "alpha_sim"
names(parameter_sim)[3] <- "beta_sim"


recovery_df <- cbind(modelfit_standard, parameter_sim)

corr_alpha <- cor.test(recovery_df$alpha_sim, recovery_df$alpha_fit)
corr_alpha

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
recovery_alpha

corr_beta <- cor.test(recovery_df$beta_sim, recovery_df$beta_fit)
corr_beta

recovery_beta <-
  ggplot(aes(x = beta_sim, y = beta_fit, color = beta_sim), data = recovery_df) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "glm", color = "darkgrey", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  xlab("Simulated beta values") +
  ylab("Estimated beta values") +
  theme_classic()
recovery_beta

setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")
modelfit_standard <- write.table(recovery_df, file = "modelfit_agents_standardtoweight_18_30_40ppts.txt", 
                                 row.names = FALSE, col.names = FALSE)


#############Parameter gegeneinander plotten
#############Boxplots der gefitteten Parameter
#############Boxplots fit und sim vergleichen

