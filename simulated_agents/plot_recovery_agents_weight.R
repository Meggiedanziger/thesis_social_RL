rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

########################### PARAMETER RECOVERY SIMULATED AGENTS WEIGHT RL MODEL 6 BLOCKS 30 TRIALS
####################################################################################################

#read in ex ante fitted data
modelfit <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_6blocks_30trials.txt", 
             " ", col_names = F, trim_ws = TRUE)

names(modelfit)[1] <- "LL"
names(modelfit)[2] <- "alpha_fit"
names(modelfit)[3] <- "beta_fit" 
names(modelfit)[4] <- "weight_fit"
names(modelfit)[5] <- "BIC"
names(modelfit)[6] <- "AIC"
names(modelfit)[7] <- "id"
names(modelfit)[8] <- "alpha_sim"
names(modelfit)[9] <- "beta_sim"
names(modelfit)[10] <- "weight_sim"

corr_alpha <- cor.test(modelfit$alpha_sim, modelfit$alpha_fit)
corr_alpha

recovery_alpha <-
  ggplot(aes(x = alpha_sim, y = alpha_fit, color = alpha_sim), data = modelfit) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "glm", color = "gray31", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "red", expression(paste("Simulated ", alpha, " values"))) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.2)) +
  scale_x_continuous(breaks = seq(0, 1.0, 0.2)) +
  xlab(expression(paste("Simulated ", alpha, " values"))) +
  ylab(expression(paste("Estimated ", alpha, " values"))) +
  annotate("text", x = 0.7, y = 0.02, label = "italic(r) == .76", parse = T, size = 4) +
  theme_classic()
recovery_alpha

corr_beta <- cor.test(modelfit$beta_sim, modelfit$beta_fit)
corr_beta

recovery_beta <-
  ggplot(aes(x = beta_sim, y = beta_fit, color = beta_sim), data = modelfit) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "glm", color = "gray31", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "dodgerblue3", high = "limegreen", expression(paste("Simulated ", beta, " values"))) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  xlab(expression(paste("Simulated ", beta, " values"))) +
  ylab(expression(paste("Estimated ", beta, " values"))) +
  annotate("text", x = 5, y = 1, label = "italic(r) == .40", parse = T, size = 4) +
  theme_classic()
recovery_beta

corr_weight <- cor.test(modelfit$weight_sim, modelfit$weight_fit)
corr_weight

recovery_weight <-
  ggplot(aes(x = weight_sim, y = weight_fit, color = weight_sim), data = modelfit) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "glm", color = "gray31", se = F, fill = "red", alpha = 0.2) +
  scale_color_gradient(low = "blue", high = "sienna1", expression(paste("Simulated ", omega, " values"))) +
  scale_y_continuous(breaks = seq(-1, 1.0, 0.2)) +
  scale_x_continuous(breaks = seq(-1, 1.0, 0.2)) +
  xlab(expression(paste("Simulated ", omega, " values"))) +
  ylab(expression(paste("Estimated ", omega, " values"))) +
  annotate("text", x = 0.75, y = -0.9, label = "italic(r) == .83", parse = T, size = 4) +
  theme_classic()
recovery_weight
