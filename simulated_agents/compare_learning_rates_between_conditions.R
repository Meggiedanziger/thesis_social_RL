rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

#load required packages
library(tidyverse) 
library(readr)

################################ COMPARE LEARNING RATES AND TEMPERATURES OF NON-SOCIAL DATA FITTED WITH STANDARD RL MODEL
################################ LEARNING RATES AND TEMPERATURES OF SOCIAL DATA FITTED WITH WEIGHT RL MODEL
########################################################################################################################
#read in data RL non-social
fit_RL_6 <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standard_RL_18_30.txt",
             " ", col_names = F, trim_ws = TRUE)


names(fit_RL_6)[1] <- "LL"
names(fit_RL_6)[2] <- "alpha_fit"
names(fit_RL_6)[3] <- "beta_fit"
names(fit_RL_6)[4] <- "BIC"
names(fit_RL_6)[5] <- "AIC"
names(fit_RL_6)[6] <- "id"
names(fit_RL_6)[7] <- "alpha_sim"
names(fit_RL_6)[8] <- "beta_sim"


#read in data RLW social
fit_RLW_6_social <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_18_30_new.txt",
             " ", col_names = F, trim_ws = TRUE)


fit_RLW_6_social[11] <- log(fit_RLW_6_social[1]) + 3 * log(540)

names(fit_RLW_6_social)[1] <- "LL_RLW"
names(fit_RLW_6_social)[2] <- "alpha_fit_RLW"
names(fit_RLW_6_social)[3] <- "beta_fit_RLW"
names(fit_RLW_6_social)[4] <- "weight_fit_RLW"
names(fit_RLW_6_social)[5] <- "BIC_RLW"
names(fit_RLW_6_social)[6] <- "AIC_RLW"
names(fit_RLW_6_social)[7] <- "id_RLW"
names(fit_RLW_6_social)[8] <- "alpha_sim_RLW"
names(fit_RLW_6_social)[9] <- "beta_sim_RLW"
names(fit_RLW_6_social)[10] <- "weight_sim_RLW"

dat_compare <- cbind(fit_RLW_6_social, fit_RL_6)

#run Wilcoxon signed rank test (like paired t-test for non-normally dsitributed data) for alpha
nonpartest <- wilcox.test(dat_compare$alpha_fit, dat_compare$alpha_fit_RLW, paired = TRUE, exact = FALSE)
nonpartest

#calculate Z statistic
Z <- qnorm(nonpartest$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50)
effect

median(dat_compare$alpha_fit)
median(dat_compare$alpha_fit_RLW)


#run Wilcoxon signed rank test (like paired t-test for non-normally dsitributed data) for beta
nonpartest <- wilcox.test(dat_compare$beta_fit, dat_compare$beta_fit_RLW, paired = TRUE, exact = FALSE)
nonpartest

#calculate Z statistic
Z <- qnorm(nonpartest$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50)
effect

median(dat_compare$beta_fit)
median(dat_compare$beta_fit_RLW)


#plot alpha and beta for both conditions in boxplot

ggplot(dat_compare) +
  geom_boxplot(aes(x = 1, y = alpha_fit), fill = "tomato", alpha = 0.4) +
  geom_boxplot(aes(x = 2, y = alpha_fit_RLW), fill = "steelblue4", alpha = 0.4) +
  scale_x_continuous(breaks = c(1, 2), labels = c("non-social", "social")) +
  scale_y_continuous(breaks = seq(0, 1.0, .25)) +
  xlab("Condition") + 
  ylab(expression(paste("Fitted ", alpha, " values"))) + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text = element_text(size = 13, colour = "black"))

ggplot(dat_compare) +
  geom_boxplot(aes(x = 1, y = beta_fit), fill = "tomato", alpha = 0.4) +
  geom_boxplot(aes(x = 2, y = beta_fit_RLW), fill = "steelblue4", alpha = 0.4) +
  scale_x_continuous(breaks = c(1, 2), labels = c("non-social", "social")) +
  scale_y_continuous(breaks = seq(0, 10, 2.5)) +
  xlab("Condition") + 
  ylab(expression(paste("Fitted ", beta, " values"))) + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text = element_text(size = 13, colour = "black"))
  
  
  
  
  
  
