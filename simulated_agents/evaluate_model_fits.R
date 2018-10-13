rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

#load required packages
library(tidyverse) 
library(readr)

################################ MODEL FIT STANDARD RL MODEL TO NON-SOCIAL DATA VS. RLW TO NON-SOCIAL DATA
##########################################################################################################
# #read in data
# fit_RL_6 <- 
#   read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standard_RL_18_30.txt",
#              " ", col_names = F, trim_ws = TRUE)
# 
# names(fit_RL_6)[1] <- "LL"
# names(fit_RL_6)[2] <- "alpha_fit"
# names(fit_RL_6)[3] <- "beta_fit"
# names(fit_RL_6)[4] <- "BIC"
# names(fit_RL_6)[5] <- "AIC"
# names(fit_RL_6)[6] <- "id"
# names(fit_RL_6)[7] <- "alpha_sim"
# names(fit_RL_6)[8] <- "beta_sim"
# 
# 
# fit_RLW_6 <- 
#   read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weighttostandard_18_30.txt",
#              " ", col_names = F, trim_ws = TRUE)
# 
# names(fit_RLW_6)[1] <- "LL_RLW"
# names(fit_RLW_6)[2] <- "alpha_fit_RLW"
# names(fit_RLW_6)[3] <- "beta_fit_RLW"
# names(fit_RLW_6)[4] <- "weight_fit_RLW"
# names(fit_RLW_6)[5] <- "BIC_RLW"
# names(fit_RLW_6)[6] <- "AIC_RLW"
# names(fit_RLW_6)[7] <- "id_RLW"
# names(fit_RLW_6)[8] <- "alpha_sim_RLW"
# names(fit_RLW_6)[9] <- "beta_sim_RLW"
# 
# df_fit <- cbind(fit_RL_6, fit_RLW_6)
# 
# df_fit$delta_BIC <- df_fit$BIC - df_fit$BIC_RLW 
# 
# 
# ggplot(aes(x = id, y = delta_BIC), data = df_fit) +
#   geom_bar(stat = "identity", fill = "steelblue4") +
#   #scale_fill_gradient(low = "dodgerblue3", high = "limegreen", expression(paste("Simulated ", beta, " values"))) +
#   ylab(expression(paste(Delta, "BIC (RL - RLW)"))) +
#   xlab("Simulated agent") +
#   scale_x_continuous(breaks = seq(5, 50, 5)) +
#   theme_classic() +
#   theme(axis.title.x = element_text(size = 15)) + 
#   theme(axis.title.y = element_text(size = 15))+
#   theme(axis.text = element_text(size = 13, colour = "black"))
# 
# hist(df_fit$delta_BIC) #not normally distributed
# 
# #run Wilcoxon signed rank test (like paired t-test for non-normally dsitributed data)
# nonpartest <- wilcox.test(df_fit$BIC, df_fit$BIC_RLW, paired = TRUE, exact = FALSE) 
# nonpartest
# 
# #calculate Z statistic
# Z <- qnorm(nonpartest$p.value/2)
# Z
# 
# #calculate effect size
# effect <- abs(Z)/sqrt(50) 
# effect
# 
# median(df_fit$BIC)        
# median(df_fit$BIC_RLW)
# 
# range(df_fit$BIC)
# range(df_fit$BIC_RLW)


################################ MODEL FIT WEIGHT RL MODEL TO SOCIAL DATA VS. RL TO SOCIAL DATA
###############################################################################################
#read in data
fit_RLW_6_social <-
read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_6_30_new.txt",
           " ", col_names = F, trim_ws = TRUE)

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


fit_RL_6_social <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standardtoweight_6_30_new.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL_6_social)[1] <- "LL"
names(fit_RL_6_social)[2] <- "alpha_fit"
names(fit_RL_6_social)[3] <- "beta_fit"
names(fit_RL_6_social)[4] <- "BIC"
names(fit_RL_6_social)[5] <- "AIC"
names(fit_RL_6_social)[6] <- "id"
names(fit_RL_6_social)[7] <- "alpha_sim"
names(fit_RL_6_social)[8] <- "beta_sim"
names(fit_RL_6_social)[9] <- "weight_sim"


df_fit_social <- cbind(fit_RL_6_social, fit_RLW_6_social)

df_fit_social$delta_BIC <- df_fit_social$BIC - df_fit_social$BIC_RLW

df_fit_social$weight_sign <- ifelse(df_fit_social$weight_sim < 0, -1, 1)

ggplot(aes(x = id, y = delta_BIC, fill = weight_sim_RLW), data = df_fit_social) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "dodgerblue3", high = "sienna1", expression(paste("Simulated ", omega, " values"))) +
  ylab(expression(paste(Delta, "BIC (RL - RLW)"))) +
  xlab("Simulated agent") +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text = element_text(size = 13, colour = "black")) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 13))

hist(df_fit_social$delta_BIC)

#run Wilcoxon signed rank test (like paired t-test for non-normally dsitributed data)
nonpartest <- wilcox.test(df_fit_social$BIC, df_fit_social$BIC_RLW, paired = TRUE) 
nonpartest

#calculate Z statistic
Z <- qnorm(nonpartest$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50) 
effect

median(df_fit_social$BIC)        
median(df_fit_social$BIC_RLW)

range(df_fit_social$BIC)
range(df_fit_social$BIC_RLW)


ggplot(aes(x = id, group = id, y = delta_BIC), data = df_fit_social) +
  geom_boxplot() +
  coord_flip()

