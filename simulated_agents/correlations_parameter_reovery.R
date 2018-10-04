rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

df <- data.frame(alpha_est = double(),
                 beta_est = double())

df[1, 1] <- corr_alpha$estimate
df[1, 2] <- corr_beta$estimate
df[2, 1]
df[2, 2]
df[3, 1]
df[3, 2]
