rm(list = ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")
getwd()

library(tidyverse)
library(truncnorm)

dis <- rtruncnorm(100, a=-Inf, b=Inf, mean = 0, sd = 1)

plot(dis)

dis <- rtruncnorm(100, a = -5, b = 5, mean = 0, sd = 1)

plot(dis)


vec = seq(from = 0, by = .001, length.out = 1000)


test = dtruncnorm(vec, a = 0, b = 1, mean = 0.5, sd = 0.2)
plot(test)


mysim_social <- sample(2, 100, prob = c(0.95, 0.05), replace = T)

df <- as.data.frame(mysim_social)
names(df)[1] <- "choice"

ggplot(aes(x = choice), data = df) +
  geom_bar()

mysim_social_rewards <- sample(2, 100, prob = c(0.25, 0.75), replace = T)
df_rew <- as.data.frame(mysim_social_rewards)
names(df_rew)[1] <- "reward"

ggplot(aes(x = reward), data = df_rew) +
  geom_bar()

df_social <- cbind(df, df_rew)
