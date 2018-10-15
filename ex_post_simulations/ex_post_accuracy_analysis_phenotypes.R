rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_post_simulations")

library(readr)
library(tidyverse)
library(Rcpp)
library(ez)
# library(MASS)

#########TEST IF FITTED LEARNING RATES AND TEMPERATURES OF THE BEST FITTING MODEL TO THE SOCIAL DATA (RLW)
#########DIFFERS IN THE GROUPS AS CATEGORIZED BY MEANS OF COMPUTATIONAL PHENOTYPING
###########################################################################################################

#read in data
fit_RLW_6_social <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_18_30_new.txt",
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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standardtoweight_18_30_new.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL_6_social)[1] <- "LL"
names(fit_RL_6_social)[2] <- "alpha_fit"
names(fit_RL_6_social)[3] <- "beta_fit"
names(fit_RL_6_social)[4] <- "BIC"
names(fit_RL_6_social)[5] <- "AIC"
names(fit_RL_6_social)[6] <- "id"
names(fit_RL_6_social)[7] <- "alpha_sim"
names(fit_RL_6_social)[8] <- "beta_sim"
#names(fit_RL_6_social)[9] <- "weight_sim"

df_fit_social <- cbind(fit_RL_6_social, fit_RLW_6_social)

df_fit_social$delta_BIC <- 
  df_fit_social$BIC - df_fit_social$BIC_RLW #calculate difference in BIC

df_fit_social$weight_sign <- 
  as.factor(ifelse(df_fit_social$weight_sim_RLW < 0, -1, 1)) #categorize by omega (positive/negative)

#allows computational phenotyping
#1  = RLW
#-1 = RL
df_fit_social$group <- as.factor(ifelse(df_fit_social$delta_BIC > 0, 1, -1))

sum(df_fit_social$group == -1) 

# only RL phenotype
test_post <- 
  df_fit_social %>% 
  filter(group == -1)

RL_ids <- as.factor(test_post$id) #RL phenotype id's

fit_RL <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_post_simulations/simulation_weight_18_30.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL)[1] <- "id"
names(fit_RL)[2] <- "block"
names(fit_RL)[3] <- "trial"
names(fit_RL)[4] <- "chosen_option"
names(fit_RL)[5] <- "feedback"

fit_RL <-
  fit_RL %>% 
  arrange(id)

fit_RL_sum <- 
  fit_RL %>% 
  group_by(id) %>% 
  summarize(accuracy_post = mean(chosen_option))

fit_RL_sum <-
  fit_RL_sum %>% 
  filter(id == 1 | id == 2 | id == 3 | id == 4 | id == 5 | id == 6 | id == 7 |
         id == 9 | id == 13 | id == 17 | id == 19 | id == 22 | id == 24 | id == 25 | 
         id == 26 | id == 27 | id == 28 | id == 29 | id == 31 | id == 32 | id == 34 | 
         id == 37 | id == 38 | id == 42 | id == 46 | id == 47 | id == 49 | id == 50)


sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_weight_18_30_new.txt", 
                       " ", escape_double = FALSE, col_names = F, 
                       trim_ws = TRUE)

names(sim_data)[1] <- "id"
names(sim_data)[2] <- "block"
names(sim_data)[3] <- "trial"
names(sim_data)[4] <- "chosen_option"
names(sim_data)[5] <- "feedback"


sim_data <-
  sim_data %>% 
  arrange(id)

sim_data_sum <- 
  sim_data %>% 
  group_by(id) %>% 
  summarize(accuracy = mean(chosen_option))


sim_data_sum <-
  sim_data_sum %>% 
  filter(id == 1 | id == 2 | id == 3 | id == 4 | id == 5 | id == 6 | id == 7 |
           id == 9 | id == 13 | id == 17 | id == 19 | id == 22 | id == 24 | id == 25 | 
           id == 26 | id == 27 | id == 28 | id == 29 | id == 31 | id == 32 | id == 34 | 
           id == 37 | id == 38 | id == 42 | id == 46 | id == 47 | id == 49 | id == 50)


test_data <- cbind(fit_RL_sum, sim_data_sum)

mean(test_data$accuracy)
mean(test_data$accuracy_post)

hist(test_data$accuracy)
hist(test_data$accuracy_post)

#run Wilcoxon signed rank test (like paired t-test for non-normally dsitributed data)
nonpartest <- wilcox.test(test_data$accuracy, test_data$accuracy_post, paired = TRUE, exact = F)
nonpartest

#calculate Z statistic
Z <- qnorm(nonpartest$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50) 
effect

median(test_data$accuracy)
median(test_data$accuracy_post)

#----------------------------------------------------------------------------------------

rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

library(readr)
library(tidyverse)
library(Rcpp)
library(ez)
# library(MASS)

#########TEST IF FITTED LEARNING RATES AND TEMPERATURES OF THE BEST FITTING MODEL TO THE SOCIAL DATA (RLW)
#########DIFFERS IN THE GROUPS AS CATEGORIZED BY MEANS OF COMPUTATIONAL PHENOTYPING
###########################################################################################################

#read in data
fit_RLW_6_social <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_18_30_new.txt",
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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standardtoweight_18_30_new.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL_6_social)[1] <- "LL"
names(fit_RL_6_social)[2] <- "alpha_fit"
names(fit_RL_6_social)[3] <- "beta_fit"
names(fit_RL_6_social)[4] <- "BIC"
names(fit_RL_6_social)[5] <- "AIC"
names(fit_RL_6_social)[6] <- "id"
names(fit_RL_6_social)[7] <- "alpha_sim"
names(fit_RL_6_social)[8] <- "beta_sim"
#names(fit_RL_6_social)[9] <- "weight_sim"

df_fit_social <- cbind(fit_RL_6_social, fit_RLW_6_social)

df_fit_social$delta_BIC <- 
  df_fit_social$BIC - df_fit_social$BIC_RLW #calculate difference in BIC

df_fit_social$weight_sign <- 
  as.factor(ifelse(df_fit_social$weight_sim_RLW < 0, -1, 1)) #categorize by omega (positive/negative)

#allows computational phenotyping
#1  = RLW
#-1 = RL
df_fit_social$group <- as.factor(ifelse(df_fit_social$delta_BIC > 0, 1, -1))

sum(df_fit_social$group == -1) 

# only RLW phenotype
test_post <- 
  df_fit_social %>% 
  filter(group == 1)

RL_ids <- as.factor(test_post$id) #RLW phenotype id's

fit_RL <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_post_simulations/simulation_standardtoweight_18_30.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL)[1] <- "id"
names(fit_RL)[2] <- "block"
names(fit_RL)[3] <- "trial"
names(fit_RL)[4] <- "chosen_option"
names(fit_RL)[5] <- "feedback"

fit_RL <-
  fit_RL %>% 
  arrange(id)

fit_RL_sum <- 
  fit_RL %>% 
  group_by(id) %>% 
  summarize(accuracy_post = mean(chosen_option))

fit_RL_sum <-
  fit_RL_sum %>% 
  filter(id == 8 | id == 10 | id == 11 | id == 12 | id == 14 | id == 15 | id == 16 |
           id == 18 | id == 20 | id == 21 | id == 23 | id == 30 | id == 33 | id == 35 | 
           id == 36 | id == 39 | id == 40 | id == 41 | id == 43 | id == 44 | id == 45 | 
           id == 48)


sim_data <- read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/agents_weight_18_30_new.txt", 
                       " ", escape_double = FALSE, col_names = F, 
                       trim_ws = TRUE)

names(sim_data)[1] <- "id"
names(sim_data)[2] <- "block"
names(sim_data)[3] <- "trial"
names(sim_data)[4] <- "chosen_option"
names(sim_data)[5] <- "feedback"


sim_data <-
  sim_data %>% 
  arrange(id)

sim_data_sum <- 
  sim_data %>% 
  group_by(id) %>% 
  summarize(accuracy = mean(chosen_option))


sim_data_sum <-
  sim_data_sum %>% 
  filter(id == 8 | id == 10 | id == 11 | id == 12 | id == 14 | id == 15 | id == 16 |
           id == 18 | id == 20 | id == 21 | id == 23 | id == 30 | id == 33 | id == 35 | 
           id == 36 | id == 39 | id == 40 | id == 41 | id == 43 | id == 44 | id == 45 | 
           id == 48)

test_data <- cbind(fit_RL_sum, sim_data_sum)

mean(test_data$accuracy)
mean(test_data$accuracy_post)

hist(test_data$accuracy)
hist(test_data$accuracy_post)

#run Wilcoxon signed rank test (like paired t-test for non-normally dsitributed data)
nonpartest <- wilcox.test(test_data$accuracy, test_data$accuracy_post, paired = TRUE, exact = F)
nonpartest

#calculate Z statistic
Z <- qnorm(nonpartest$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50) 
effect

median(test_data$accuracy)
median(test_data$accuracy_post)
