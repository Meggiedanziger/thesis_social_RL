library(readr)


sim_data <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulation_50_agents_weight_model.txt", 
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

# data <- sim_data
# data <- as.data.frame(data)


#check rewards / reward probabilities from simulation function

sum(sim_data$chosen_option == 2) # good option
sum(sim_data$chosen_option == 2 & sim_data$feedback == 10) # good option with positive feedback
sum(sim_data$chosen_option == 2 & sim_data$feedback == -10) # good option with negative feedback

sum(sim_data$chosen_option == 1) # bad option
sum(sim_data$chosen_option == 1 & sim_data$feedback == 10) # bad option with positive feedback
sum(sim_data$chosen_option == 1 & sim_data$feedback == -10) # bad option with negative feedback


parameter_sim <- 
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/parameter_values_simulation_weight_model.txt", 
             " ", col_names = F, 
             trim_ws = TRUE)

names(parameter_sim)[1] <- "id"
names(parameter_sim)[2] <- "alpha_sim"
names(parameter_sim)[3] <- "beta_sim"
names(parameter_sim)[4] <- "weight_sim"

sim_data_7 <-
  sim_data %>% 
  filter(id == 7)

plot_bar <-
  sim_data_7 %>% 
  group_by(chosen_option)


ggplot(plot_bar, aes(x = chosen_option)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  #facet_wrap( ~ id, nrow = 2, ncol = 5) +
  xlab("chosen option") +
  ylab("percentage of choices") +
  scale_x_continuous(breaks = seq(1, 2, 1)) +
  #scale_y_continuous(breaks = seq(0, 100, 20)) +
  theme_classic()


sim_data_15 <-
  sim_data %>% 
  filter(id == 15)

plot_bar <-
  sim_data_15 %>% 
  group_by(chosen_option)


ggplot(plot_bar, aes(x = chosen_option)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  #facet_wrap( ~ id, nrow = 2, ncol = 5) +
  xlab("chosen option") +
  ylab("percentage of choices") +
  scale_x_continuous(breaks = seq(1, 2, 1)) +
  #scale_y_continuous(breaks = seq(0, 100, 20)) +
  theme_classic()

sim_data_18 <-
  sim_data %>% 
  filter(id == 18)

plot_bar <-
  sim_data_18 %>% 
  group_by(chosen_option)


ggplot(plot_bar, aes(x = chosen_option)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  #facet_wrap( ~ id, nrow = 2, ncol = 5) +
  xlab("chosen option") +
  ylab("percentage of choices") +
  scale_x_continuous(breaks = seq(1, 2, 1)) +
  #scale_y_continuous(breaks = seq(0, 100, 20)) +
  theme_classic()

sim_data$chosen_option <- sim_data$chosen_option -1

sim_data_sum <- 
  sim_data %>% 
  group_by(id) %>% 
  summarize(accuracy = mean(chosen_option))

plot_data <-
  as.data.frame(cbind(parameter_sim, sim_data_sum))

plot_data[5] <- NULL

test1 <- 
  ggplot(aes(x = weight_sim, y = accuracy), data = plot_data) +
  geom_point() +
  geom_smooth(method = "glm") 

test1

test2 <- 
  ggplot(aes(x = alpha_sim, y = accuracy), data = plot_data) +
  geom_point() +
  geom_smooth(method = "glm") 

test2

test3 <- 
  ggplot(aes(x = beta_sim, y = accuracy), data = plot_data) +
  geom_point() +
  geom_smooth(method = "glm") 

test3
