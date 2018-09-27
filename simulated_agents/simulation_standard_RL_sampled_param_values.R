rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents") #set working directory
getwd()

#load required packages
library(tidyverse)
library(statmod)
library(reshape)


#create beta distribution from which to sample alpha values
set.seed(123)
n = 100000
binwidth = 0.05
pop_alpha <- rbeta(n, 1.5, 5.5)
pop_alphadf <- as.data.frame(pop_alpha)

#plot distribution and density function
ggplot(pop_alphadf, aes(x = pop_alpha, binwidth = binwidth, n = n)) +
  geom_histogram(binwidth = binwidth, colour = "white", fill = "steelblue3", size = 0.1) +
  stat_function(fun = function(x) dbeta(x, 1.5, 5.5) * n * binwidth, color = "firebrick1", size = 1) +
  xlab("x") +
  ylab("Frequency") +
  ggtitle("Beta(alpha = 1.5, beta = 5.5)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#sample alpha values
set.seed(123)
n = 50
binwidth = 0.04
alpha <- sample(pop_alpha, n)
alpha_df <- as.data.frame(alpha)
min(alpha)
max(alpha)
mean(alpha)
median(alpha)
boxplot(alpha) # noch schöner plotten

#plot histrogram of alpha values to inspect distribution of sampled avlues
ggplot(alpha_df, aes(x = alpha, binwidth = binwidth, n = n)) +
  geom_histogram(binwidth = binwidth, colour = "white", fill = "steelblue3", size = 0.1) +
  stat_function(fun = function(x) dbeta(x, 1.5, 5.5) * n * binwidth, color = "firebrick1", size = 1) +
  xlab("Sampled alpha values") +
  ylab("Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#create beta distribution from which to sample beta values
set.seed(123)
n = 100000
binwidth = 0.5
pop_beta <- rinvgauss(n, 1.5, 1)
pop_betadf <- as.data.frame(pop_beta)

#plot distribution and density function
ggplot(pop_betadf, aes(x = pop_beta, binwidth = binwidth, n = n)) +
  geom_histogram(binwidth = binwidth, colour = "white", fill = "steelblue3", size = 0.1) +
  stat_function(fun = function(x) dinvgauss(x, 1.5, 1) * n * binwidth, color = "firebrick1", size = 0.5) +
  xlab("x") +
  ylab("Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#sample beta values
set.seed(123)
n = 50
binwidth = 0.2
beta <- sample(pop_beta, n)
beta_df <- as.data.frame(beta)
min(beta)
max(beta)
mean(beta)
median(beta)
boxplot(beta) # noch schöner plotten

#plot histrogram of beta values to inspect distribution of sampled avlues
ggplot(beta_df, aes(x = beta, binwidth = binwidth, n = n)) +
  geom_histogram(binwidth = binwidth, colour = "white", fill = "steelblue3", size = 0.1) +
  stat_function(fun = function(x) dinvgauss(x, 1.5, 1) * n * binwidth, color = "firebrick1", size = 0.7)+
  xlab("Sampled beta values") +
  ylab("Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



#create data frame with alpha and beta parameter values
sampling_df <- cbind(alpha_df, beta_df)

#--------------------------------------------------------------------------------
#run simulation to create 50 data sets with 12 blocks and 30 trials
num  = 50
subj = c(1:50)

#determine prediction of the model with best parameter estimates
cchoice      <- array(0, c(50, 12, 30))
R            <- array(0, c(50, 12, 30))
Prob         <- array(0, c(50, 12, 30))
Feed         <- array(0, c(50, 12, 30))
Feed_c       <- array(0, c(50, 12, 30))
Feed_i       <- array(0, c(50, 12, 30))
Prob_correct <- array(0, c(50, 12, 30))
PE <- Q_all  <- array(0, c(50, 12, 30))



id <- c(1:50)


FIT <- cbind(id, sampling_df)

for (id in subj) {
  
  alpha <- FIT[id, 2]; #take alpha values from second column 
  beta  <- FIT[id, 3]; #take beta values from third column 
  
  for (block in c(1:12)) {
    
    Q    <- matrix(0, 1, 2) # 1 row, 2 columns 
    PROB <- matrix(0, 1, 2) 
    
    for (trial in c(1:30)) {
      
      for (j in c(1:2)) { #2 options 
        PROB[1, j] <- exp(beta*Q[1, j]) / (exp(beta*Q[1, 1]) + exp(beta*Q[1, 2])) #softmax function / decision rule
        choice     <- c(j, 3-j)
        P          <-   c(PROB[1, j], 1-PROB[1, j])
        cchoice [id, block, trial] <- sample(choice, 1, replace = FALSE, prob = P)
      }
      
      feedback <- c(-10, 10)
      
      if (cchoice [id, block, trial] == 2) { #good option
        rew_prob <- c(.25, .75)
        R [id, block, trial] <- sample(feedback, 1, replace = FALSE, prob = rew_prob)
      }
      
      else { #cchoice == 1 == bad option
        rew_prob <- c(.75, .25)
        R[id, block, trial] <- sample(feedback, 1, replace = FALSE, prob = rew_prob)
      }
      
      Q[1,cchoice[id,block,trial] ] <- Q[1,cchoice[id,block,trial] ] + alpha * (R[id,block,trial] - Q[1,cchoice[id,block,trial] ]);
      
      #if (cchoice[id,block,trial]  == 2) { 
      #  Q[1,1] <- -Q[1,2];
      #}
      #else  {
      #  Q[1,2] <- -Q[1,1]; #symmetrisches Update der Q values
      #}
    }
  }    
}


R_ <- melt(R)
C_ <- melt(cchoice)

merged_dat <- merge(C_, R_, by = c("X1", "X2", "X3"))
names(merged_dat)[1] <- "id" 
names(merged_dat)[2] <- "block"
names(merged_dat)[3] <- "trial"
names(merged_dat)[4] <- "chosen_option"
names(merged_dat)[5] <- "feedback"

merged_dat$chosen_option <- merged_dat$chosen_option - 1
acc <- tapply(merged_dat$chosen_option, merged_dat$id, mean)
plot(acc)

sim_data <- merged_dat

sim_data <- write.table(merged_dat, file = "agents_standard_RL_12blocks_30trials.txt", 
                        row.names = FALSE, col.names = FALSE)

sampled_values <- write.table(FIT, file = "agents_standard_RL_12blocks_30trials_parameters.txt",
                              row.names = FALSE, col.names = FALSE)