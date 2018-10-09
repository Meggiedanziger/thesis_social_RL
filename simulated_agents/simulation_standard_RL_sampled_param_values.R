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
  scale_x_continuous(breaks = seq(0, 1.0, 0.2)) +
  xlab("x") +
  ylab("Frequency") +
  ggtitle(expression(paste("Beta distribution with ", alpha, " = 1.5, " , beta, " = 5.5 "))) +
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

#alpha_df$id <- c(1:50)

ggplot(aes(x = id, y = alpha), data = alpha_df) +
  geom_point(size = 2.5, color = "red", alpha = 0.6) +
  xlab("Simulated agent") +
  ylab(expression(paste("Simulated ", alpha, " values"))) +
  theme_classic()

alphabox <- 
  ggplot(aes(x = 1, y = alpha), data = alpha_df) +
  geom_boxplot(width = 0.3, outlier.colour = "red", outlier.alpha = 0.6, outlier.size = 2.5) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab(expression(paste("Simulated ", alpha, " values"))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
alphabox

dat <- ggplot_build(alphabox)$data[[1]]
alphabox + 
  geom_segment(aes(x = xmin, xend = xmax, y = middle, yend = middle), data = dat, colour = "blue", size = 1.3) +
  geom_jitter(size = 2.5, width = 0.05, height = 0.0, color = "red", alpha = 0.6)
  

#plot histrogram of alpha values to inspect distribution of sampled avlues
ggplot(alpha_df, aes(x = alpha, binwidth = binwidth, n = n)) +
  geom_histogram(binwidth = binwidth, colour = "white", fill = "steelblue3", size = 0.1) +
  stat_function(fun = function(x) dbeta(x, 1.5, 5.5) * n * binwidth, color = "firebrick1", size = 1) +
  xlab("Sampled alpha values") +
  ylab("Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#create Inverse Gaussian distribution from which to sample beta values
set.seed(123)
n = 100000
binwidth = 0.5
pop_beta <- rinvgauss(n, 1.5, 1)
pop_betadf <- as.data.frame(pop_beta)

#plot distribution and density function
ggplot(pop_betadf, aes(x = pop_beta, binwidth = binwidth, n = n)) +
  geom_histogram(binwidth = binwidth, colour = "white", fill = "steelblue3", size = 0.1) +
  stat_function(fun = function(x) dinvgauss(x, 1.5, 1) * n * binwidth, color = "firebrick1", size = 0.7) +
  ggtitle(expression(paste("Inverse Gaussian distribution with  ", mu, " = 1.5, " , lambda, " = 1) "))) +
  #scale_x_continuous(breaks = seq(0, 30, 5)) +
  xlab("x") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5)) +
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

#beta_df$id <- c(1:50)

ggplot(aes(x = id, y = beta), data = beta_df) +
  geom_point(size = 2.5, color = "limegreen", alpha = 0.6) +
  xlab("Simulated agent") +
  ylab(expression(paste("Simulated ", beta, " values"))) +
  theme_classic()

betabox <- 
  ggplot(aes(x = 1, y = beta), data = beta_df) +
  geom_boxplot(width = 0.15, outlier.color = "limegreen", outlier.alpha = 0.7, outlier.size = 2.5) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab(expression(paste("Simulated ", beta, " values"))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
betabox

dat <- ggplot_build(betabox)$data[[1]]
betabox + 
  geom_segment(aes(x = xmin, xend = xmax, y = middle, yend = middle), data = dat, colour = "blue", size = 1.3) +
  geom_jitter(size = 2.5, width = 0.05, height = 0.0, color = "limegreen", alpha = 0.7)

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
cchoice      <- array(0, c(50, 6, 30))
R            <- array(0, c(50, 6, 30))
Prob         <- array(0, c(50, 6, 30))
Feed         <- array(0, c(50, 6, 30))
Feed_c       <- array(0, c(50, 6, 30))
Feed_i       <- array(0, c(50, 6, 30))
Prob_correct <- array(0, c(50, 6, 30))
PE <- Q_all  <- array(0, c(50, 6, 30))



id <- c(1:50)


FIT <- cbind(id, sampling_df)

for (id in subj) {
  
  alpha <- FIT[id, 2]; #take alpha values from second column 
  beta  <- FIT[id, 3]; #take beta values from third column 
  
  for (block in c(1:6)) {
    
    Q    <- matrix(0.5, 1, 2) # 1 row, 2 columns 
    PROB <- matrix(0.5, 1, 2) 
    
    for (trial in c(1:30)) {
      
      for (j in c(1:2)) { #2 options 
        PROB[1, j] <- exp(beta*Q[1, j]) / (exp(beta*Q[1, 1]) + exp(beta*Q[1, 2])) #softmax function / decision rule
        choice     <- c(j, 3-j)
        P          <-   c(PROB[1, j], 1-PROB[1, j])
        cchoice [id, block, trial] <- sample(choice, 1, replace = FALSE, prob = P)
      }
      
      feedback <- c(-20, 20)
      
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

sim_data <- write.table(merged_dat, file = "agents_standard_RL_test.txt", 
                        row.names = FALSE, col.names = FALSE)

sampled_values <- write.table(FIT, file = "agents_standard_RL_test_params.txt",
                              row.names = FALSE, col.names = FALSE)

