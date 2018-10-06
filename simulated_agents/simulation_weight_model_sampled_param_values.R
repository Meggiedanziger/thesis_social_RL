rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL") #set working directory
getwd()

#load required packages
library(tidyverse)
library(statmod)
library(reshape)


#create beta distribution from which to sample alpha values
set.seed(234)
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
set.seed(234)
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


#create beta distribution from which to sample beta values
set.seed(234)
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
set.seed(234)
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



#create beta distribution from which to sample weight values
set.seed(234)
n = 100000
binwidth = 0.05
pop_weight <- rbeta(n, 16, 6) ################## THINK THIS OVER!! 
                             #### DISTRIBUTION SHOULB BE MORE SKEWED TO 1 AS THIS IS STANDARD RL
pop_weightdf <- as.data.frame(pop_weight)

#plot distribution and density function
ggplot(pop_weightdf, aes(x = pop_weight, binwidth = binwidth, n = n)) +
  geom_histogram(binwidth = binwidth, colour = "white", fill = "steelblue3", size = 0.1) +
  stat_function(fun = function(x) dbeta(x, 16, 6) * n * binwidth, color = "firebrick1", size = 1) +
  scale_x_continuous(breaks = seq(0, 1.0, 0.2)) +
  xlab("x") +
  ylab("Frequency") +
  ggtitle(expression(paste("Beta distribution with ", alpha, " = 4, " , beta, " = 4 "))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#sample weight values
set.seed(234)
n = 50
binwidth = 0.04
weight <- sample(pop_weight, n)
weight_df <- as.data.frame(weight)
min(weight)
max(weight)
mean(weight)
median(weight)

#determine whether weight is positive or negative
set.seed(234)
n = 100000
pop_d <- rbeta(n, 4, 4)
hist(pop_d)

#sample d values to determine which of the weight values is positive or negative
set.seed(234)
determ <- sample(pop_d, 50)
sum(determ < 0.5) #check if equally many are positive or negative

determ_t <- ifelse(determ < 0.5, 1, -1) #transform all values below 0.5 to -1, all above to 1

df_sign <- as.data.frame(cbind(weight, determ_t))

weight <- df_sign$weight * df_sign$determ_t

weight_df <- as.data.frame(weight)
median(weight_df$weight)
mean(weight_df$weight)

#weight_df$id <- c(1:50)

ggplot(aes(x = id, y = weight), data = weight_df) +
  geom_point(size = 2.5, color = "sienna1", alpha = 0.8) +
  xlab("Simulated agent") +
  ylab(expression(paste("Simulated ", omega, " values"))) +
  theme_classic()

weightbox <- 
  ggplot(aes(x = 1, y = weight), data = weight_df) +
  geom_boxplot(width = 0.15, outlier.color = "limegreen", outlier.alpha = 0.7, outlier.size = 2.5) +
  scale_y_continuous(breaks = seq(-1, 1, 0.3)) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab(expression(paste("Simulated ", omega, " values"))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))
weightbox

dat <- ggplot_build(weightbox)$data[[1]]
weightbox + 
  geom_segment(aes(x = xmin, xend = xmax, y = middle, yend = middle), data = dat, colour = "blue", size = 1.3) +
  geom_jitter(size = 2.5, width = 0.05, height = 0.0, color = "sienna1", alpha = 0.9)



#create data frame with alpha and beta parameter values
sampling_df <- cbind(alpha_df, beta_df, weight_df)



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
  
  alpha  <- FIT[id, 2]; #take alpha values from second column 
  beta   <- FIT[id, 3]; #take beta values from third column 
  weight <- FIT[id, 4]; #take weight values from fourth column
  
  for (block in c(1:12)) {
    
    Q    <- matrix(0.5, 1, 2) # 1 row, 2 columns 
    PROB <- matrix(0.5, 1, 2) 
    
    for (trial in c(1:30)) {
      
      for (j in c(1:2)) { #2 options 
        PROB[1, j] <- exp(beta*Q[1, j]) / (exp(beta*Q[1, 1]) + exp(beta*Q[1, 2])) #softmax function / decision rule
        choice     <- c(j, 3-j)
        P          <-   c(PROB[1, j], 1-PROB[1, j])
        cchoice [id, block, trial] <- sample(choice, 1, replace = FALSE, prob = P)
      }
      
      feedback <- c(-1, 1)
      
      if (cchoice [id, block, trial] == 2) { #good option
        rew_prob <- c(.25, .75)
        R [id, block, trial] <- sample(feedback, 1, replace = FALSE, prob = rew_prob)
      }
      
      else { #cchoice == 1 == bad option
        rew_prob <- c(.75, .25)
        R[id, block, trial] <- sample(feedback, 1, replace = FALSE, prob = rew_prob)
      }
      
      Q[1,cchoice[id,block,trial] ] <- Q[1,cchoice[id,block,trial] ] + alpha * ((weight * R[id,block,trial]) - Q[1,cchoice[id,block,trial] ]);
      
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

setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

sim_data <- write.table(merged_dat, file = "agents_weight_12blocks_30trials_new.txt", 
                        row.names = FALSE, col.names = FALSE)

sampled_values <- write.table(FIT, file = "agents_weight_12blocks_30trials_parameters_new.txt",
                              row.names = FALSE, col.names = FALSE)
