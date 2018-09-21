rm(list = ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")
getwd()

library(tidyverse)
library(truncnorm)


#sampling 30 alpha values  
vec1 <- seq(from = 0, by = .01, length.out = 100)

test <- dtruncnorm(vec1, a = 0, b = 1, mean = 0.5, sd = 0.1)
plot(test)

alpha <- sample(vec1, 50)
plot(alpha)

alpha_df <- as.data.frame(alpha)
alpha_df$index <- c(1:50)

ggplot(aes(x = index, y = alpha), data = alpha_df) +
  geom_point() + 
  geom_smooth(method = "glm")



#sampling 30 beta values  
vec2 <- seq(from = 0, by = .01, length.out = 100)

test <- dtruncnorm(vec2, a = 0, b = 1, mean = 0.5, sd = 0.1)
plot(test)

beta <- sample(vec2, 50)
plot(beta)

beta_df <- as.data.frame(beta)
beta_df$index <- c(1:50)

ggplot(aes(x = index, y = beta), data = beta_df) +
  geom_point() + 
  geom_smooth(method = "glm")

#sampling 30 weight values  
vec3 <- seq(from = -1, by = .01, length.out = 200)

test <- dtruncnorm(vec3, a = -1, b = 1, mean = 0, sd = .3)
plot(test)

weight <- sample(vec3, 50)
plot(weight)

weight_df <- as.data.frame(weight)
weight_df$index <- c(1:50)

ggplot(aes(x = index, y = weight), data = weight_df) +
  geom_point() + 
  geom_smooth(method = "glm")


sampling_df <- cbind(alpha_df, beta_df, weight_df)

sampling_df[2] <- NULL
sampling_df[3] <- NULL
sampling_df[4] <- NULL

#simulate data for social condition with weight parameter
library(reshape)

num = 50
subj = c(1:50)

#determine prediction of the model with best parameter estimates
cchoice <-  array(0, c(50, 8, 100))

#Q <- matrix(0,1,2) 
R            <- array(0, c(50, 8, 100))
Prob         <- array(0, c(50, 8, 100))
Feed         <- array(0, c(50, 8, 100))
Feed_c       <- array(0, c(50, 8, 100))
Feed_i       <- array(0, c(50, 8, 100))
Prob_correct <- array(0, c(50, 8, 100))
PE <- Q_all  <- array(0, c(50, 8, 100))

id    <- rep(1:50) 
# temp  <- rep(5)/10
# lrate <- rep(1:10)/10


FIT <- cbind(id, sampling_df)

for (id in subj) {
  
  alpha <- FIT[id, 2]; 
  beta  <- FIT[id, 3];
  weight <- FIT[id, 4];
  
  for (block in c(1:8)) {
    
    Q    <- matrix(0, 1, 2) # 1 row, 4 col 
    PROB <- matrix(0, 1, 2) 
    
    for (trial in c(1:100)) {
      
      #c <- c(1,2)
      #p <- c(.5,.5)
      
      #t <- sample(c, 1, replace = FALSE, prob = p)
      
      for (j in c(1:2)) { # options 
        PROB[1, j] <- exp(beta*Q[1, j]) / (exp(beta*Q[1, 1]) + exp(beta*Q[1, 2]) )
        choice     <- c(j, 3-j)
        P          <-   c(PROB[1, j], 1-PROB[1, j])
        cchoice [id, block, trial] <- sample(choice, 1, replace = FALSE, prob = P)
      }
      
      feedback <- c(-10, 10)
      
      if (cchoice [id, block, trial] == 2){
        rew_prob <- c(.25, .75)
        R [id, block, trial] <- sample(feedback, 1, replace = FALSE, prob = rew_prob)
      }
      
      else {
        rew_prob <- c(.75, .25)
        R[id, block, trial] <- sample(feedback, 1, replace = FALSE, prob = rew_prob)
      }
      
      Q[1,cchoice[id,block,trial]] <- Q[1,cchoice[id,block,trial]] + alpha * ((weight * R[id,block,trial]) - Q[1,cchoice[id,block,trial] ]);      
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

sim_data <- write.table(merged_dat, file = "simulation_50_agents_weight_model.txt", 
                        row.names = FALSE, col.names = FALSE)

sampled_values <- write.table(FIT, file = "parameter_values_simulation_weight_model.txt",
                              row.names = FALSE, col.names = FALSE)
s
