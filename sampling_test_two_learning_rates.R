rm(list = ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")
getwd()

library(tidyverse)
library(truncnorm)

#sampling 30 alpha values for excluder
vec1 <- seq(from = 0, by = .01, length.out = 80)

test <- dtruncnorm(vec1, a = 0, b = 1, mean = 0.4, sd = 0.1)
plot(test)

alpha_ex <- sample(vec1, 10)

alpha_ex_df <- as.data.frame(alpha_ex)

#sampling 30 alpha values for includer
vec2 <- seq(from = 0, by = .01, length.out = 80)

test <- dtruncnorm(vec2, a = 0, b = 1, mean = 0.4, sd = 0.1)
plot(test)

alpha_in <- sample(vec2, 10)

alpha_in_df <- as.data.frame(alpha_in)



#sampling 30 beta values  
vec3 <- seq(from = 0, by = .01, length.out = 80)

test <- dtruncnorm(vec3, a = 0, b = 1, mean = 0.4, sd = 0.1)
plot(test)

beta <- sample(vec3, 10)

beta_df <- as.data.frame(beta)


sampling_df <- cbind(alpha_ex_df, alpha_in_df, beta_df)

#simulate data for social condition with two separate learning rates for excluder and includer
library(reshape)

num = 10
subj = c(1:10)

#determine prediction of the model with best parameter estimates
cchoice <-  array(0, c(10, 4, 100))

#Q <- matrix(0,1,2) 
R <- array(0, c(10, 4, 100))

Prob         <- array(0, c(10, 4, 100))
Feed         <- array(0, c(10, 4, 100))
Feed_c       <- array(0, c(10, 4, 100))
Feed_i       <- array(0, c(10, 4, 100))
Prob_correct <- array(0, c(10, 4, 100))
PE <- Q_all  <- array(0, c(10, 4, 100))

id    <- rep(1:10) 

FIT <- cbind(id, sampling_df)

for (id in subj) {
  
  alpha_ex  <- FIT[id, 2]; 
  alpha_in  <- FIT[id, 3];
  beta      <- FIT[id, 4];
  
  for (block in c(1:4)) {
    
    Q    <- matrix(0, 1, 2) # 1 row, 4 col 
    PROB <- matrix(0, 1, 2) 
    
    for (trial in c(1:200)) {
      
      #c <- c(1,2)
      #p <- c(.5,.5)
      
      #t <- sample(c, 1, replace = FALSE, prob = p)
      
      for (j in c(1:2)) { # options 
        PROB[1, j] <- exp(beta*Q[1, j]) / (exp(beta*Q[1, 1]) + exp(beta*Q[1, 2]))
        choice     <- c(j, 3-j)
        P          <-   c(PROB[1, j], 1-PROB[1, j])
        cchoice [id, block, trial] <- sample(choice, 1, replace = FALSE, prob = P)
        
        
        feedback <- c(-10, 10)
        
        if (cchoice [id, block, trial] == 2) {#good option --> includer
          
          rew_prob <- c(.25, .75)
          
          R [id, block, trial] <- sample(feedback, 1, replace = FALSE, prob = rew_prob)
          Q[1,cchoice[id,block,trial] ] <- Q[1,cchoice[id,block,trial] ] + alpha_in * (R[id,block,trial] - Q[1,cchoice[id,block,trial] ]);
        }
        
        else {#bad option --> excluder
          
          rew_prob <- c(.75, .25)
          
          R[id, block, trial] <- sample(feedback, 1, replace = FALSE, prob = rew_prob)
          Q[1,cchoice[id,block,trial] ] <- Q[1,cchoice[id,block,trial] ] + alpha_ex * (R[id,block,trial] - Q[1,cchoice[id,block,trial] ]);
        }
        #if (cchoice[id,block,trial]  == 2) { 
        #  Q[1,1] <- -Q[1,2];
        #}
        #else  {
        #  Q[1,2] <- -Q[1,1]; #symmetrisches Update der Q values
        #}
      }
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

sim_data <- write.table(merged_dat, file = "simulation_10_agents_2lr_model.txt", 
                        row.names = FALSE, col.names = FALSE)

sampled_values <- write.table(FIT, file = "parameter_values_simulation_2lr_model.txt",
                              row.names = FALSE, col.names = FALSE)
