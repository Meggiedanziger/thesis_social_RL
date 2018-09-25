rm(list = ls()) # delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL")
getwd()

library(tidyverse)
library(reshape)

num = 250
subj = c(1:250)

#determine prediction of the model with best parameter estimates
cchoice      <- array(0, c(250, 8, 30))
R            <- array(0, c(250, 8, 30))
Prob         <- array(0, c(250, 8, 30))
Feed         <- array(0, c(250, 8, 30))
Feed_c       <- array(0, c(250, 8, 30))
Feed_i       <- array(0, c(250, 8, 30))
Prob_correct <- array(0, c(250, 8, 30))
PE <- Q_all  <- array(0, c(250, 8, 30))


id     <- c(1:250)
temp   <- rep(seq(1, 9, 2), each = 5)/10
lrate  <- rep(seq(1, 9, 2), each = 1)/10
weight <- rep(seq(from = -9, to = 9, 2), each = 25)/10


FIT <- cbind(id, lrate , temp, weight)

for (id in subj) {
  
  alpha  <- FIT[id, 2]; 
  beta   <- FIT[id, 3];
  weight <- FIT[id, 4];
    
  for (block in c(1:8)) {
    
    Q    <- matrix(0, 1, 2) # 1 row, 2 columns
    PROB <- matrix(0, 1, 2) 
    
    for (trial in c(1:30)) {
      
      for (j in c(1:2)) { # options 
        PROB[1, j] <- exp(beta*Q[1, j]) / (exp(beta*Q[1, 1]) + exp(beta*Q[1, 2]))
        choice     <- c(j, 3-j)
        P          <-   c(PROB[1, j], 1-PROB[1, j])
        cchoice [id, block, trial] <- sample(choice, 1, replace = FALSE, prob = P)
      }
      
      feedback <- c(-10, 10)
      
      if (cchoice [id, block, trial] == 2) {
        rew_prob <- c(.25, .75)
        R [id, block, trial] <- sample(feedback, 1, replace = FALSE, prob = rew_prob)
      }
      
      else {
        rew_prob <- c(.75, .25)
        R[id, block, trial] <- sample(feedback, 1, replace = FALSE, prob = rew_prob)
      }
      
      Q[1,cchoice[id,block,trial]] <- Q[1,cchoice[id,block,trial]] + alpha * ((weight * R[id,block,trial]) - Q[1,cchoice[id,block,trial]]);
      
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

sim_data <- write.table(merged_dat, file = "ex_ante_simulation_weight_model_8blocks_30trials.txt", 
                        row.names = FALSE, col.names = FALSE)

params_exante <- write.table(FIT, file = "ex_ante_simulation_parameters_weight_model_8blocks_30trials.txt", 
                             row.names = FALSE, col.names = FALSE)

#check rewards / reward probabilities from simulation function

sim_data$chosen_option <- sim_data$chosen_option + 1

sum(sim_data$chosen_option == 2) # good option
sum(sim_data$chosen_option == 2 & sim_data$feedback == 10) # good option with positive feedback
sum(sim_data$chosen_option == 2 & sim_data$feedback == -10) # good option with negative feedback

sum(sim_data$chosen_option == 1) # bad option
sum(sim_data$chosen_option == 1 & sim_data$feedback == 10) # bad option with positive feedback
sum(sim_data$chosen_option == 1 & sim_data$feedback == -10) # bad option with negative feedback
