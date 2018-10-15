rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_post_simulations")

library(readr)
library(tidyverse)



#read in data
fit <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weighttostandard_18_30.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit)[1] <- "LL_RLW"
names(fit)[2] <- "alpha_fit_RLW"
names(fit)[3] <- "beta_fit_RLW"
names(fit)[4] <- "weight_fit_RLW"
# names(fit)[5] <- "BIC_RLW"
# names(fit)[6] <- "AIC_RLW"
# names(fit)[7] <- "id_RLW"
# names(fit)[8] <- "alpha_sim_RLW"
# names(fit)[9] <- "beta_sim_RLW"
# names(fit)[10] <- "weight_sim_RLW"


library(reshape)

num = 50
subj = c(1:50)

#determine prediction of the model with best parameter estimates
cchoice      <- array(0, c(50, 18, 30))
R            <- array(0, c(50, 18, 30))
Prob         <- array(0, c(50, 18, 30))
Feed         <- array(0, c(50, 18, 30))
Feed_c       <- array(0, c(50, 18, 30))
Feed_i       <- array(0, c(50, 18, 30))
Prob_correct <- array(0, c(50, 18, 30))
PE <- Q_all  <- array(0, c(50, 18, 30))

id    <- rep(1:50)

FIT <- cbind(id, fit)

for (id in subj) {
  
  alpha  <- FIT[id, 3]; 
  beta   <- FIT[id, 4];
  weight  <- FIT[id, 5];
  
  for (block in c(1:18)) {
    
    Q    <- matrix(0.5, 1, 2) # 1 row, 2 columns 
    PROB <- matrix(0.5, 1, 2) 
    
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
      
      Q[1,cchoice[id,block,trial] ] <- Q[1,cchoice[id,block,trial] ] + alpha * ((weight * R[id,block,trial]) - Q[1,cchoice[id,block,trial]]);
      
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

sim_data <- write.table(merged_dat, file = "simulation_weightstandard_18_30.txt", 
                        row.names = FALSE, col.names = FALSE)

