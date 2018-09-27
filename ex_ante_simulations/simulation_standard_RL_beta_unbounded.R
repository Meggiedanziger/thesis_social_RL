rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/ex_ante_simulations")
getwd()

library(reshape)

num = 80
subj = c(1:80)

#determine prediction of the model with best parameter estimates
cchoice      <- array(0, c(80, 12, 30))
R            <- array(0, c(80, 12, 30))
Prob         <- array(0, c(80, 12, 30))
Feed         <- array(0, c(80, 12, 30))
Feed_c       <- array(0, c(80, 12, 30))
Feed_i       <- array(0, c(80, 12, 30))
Prob_correct <- array(0, c(80, 12, 30))
PE <- Q_all  <- array(0, c(80, 12, 30))


id    <- c(1:80)
#temp  <- rep(1:10, each = 10)/10
temp  <- rep(c(1, 4, 7, 10, 30, 50, 70, 100), each = 10)/10
lrate <- rep(1:10, each = 1)/10



FIT <- cbind(id, lrate , temp)

for (id in subj) {
  
  alpha <- FIT[id, 2]; 
  beta  <- FIT[id, 3];
  
  for (block in c(1:12)) {
    
    Q    <- matrix(0, 1, 2) # 1 row, 4 col 
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
      
      Q[1,cchoice[id,block,trial]] <- Q[1,cchoice[id,block,trial] ] + alpha * (R[id,block,trial] - Q[1,cchoice[id,block,trial]]);
      
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

sim_data <- write.table(merged_dat, file = "ex_ante_simulation_standard_RL_12blocks_30trials_unbounded_beta.txt", 
                        row.names = FALSE, col.names = FALSE)

params_exante <- write.table(FIT, file = "ex_ante_simulation_parameters_standard_RL_12blocks_30trials_unbounded_beta.txt", 
                             row.names = FALSE, col.names = FALSE)
