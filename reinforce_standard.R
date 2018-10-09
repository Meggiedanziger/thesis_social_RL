####### REINFORCEMENT FUNCTION #########

#is intergrated into optim function of the main loop for parameter estimation

reinforce <- function(param, subj, data) {
  
  alpha = param[1]
  theta = param[2]
  
  LL <- 0 #log likelihood goodness-of-fit measure  
  
  #main loop
  

  for (block in c(1:24)) {

    
    Q <- matrix(0.5, 1, 2) # 1 row, 2 col
    Prob <- matrix(0.5, 1, 2)
    
    for (trial in c(1:30)) {
       
      cchoice <-   data[data[ ,1] == id & data[ ,2] == block & data[ ,3] == trial, 4]
      
      if (cchoice != 0) {
        
        cchoice <-   data[data[ ,1] == id & data[ ,2] == block & data[ ,3] == trial, 4]
        
        Prob[1, cchoice] <- exp(theta*Q[1, cchoice]) / (exp(theta*Q[1, 1]) + exp(theta*Q[1, 2]))
        
        LL <- LL + log(Prob[1, cchoice]);
        
        R  <-    data[data[ ,1] == id & data[ ,2] == block & data[ ,3] == trial, 5]
        
        Q[1,cchoice] <- Q[1,cchoice] + alpha * (R - Q[1, cchoice]);
      }
      else if (cchoice == 0){
      }
    }
  }
  return(-2*LL)
  
}



