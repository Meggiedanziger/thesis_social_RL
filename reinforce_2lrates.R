reinforce2lrates <- function(param, subj, data) {
  
  alpha_ex = param[1]
  alpha_in = param[2]
  theta    = param[3]
  
  LL <- 0 #log likelihood goodness-of-fit measure  
  
  # main loop
  
  for (block in c(1:12)) {
    
    Q <- matrix(0, 1, 2) # 1 row, 2 col
    Prob <- matrix(0, 1, 2)
  
    
    for (trial in c(1:100)) {
      
      cchoice <-   data[data[ ,1] == id & data[ ,2] == block & data[ ,3] == trial, 4]
      
      if (cchoice == 1) { #bad option --> excluder
        #if choice == 1 check whether includer or excluder and assign respective learning rate
        cchoice <-   data[data[ ,1] == id & data[ ,2] == block & data[ ,3] == trial, 4]
        
        Prob[1, cchoice] <- exp(theta*Q[1, cchoice]) / (exp(theta*Q[1, 1]) + exp(theta*Q[1, 2]))
        
        LL <- LL + log(Prob[1, cchoice]);
        
        R  <-    data[data[ ,1] == id & data[ ,2] == block & data[ ,3] == trial, 5]
        
        Q[1,cchoice] <- Q[1,cchoice] + alpha_ex * (R - Q[1, cchoice]);
        #excluder alpha to modulate prediction error
      }
      
      else if (cchoice == 2) { #good option --> includer
        cchoice <-   data[data[ ,1] == id & data[ ,2] == block & data[ ,3] == trial, 4]
        
        Prob[1, cchoice] <- exp(theta*Q[1, cchoice]) / (exp(theta*Q[1, 1]) + exp(theta*Q[1, 2]))
        
        LL <- LL + log(Prob[1, cchoice]);
        
        R  <-    data[data[ ,1] == id & data[ ,2] == block & data[ , 3] == trial, 5]
        
        Q[1,cchoice] <- Q[1,cchoice] + alpha_in * (R - Q[1, cchoice]);
        #includer alpha to modulate prediction error
      }
    }
  }
  return(-2*LL)
  
}
