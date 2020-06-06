cf<-function(n){
  df <- n-1
  t_count <- 0
  z_count <- 0
  
  #Find the length of the column data
  for (i in 1:10000){
    random_array <- rexp(n)
    root_n <- sqrt(n)
    s <- sd(random_array)
    x_bar <- mean(random_array)
    mean <- 1
    alpha <- .95
    
    # 1-Sample T test
    t <-abs(qt((1-alpha)/2,df))
    
    t_LL <- x_bar - (t *(sqrt(s*s/(n))))
    t_UL <- x_bar + (t *(sqrt(s*s/(n))))
    
    t_int <- c(t_LL,t_UL)
    
    # 1-Sample Z interval
    # 95% confidence interval:
    z <- alpha#qnorm(1-.05/2)
    
    z_LL <-  x_bar - (z *(sqrt(s*s/(n))))
    z_UL <- x_bar + (z *(sqrt(s*s/(n))))
    z_int <- c(z_LL,z_UL)
    
    if ( (mean >= t_LL) & (mean <= t_UL)){
      # print("The t-interval captures the model mean")
      t_count <- t_count + 1
      t_LRCP <- t_count / i
    }
    else{
      # print("The t-interval does not capture the model mean")  
    }
    if ( (mean >= z_LL) & (mean <= z_UL)){
      # print("The z-interval captures the model mean")
      z_count <- z_count + 1
      z_LRCP <- z_count / i
    }
    else{
      # print("The z-interval does not capture the model mean")  
    }
    
    
  }
  
  print(t_LRCP)
  print(z_LRCP)
}

