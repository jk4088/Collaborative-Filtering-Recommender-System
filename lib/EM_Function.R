###################################################################
### Model-based Collaborative Filtering Algorithm EM Clustering###
###################################################################

em_step <- function(data, C, scores, iteration, tol, conver){
  
  # Step 0: Initialize Parameters 
  
  vec <- abs(rnorm(C))
  vec <- vec/sum(vec)
  
  vec1 <- abs(rnorm(C))
  vec1 <- vec1/sum(vec1)
  
  n_user <- nrow(data)
  n_movie <- ncol(data)
  
  mu <- matrix(vec1, nrow = C) # Initialize mu with random mean values of each cluster 
  gamma <- array(vec,  dim = c(scores, C, n_movie)) # Initialize gamma with randomized probabilities 
  assign_mat <- matrix(NA, nrow = n_user, ncol = C) # Empty assignment matrix 
  
  iter <- 1
  
  while(conver > tol | iter <= iteration){
    
    print(iter)
    
    # Step 1: E-step 
    
    for (i in 1:n_user){
      for (c in 1:C){
        movies_i <- as.vector(which(!is.na(data[i, ]))) #vector of movies that user i rated
        scores_i <- as.vector(data[i, !is.na(data[i, ])]) #vector of scores that user i gave
        
        denominator <- 0
        
        for(j in 1:length(movies_i)){ #calculating numerator by looping through all the movies that user i rated 
          gamma_extract<- sum(log(gamma[scores_i[j], c, movies_i[j]]))
        }
        numerator <- gamma_extract + log(as.vector(mu[c]))
        
        for(l in 1:C){ #calculating denominator by summing the numerators 
          for(j in 1:length(movies_i)){
            gamma_extract1<- sum(log(gamma[scores_i[j], l, movies_i[j]]))
          }
          gamma_product_log_dem <- gamma_extract1 + log(as.vector(mu[l]))
          
          denominator <- denominator + gamma_product_log_dem
        }
        
        assign_mat[i,c] <- numerator / denominator
      }
    }
    
    print(paste("E-step is complete!"))
    
    # Step 2.1: M-step (mu update) 
    
    mu <- colMeans(assign_mat)
    
    # Step 2.2: M-step (gamma upate)
    
    for (c in 1:C) {  # Calculating numerator 
      for (k in 1:scores) {
        indicator <- matrix(0, ncol = ncol(data), nrow = nrow(data))
        indicator1 <- ifelse(data == k, 1, 0) #Creating a 0,1 matrix for each score (1 if score matches score, 0 otherwise)
        indicator[indicator1 == 1] <- 1
        
        for (j in 1:n_movie) { # Calculating denominator
          users_j <- as.vector(which(!is.na(data[, j])))
          gamma[k, c, j] <- sum(assign_mat[users_j, c] * indicator[users_j, j]) / sum(assign_mat[users_j, c])
        }
      }
    }
    print(paste("M-step is complete!"))
    
    
    assign_mat_old <- assign_mat
    conver <- norm(assign_mat_old - assign_mat, type = "O")
    iter <- iter + 1
    
  }
  return(list(gamma, assign_mat))
  
}
