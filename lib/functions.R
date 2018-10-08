###################################################################
### Memory-based Collaborative Filtering Algorithm Starter Code ###
###################################################################

### Authors: CIndy Rush
### Project 3
### ADS Spring 2018


MS_data_transform <- function(MS) {
  
  ## Calculate UI matrix for Microsoft data
  ##
  ## input: data   - Microsoft data in original form
  ##
  ## output: UI matrix
  
  
  # Find sorted lists of users and vroots
  users  <- sort(unique(MS$V2[MS$V1 == "C"]))
  vroots <- sort(unique(MS$V2[MS$V1 == "V"]))
  
  nu <- length(users)
  nv <- length(vroots)
  
  # Initiate the UI matrix
  UI            <- matrix(0, nrow = nu, ncol = nv)
  row.names(UI) <- users
  colnames(UI)  <- vroots
  
  user_locs <- which(MS$V1 == "C")
  
  # Cycle through the users and place 1's for the visited vroots.
  for (i in 1:nu) {
    name     <- MS$V2[user_locs[i]]
    this_row <- which(row.names(UI) == name)
    
    # Find the vroots
    if (i == nu) {
      v_names <- MS$V2[(user_locs[i] + 1):nrow(MS)]
    } else {
      v_names <- MS$V2[(user_locs[i] + 1):(user_locs[i+1] - 1)]
    }  
    
    # Place the 1's
    UI[this_row, colnames(UI) %in% v_names] <- 1
  }
  return(UI)
}



movie_data_transform <- function(movie) {
  
  ## Calculate UI matrix for eachmovie data
  ##
  ## input: data   - movie data in original form
  ##
  ## output: UI matrix
  
  
  # Find sorted lists of users and vroots
  users  <- sort(unique(movie$User))
  movies <- sort(unique(movie$Movie))
  
  # Initiate the UI matrix
  UI            <- matrix(NA, nrow = length(users), ncol = length(movies))
  row.names(UI) <- users
  colnames(UI)  <- movies
  
  # We cycle through the users, finding the user's movies and ratings
  for (i in 1:length(users)) {
    user    <- users[i]
    movies  <- movie$Movie[movie$User == user]
    ratings <- movie$Score[movie$User == user]
    
    ord     <- order(movies)
    movies  <- movies[ord]
    ratings <- ratings[ord]
    
    # Note that this relies on the fact that things are ordered
    UI[i, colnames(UI) %in% movies] <- ratings
  }
  return(UI)
}  


calc_weight <- function(data, run.pearson=F, run.entropy=F, run.spearman=F, run.sqdiff=F, run.cosin = F) {
  
  ## Calculate similarity weight matrix
  ##
  ## input: data   - movie data or MS data in user-item matrix form
  ##        method - 'pearson'
  ##
  ## output: similarity weight matrix
  
  
  # Iniate the similarity weight matrix
  data       <- as.matrix(data)
  weight_mat <- diag(x = 1, nrow(data), nrow(data))
  weight_func <- function(rowA, rowB) {
    
    # weight_func takes as input two rows (thought of as rows of the data matrix) and 
    # calculates the similarity between the two rows according to 'method'
    
    joint_values <- !is.na(rowA) & !is.na(rowB)
    if (sum(joint_values) == 0) {
      return(0)
    } else {
      if (run.pearson) {
        return(cor(rowA[joint_values], rowB[joint_values], method = 'pearson'))
      }
      if (run.entropy) {
        if(!require("infotheo")){
          install.packages("infotheo")
        }
          library("infotheo")
        return(mutinformation(rowA[joint_values], rowB[joint_values], method = 'emp'))
      }
      if (run.spearman) {
        return(cor(rowA[joint_values], rowB[joint_values], method = 'spearman'))
      }
      if (run.sqdiff) {
        return(mean((rowA[joint_values]-rowB[joint_values])^2))
      }
      if(run.cosin){
        if(!require("lsa")){
          install.packages("lsa")
        }
        library("lsa")
      stand_rowA <- as.vector(scale(rowA[joint_values]))
      stand_rowB <- as.vector(scale(rowB[joint_values]))               
      return(cosine(stand_rowA, stand_rowB))
      }
    }
  }
  
  # Loops over the rows and calculate sall similarities using weight_func
  ################ Note ##############
  # Since similarity weights are symmetric, I decided to only fill the upper
  # triangle in order to save computation time and space.
  for(i in 1:(nrow(data)-1)) {
    weight_mat[i, i:nrow(data) ] <- apply(data[i:nrow(data),], 1, weight_func, data[i, ])
  }
  weight_mat <- as.matrix(Matrix::forceSymmetric(round(weight_mat,4), uplo = "U"))
  return(weight_mat)
}


# Calculate significance weighting

calc_significance <- function(data,lower) {
  ## Calculate significance coefficient matrix
  ##
  ## input: data   - movie data or MS data in user-item matrix form
  ##
  ## output: significance weighting matrix
  
  
  # Iniate the similarity weight matrix
  data       <- as.matrix(data)
  sig_weight <- matrix(NA, nrow = nrow(data), ncol = nrow(data))
  data[data==0]  <- NA
  significance_func <- function(rowA, rowB) {
    
    # significance_func takes as input two rows (thought of as rows of the data matrix) and 
    # calculates the similarity between the two rows according to 'method'
    
    joint_values <- !is.na(rowA) & !is.na(rowB)
    k <-  length(rowA[joint_values])
    if (k >= lower) {
      num <- 1
      return(num)
    } 
    else {
      num <- k/lower
      return(num)
    }
    
  }
  
  # Loops over the rows and calculate sall similarities using significance_func
  for(i in 1:nrow(data)) {
    sig_weight[i, ] <- apply(data, 1, significance_func, data[i, ])
  }
  return(sig_weight)
}


calc_weight_var <- function(data, method = "pearson") {
  
  ## Calculate similarity weight matrix
  ##
  ## input: data   - movie data or MS data in user-item matrix form
  ##        method - 'pearson'
  ##
  ## output: similarity weight matrix
  
  
  # Iniate the similarity weight matrix
  library(wCorr)
  
  data       <- as.matrix(data)
  weight_mat_var <- matrix(NA, nrow = nrow(data), ncol = nrow(data))
  
  var_weight <- matrix(NA, ncol = ncol(data))
  mean <- colMeans(data, na.rm = T, dims = 1)
  data_sub_mean <- (data-mean)^2
  vari <- colSums(data_sub_mean, na.rm = T, dims = 1) / (ncol(data_sub_mean)-1)
  
  
  for (i in 1:ncol(data)) {
    max  <- max(data[,i], na.rm=T)
    min  <- min(data[,i], na.rm=T)
    var_weight[i] <- (vari[i] - min)/max
  }
  
  weight_func_var <- function(rowA, rowB) {
    
    # weight_func takes as input two rows (thought of as rows of the data matrix) and 
    # calculates the similarity between the two rows according to 'method'
    
    joint_values <- !is.na(rowA) & !is.na(rowB)
    if (sum(joint_values) == 0) {
      return(0)
    } else {
      if (method == 'pearson') {
        return(weightedCorr(rowA[joint_values], rowB[joint_values], method = "pearson", weights = var_weight[joint_values], ML = FALSE, fast = TRUE))
      }
    }
  }
  
  # Loops over the rows and calculate sall similarities using weight_func
  for(i in 1:nrow(data)) {
    weight_mat_var[i, ] <- apply(data, 1, weight_func_var, data[i, ])
    print(i)
  }
  return(round(weight_mat_var, 4))
}

pred_matrix <- function(data, simweights) {
  
  ## Calculate prediction matrix
  ##
  ## input: data   - movie data or MS data in user-item matrix form
  ##        simweights - a matrix of similarity weights
  ##
  ## output: prediction matrix
  
  # Initiate the prediction matrix.
  pred_mat <- data
  
  # Change MS entries from 0 to NA
  pred_mat[pred_mat == 0] <- NA
  
  row_avgs <- apply(data, 1, mean, na.rm = TRUE)
  
  for(i in 1:nrow(data)) {
    
    # Find columns we need to predict for user i and sim weights for user i
    cols_to_predict <- which(is.na(pred_mat[i, ]))
    num_cols        <- length(cols_to_predict)
    neighb_weights  <- simweights[i, ]
    
    # Transform the UI matrix into a deviation matrix since we want to calculate
    # weighted averages of the deviations
    dev_mat     <- data - matrix(rep(row_avgs, ncol(data)), ncol = ncol(data))
    weight_mat  <- matrix(rep(neighb_weights, ncol(data)), ncol = ncol(data))
    
    weight_sub <- weight_mat[, cols_to_predict]
    dev_sub    <- dev_mat[ ,cols_to_predict]
    
    pred_mat[i, cols_to_predict] <- row_avgs[i] +  apply(dev_sub * weight_sub, 2, sum, na.rm = TRUE)/sum(neighb_weights, na.rm = TRUE)
    print(i)
  }
  
  return(pred_mat)
}

#######################################
# Match matrix function
# Input: Full matrix = predicted matrix
#        small matrix = test matrix
# Output: return a small matrix, with values from predicted matrix
#######################################
match_the_matrix <- function(small_matrix, full_matrix){
  
  small_matrix <- full_matrix[rownames(full_matrix) %in% rownames(small_matrix), 
                              colnames(full_matrix) %in% colnames(small_matrix)]
  return(small_matrix)
}

#######################################
# Rank matrix function (helper function)
# 
# Input: observed matrix and predicted matrix, they should be in same dim
#        
# Output: return the ranked test set matrix, based on predicted vote values.
#######################################
rank_matrix <- function(pred_matrix, observed_matrix){
  
  result = matrix(NA, nrow(observed_matrix), ncol(observed_matrix))
  
  for (i in 1:nrow(observed_matrix)){
    
    # sort predicted values for each row
    sorted_pred = sort(pred_matrix[i,], decreasing=TRUE) 
    
    # sort observed values based on predicted values.
    sorted_obs = unlist(observed_matrix[i,][names(sorted_pred)])
    
    # save the ranked row in the new matrix.
    result[i,] = unname(sorted_obs)
  }
  rownames(result) = rownames(observed_matrix)
  return(result)
}

#######################################
# Ranked Scroing function
# Input: predicted matrix
#        observed matrix
#        alpha value
#        These matrices need to be in same dim
# Output: return the ranked score for the predicted matrix
#######################################

ranked_scoring <- function(pred_matrix, observed_matrix, alpha,d){
  
  # First we want to match the dimensions of the two matrix
  pred_matrix <- match_the_matrix(observed_matrix, pred_matrix)
  # Also, we want to strip the values of 1 given by the train set from the predicted matrix
  pred_matrix <- ifelse(pred_matrix==1,0,pred_matrix)
  
  # ranked matrix of the observed_matrix
  ranked_mat = rank_matrix(pred_matrix, observed_matrix)
  adjust = ifelse(ranked_mat - d > 0, ranked_mat - d, 0)
  
  nrow = nrow(ranked_mat)
  ncol = ncol(ranked_mat)
  
  # denominator of r_a & r_a_max
  denominator_mat = matrix(rep(2^(0:(ncol-1)/(alpha-1)), nrow), nrow, ncol, byrow=T)
  
  # Get a vector of r_a
  utility_matrix = adjust/denominator_mat
  r_a_vector = rowSums(utility_matrix)
  
  # Rank the observed_matrix for r_a max in order to have the maximum achievable utility.
  r_a_max_matrix = t(apply(observed_matrix, 1, sort,decreasing=T))
  
  # Get a vector of r_a max
  max_utility_matrix = r_a_max_matrix/denominator_mat
  r_a_max_vector = rowSums(max_utility_matrix)
  
  # Obtain the r_a / r_a_max score
  r = 100 * sum(r_a_vector)/sum(r_a_max_vector)
  
  return(r)
  
}

#######################################
# MAE function
# Input: predicted matrix
#        observed matrix
#        alpha value
#        These matrices need to be in same dim
# Output: return the Mean Absolute Error
#######################################
MAE <- function(pred_matrix, observed_matrix){
  if(!require("hydroGOF")){
  install.packages("hydroGOF")
  }
  library("hydroGOF")
  
  # First we want to match the dimensions of the two matrix
  pred_matrix <- match_the_matrix(observed_matrix, pred_matrix)
  # Since the function mae calculates the absolute deviance by column, we transpose the matrices
  score <- hydroGOF::mae(t(pred_matrix),t(observed_matrix),na.rm = TRUE)
  return(mean(score))
}
