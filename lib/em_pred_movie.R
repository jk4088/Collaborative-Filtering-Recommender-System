em_pred_movie <- function(data, gamma, assign_mat) {
  
n_user <- nrow(data)
n_movie <- ncol(data)
  
  #######PREDICTION - Movie Data##########
  
  # exp_clust <- matrix(0, nrow = n_user, ncol = C)
  exp_score_mat <- matrix(0, nrow = n_user, ncol = n_movie)
  
  for (n in 1:n_user) {
    max_clust <- which.is.max(assign_mat[n,]) #This gives the most likely cluster for that particular user
    # exp_clust[n, max_clust] <- 1
    
    for (m in 1:n_movie) {
        score <- which.is.max(gamma[,max_clust,m])
        exp_score_mat[n,m] <- score
    }
  }
  return(exp_score_mat)
}
