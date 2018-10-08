

# f1 update the scores of userA and userB based on the current object score matrix
f1 <- function(rowA, rowB,s_obj){
  p_user <- rowA %*% t(rowB)
  if(sum(p_user) < 0.3* nrow(p_user)){
    return(0)
  }else{
  len_x <- sum(rowA)
  len_y <- sum(rowB)
  if(len_x * len_y ==0){
    return(0)
  }else{
    return(sum(s_obj * p_user)*C1/(len_x*len_y))
    }
  }
}
# f2 update the scores of objA and objB based on the current user score matrix
f2 <- function(colA, colB,s_user){
  p_obj <- colA %*% t(colB)
  len_a <- sum(colA)
  len_b <- sum(colB)
  if(len_a * len_b == 0){
    return(0)
  }else{
    return(sum(s_user * p_obj) * C2/(len_a * len_b))
  }
}

simrank <- function(data){
  # First construct the user and object score matrix
  s_user <- diag(x = 1, nrow(data), nrow(data))
  s_obj <- diag(x = 1, ncol(data), ncol(data))
  

  # Iteratively calculate the two matrices, K = 5
  for(k in 1:K){
    # Run a loop and use apply function to update the scores using f1 and f2.
    for(i in 1:(nrow(data)-1)) {
      s_user[i, i:nrow(s_user) ] <- apply(data[i:nrow(data),], 1, f1, data[i, ],s_obj)
      if( i %% 10==0){
        print(i)
      }
    }
    diag(s_user) <- 1
    # Since we only filled in the upper triangle, copy and paste to the lower triangle
    s_user <- as.matrix(Matrix::forceSymmetric(s_user, uplo = "U"))
    
    for(i in 1:(ncol(data)-1)) {
      s_obj[i, i:ncol(s_obj) ] <- apply(data[,i:ncol(data)], 2, f2, data[, i],s_user)

    }
    diag(s_obj) <- 1
    # Since we only filled in the upper triangle, copy and paste to the lower triangle
    # s_obj <- as.matrix(Matrix::forceSymmetric(s_obj, uplo = "U"))
    print(c("k=",k))
    print(s_user)
  }
  return(s_user)
}


compute_user_cum_movie_sum = function(m1,m2,
                                      Movie_Matrix_ = Movie_Matrix) {
  
  return(Movie_Matrix_[m1,m2])
}

compute_user_sim = function(j, k, iter = i,
                            train_adj_ = train_adj,
                            Movie_Matrix_ = Movie_Matrix,
                            Users_ = Users,
                            Movies_ = Movies) {
  
  if (j == k) {
    return(1)
  } else {
    moviesj = train_adj_$Movie[which(train_adj_$User == Users_[j])]
    moviesk = train_adj_$Movie[which(train_adj_$User == Users_[k])]
    
    # Pruning
    if (length(moviesj) + length(moviesk) - 
        length(unique(c(moviesj,moviesk))) <= 0.25 * length(moviesj)) {
      return(0)
    }
    
    # First Iter
    if (iter == 1) {
      return((length(moviesj) + length(moviesk) - 
                length(unique(c(moviesj,moviesk)))) * 0.8 / length(moviesk) / length(moviesj))
    }
    
    m1s = match(sort(rep(moviesj,length(moviesk))),Movies_)
    m2s = match(rep(moviesk,length(moviesj)),Movies_)
    
    mins = (m1s+m2s - sqrt((m1s-m2s)^2) )/2
    maxs = (m1s+m2s + sqrt((m1s-m2s)^2) )/2
    
    cum_sum = sum(mapply(compute_user_cum_movie_sum,
                         m1 = maxs, 
                         m2 = mins), na.rm=T)
    
    cum_sum = cum_sum * 0.8 / length(mins)
    return(cum_sum)
  }
}

compute_movie_cum_user_sum = function(m1,m2,
                                      User_Matrix_ = User_Matrix) {
  
  return(User_Matrix_[m1,m2])
}

compute_movie_sim = function(j, k, 
                             train_adj_ = train_adj,
                             User_Matrix_ = User_Matrix,
                             Users_ = Users,
                             Movies_ = Movies) {
  
  if (j == k) {
    return(1)
  } else {
    usersj = train_adj_$User[which(train_adj_$Movie == Movies_[j])]
    usersk = train_adj_$User[which(train_adj_$Movie == Movies_[k])]
    
    if (length(usersj) + length(usersk) - 
        length(unique(c(usersj,usersk))) <= 0.25 * length(usersj)) {
      return(0)
    }
    
    m1s = match(sort(rep(usersj,length(usersk))),Users_)
    m2s = match(rep(usersk,length(usersj)),Users_)
    
    mins = (m1s+m2s - sqrt((m1s-m2s)^2) )/2
    maxs = (m1s+m2s + sqrt((m1s-m2s)^2) )/2
    
    cum_sum = sum(mapply(compute_movie_cum_user_sum,
                         m1 = maxs, 
                         m2 = mins),na.rm=T)
    
    cum_sum = cum_sum * 0.8 / length(mins)
    return(cum_sum)
  }
}

generate_simrank_rdata = function(filename="simrank_matrix") {
  
  train = read.csv("./data/Proj3_Data/eachmovie_sample/data_train.csv")
  test = read.csv("./data/Proj3_Data/eachmovie_sample/data_test.csv")
  
  
  train$rescore = 0
  train$rescore[which(train$Score >= 6)] = 1
  
  train_adj = train[which(train$rescore ==1),]
  
  Movies = sort(unique(train_adj$Movie))
  Users = unique(train_adj$User)
  
  Movie_Matrix = diag(length(Movies))
  User_Matrix = diag(length(Users))
  
  for( i in 1:5 ) {
    print(paste0("i = ",i))
    for (j in 1:length(Users)) {
      if (j %% 100 == 0) {
        print(Sys.time())
        print(paste0("Users j=",j))
      }
      User_Matrix[j,1:j] = mapply(compute_user_sim,j=j,k=1:j)
    }
    
    for (j in 1:length(Movies)) {
      if (j %% 50 == 0) {
        print(Sys.time())
        print(paste0("Movies j=",j))
      }
      Movie_Matrix[j,1:j] = mapply(compute_movie_sim,j=j,k=1:j)
    }
    
  }
  
  User_Matrix[upper.tri(User_Matrix)] <- t(User_Matrix)[upper.tri(User_Matrix)]
  
  save(User_Matrix,Users,file=paste0("../output/",filename,".RData"))
  
}


# 
# 
# 
#   j <- !is.na(rowA) | !is.na(rowB)
#   rowA <- rowA[j]
#   rowB <- rowB[j]
#   if(length(rowA) <= 1){
#     score <- 0
#   }else{
#     # Convert to 1/0 vector
#     rowA <- ifelse(is.na(rowA), 0, 1)
#     rowB <- ifelse(is.na(rowB), 0, 1)
#     
#     # First construct the user and object score matrix
#     s_user <- diag(x = 1, 2,2)
#     s_obj <- diag(x = 1, length(rowA), length(rowB))
#     p_user <- rowA %*% t(rowB)
#     # Find |O(X)|,|O(Y)|
#     len_x <- sum(rowA)
#     len_y <- sum(rowB)
#     # Iteratively update the scores of user and objects
#     for(k in 1:(K+1)){
#       # Calculate s_user
#       s_user[1,2]=s_user[2,1] <- sum(s_obj * p_user)*C1/(len_x*len_y)
#       
#       # Update s_obj
#       for(row_i in 1:nrow(s_obj)){
#         for(col_j in 1:ncol(s_obj)){
#           if(row_i == col_j){
#             s_obj[row_i, col_j] <- 1
#           }else{
#             I_a <- as.vector(c(rowA[[row_i]],rowB[[row_i]]))
#             I_b <- as.vector(c(rowA[[col_j]], rowB[[col_j]]))
#             p_obj <- I_a %*% t(I_b)
#             s_obj[row_i, col_j] <- sum(s_user * p_obj) * C2/(sum(I_a) * sum(I_b))
#           }
#         }
#       }
#     }
#     score <- s_user[1,2]
#     
#   }
# 
# 
# 
# system.time(weight_func(rowA, rowB))
# 
# data       <- as.matrix(movie_UI[1:10,])
# weight_mat <- matrix(NA, nrow = nrow(data), ncol = nrow(data))
# weight_mat2 <- diag(x = 1, nrow(data), nrow(data))
# weight_func <- function(rowA, rowB) {
#   
#   # weight_func takes as input two rows (thought of as rows of the data matrix) and 
#   # calculates the similarity between the two rows according to 'method'
#   
#   joint_values <- !is.na(rowA) & !is.na(rowB)
#   return(cor(rowA[joint_values], rowB[joint_values], method = 'pearson'))
#   }
# 
# # Loops over the rows and calculate sall similarities using weight_func
# system.time(
#   for(i in 1:(nrow(data)-1)) {
#   weight_mat2[i, i:nrow(data) ] <- apply(data[i:nrow(data),], 1, weight_func, data[i, ])
#   print(i)
#   }
# )
# system.time(
# for(i in 1:nrow(data)) {
#   weight_mat[i, ] <- apply(data, 1, weight_func, data[i, ])
#   print(i)
# }
# )


