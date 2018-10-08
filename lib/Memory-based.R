
###################################################################
### Memory-based Collaborative Filtering Algorithm ################
###################################################################


########################################################
######## Building the UI matrix for the MS Data ########
########################################################

# install.packages("DescTools")
library("DescTools")
# install.packages("infotheo")
library("infotheo")


setwd("/Users/wcheng/Desktop/Spring 2018/data science/project-3-algorithms-project-3-algorithms-group-2")
source("./lib/functions.R")

dir_MS <- "/data/Proj3_Data/MS_sample/"
getwd()

# Load the data
MS_test <- read.csv(paste(getwd(),dir_MS, "data_test.csv", sep = ""), as.is = TRUE, header = TRUE)
MS_test <- MS_test[, 2:4]




# Transform from narrow to wide, i.e. user-item matrix 
# using MS_data_transform function

# Below takes 2.17 minutes
MS_UI <- MS_data_transform(MS_train)
save(MS_UI, file = "./output/MS_UI.RData")

###############################################################
#################### Constants definition #####################
###############################################################

run.pearson <- F
run.entropy <- F
run.spearman <- F
run.sqdiff <- F
run.cosin <- F

# Constants for SimRank calculations
C1 <- 0.8
C2 <- 0.8
K <- 5



###############################################################
######## Building the UI matrix for the EachMovie Data ########
###############################################################

dir_mov <- "/data/Proj3_Data/eachmovie_sample/"

# Load the data
movie_train <- read.csv(paste(getwd(),dir_mov, "data_train.csv", sep = ""), as.is = TRUE, header = TRUE)
movie_train <- movie_train[, 2:4]


# Compute the full matrix
# Below takes about 4 minutes

movie_UI <- movie_data_transform(movie_train)
save(movie_UI, file = "./output/movie_UI.RData")



# If we want to directly pull out the user-item matrix from the data folder
load("./output/MS_UI.Rdata")
load("./output/movie_UI.Rdata")



#################################################################
######## Calculating the Similarity Weights of the Users ########
#################################################################

# Initiate the similarity weight matrix

movie_UI         <- as.matrix(movie_UI)
movie_sim_weight <- matrix(NA, nrow = nrow(movie_UI), ncol = nrow(movie_UI))


# Calculate the pearson weights on the movie data
# The below took 87 minutes on my Macbook, 35 on my iMac

movie_sim <- calc_weight(movie_UI, run.pearson = T)
save(movie_sim, file = "./output/movie_sim.RData")


# Calculate the pearson weights on the MS data
# The below took 30 minutes on my Macbook and 14 on my iMac

MS_sim <- calc_weight(MS_UI, run.pearson = T)
save(MS_sim, file = "./output/MS_sim.RData")


# Calculate the entropy weights on the movie data
# The below took 46460 seconds

tm_movie_ent <- system.time(movie_ent <- 
                              calc_weight(movie_UI, run.entropy = T))
save(movie_ent, file = "./output/movie_ent.RData")


# Calculate the entropy weights on the MS data
# The below took 51548 seconds

tm_MS_ent <- system.time(MS_ent <- 
                           calc_weight(MS_UI, run.entropy = T))
save(MS_ent, file = "./output/MS_ent.RData")

# Calculate the spearman weights on the movie data
# The below took 3668.39s

tm_movie_spm <- system.time(movie_spm <- 
                              calc_weight(movie_UI,run.spearman = T))
save(movie_spm, file = "./output/movie_spm.RData")


# Calculate the spearman weights on the MS data
# The below took 2071s

tm_MS_spm <- system.time(MS_spm <- 
                           calc_weight(MS_UI, run.spearman = T))
save(MS_spm, file = "./output/MS_spm.RData")

# Calculate the cosin weights on the movie data
# The below took  20808 seconds

tm_movie_cos <- system.time(movie_cos <- 
                              calc_weight(movie_UI,run.cosin = T))
save(movie_cos, file = "./output/movie_cos.RData")


# Calculate the cosin weights on the MS data
# The below took 13891 seconds

tm_MS_cos <- system.time(MS_cos <- 
                           calc_weight(MS_UI, run.cosin = T))
save(MS_cos, file = "./output/MS_cos.RData")

# Calculate the squared difference weights on the movie data
# The below took  4606 seconds

tm_movie_sqd <- system.time(movie_sqd <- 
                              calc_weight(movie_UI,run.sqdiff = T))
save(movie_sqd, file = "./output/movie_sqd.RData")


# Calculate the squared difference weights on the MS data
# The below took 1560 seconds

tm_MS_sqd <- system.time(MS_sqd <- 
                           calc_weight(MS_UI, run.sqdiff = T))
save(MS_sqd, file = "./output/MS_sqd.RData")



###########################################################
######## Calculating the Predictions for the Users ########
###########################################################

load("./output/MS_pred.Rdata")
load("./output/movie_sqd.Rdata")


# Calculate predictions for MS based on pearson correlation
# This calculation took me 15 minutes

MS_pred <- pred_matrix(MS_UI, MS_sim)
save(MS_pred, file = "./output/MS_pred.RData")

# Calculate predictions for movies based on pearson correlation
# This calculation took me 1+ hour

movie_pred <- pred_matrix(movie_UI, movie_sim)
save(movie_pred, file = "./output/movie_pred.RData")

# Calculate predictions for MS based on squared difference similarity
# This calculation took me 15 minutes

MS_pred_sqd <- pred_matrix(MS_UI, MS_sqd)
save(MS_pred_sqd, file = "./output/MS_pred_sqd.RData")

# Calculate predictions for movies based on squared difference similarity
# This calculation took me 1+ hour

movie_pred_sqd <- pred_matrix(movie_UI, movie_sqd)
save(movie_pred_sqd, file = "./output/movie_pred_sqd.RData")


#Time 288.66s
tm_MS_cos_pred <- system.time(MS_cos_predict <- pred_matrix(MS_UI,MS_cos))
save(MS_cos_predict, file = "../output/MS_cos_predict.RData")

#Time 3290.46s
tm_movie_cos <- system.time(movie_cos_predict <-
pred_matrix(movie_UI,movie_cos))
save(movie_cos_predict, file = "../output/movie_cos_predict.RData")

#Time 3059.67s
tm_movie_sqd_pred <- system.time(movie_pred_sqd <- pred_matrix(movie_UI, movie_sqd))
save(movie_pred_sqd, file = "../output/movie_pred_sqd.RData")

#Time: 273.13s
tm_MS_ent_pred <- system.time(MS_pred_ent <- pred_matrix(MS_UI, MS_ent))
save(MS_pred_ent, file = "../output/MS_pred_ent.RData")

#Time: 3361.19s
tm_movie_ent_pred <- system.time(movie_pred_ent <- pred_matrix(movie_UI, movie_ent))
save(movie_pred_ent, file = "../output/movie_pred_ent.RData")

#Time 352.22ss
tm_MS_spm_pred <- system.time(MS_spm_predict <- pred_matrix(MS_UI, MS_spm))
save(MS_spm_predict, file = "../output/MS_spm_predict.RData")

#Time 3228.86s
tm_movie_spm <- system.time(movie_spm_predict <-
pred_matrix(movie_UI,movie_spm))
save(movie_spm_predict, file = "../output/movie_spm_predict.RData")

# Time 1915.88s
tm_movie_spm_var <- system.time(movie_spm_variance <-
calc_weight_var(movie_UI, method = "pearson"))
save(movie_spm_variance, file = "../output/movie_pear_variance.RData")

#Time 791.53s
tm_MS_pear_var <- system.time(MS_pear_variance <-
calc_weight_var(MS_UI, method = "pearson"))
save(MS_pear_variance, file = "../output/MS_pear_variance.RData")

#Time 1336.62s
tm_movie_sig <- system.time(movie_sig <-
calc_significance(movie_UI))
save(movie_sig, file = "../output/movie_sig.RData")

#Time 308.93s
tm_MS_pear_sig_pred <- system.time(MS_pear_sig_pred <- pred_matrix(MS_UI,MS_pear_sig))
save(MS_pear_sig_pred, file = "../output/MS_pear_sig_pred.RData")

#Time 405.97s
tm_MS_pear_var_pred <- system.time(MS_pear_var_pred <- pred_matrix(MS_UI,MS_pear_variance))
save(MS_pear_var_pred, file = "../output/MS_pear_var_pred.RData")

#Time 486.67s
tm_MS_pear_sig_var_pred <- system.time(MS_pear_sig_var_pred <- pred_matrix(MS_UI,MS_pear_sig_var))
save(MS_pear_sig_var_pred, file = "../output/MS_pear_sig_var_pred.RData")

#Time 3314.54s
tm_movie_pear_sig_pred <- system.time(movie_pear_sig_pred <- pred_matrix(movie_UI,movie_pear_sig))
save(movie_pear_sig_pred, file = "../output/movie_pear_sig_pred.RData")

#Time 3255.58s
tm_movie_pear_var_pred <- system.time(movie_pear_var_pred <- pred_matrix(movie_UI,movie_pear_variance))
save(movie_pear_var_pred, file = "../output/movie_pear_var_pred.RData")

#TIme 3287.58s
tm_movie_pear_sig_var_pred <- system.time(movie_pear_sig_var_pred <- pred_matrix(movie_UI,movie_pear_sig_var))
save(movie_pear_sig_var_pred, file = "../output/movie_pear_sig_var_pred.RData")

_


