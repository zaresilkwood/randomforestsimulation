###########################################################################################
#CONTINUOUS (CHANGING MEAN) VS DISCRETE VARIABLE (CHANGING PROBBILITY)
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=8

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.1, 1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.7, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix1[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance1[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix1 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix1[[i]][[j]]
  }
  result_matrix_fix1[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance1, result_matrix_fix1[[i]],
                  paste("continuous_mean_discrete_prob_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.2, 1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.8, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix2[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance2[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix2 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix2[[i]][[j]]
  }
  result_matrix_fix2[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance2, result_matrix_fix2[[i]], 
                  paste("continuous_mean_discrete_prob_remove2",i,".png", sep = ""))
}

#what happen now?
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("continuous_mean_discrete_prob_remove",i,".png", sep = ""))
}


###########################################################################################
#CONTINUOUS (CHANGING MEAN) VS DISCRETE VARIABLE (CHANGING NUMBER OF TRIAL)
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=8

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.1, 1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup3(1000, 4, 1, 4, 0.5, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix1[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance1[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix1 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix1[[i]][[j]]
  }
  result_matrix_fix1[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance1, result_matrix_fix1[[i]],
                  paste("continuous_mean_discrete_trial_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.2, 1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup3(1000, 4, 1, 7, 0.5, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix2[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance2[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix2 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix2[[i]][[j]]
  }
  result_matrix_fix2[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance2, result_matrix_fix2[[i]],
                  paste("continuous_mean_discrete_trial_remove2",i,".png", sep = ""))
}

#what happen now?
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("continuous_mean_discrete_trial_remove",i,".png", sep = ""))
}


###########################################################################################
#CONTINUOUS (CHANGING STANDARD DEVIATION) VS DISCRETE VARIABLE (CHANGING PROBBILITY)
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=8

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.7, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix1[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance1[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix1 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix1[[i]][[j]]
  }
  result_matrix_fix1[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance1, result_matrix_fix1[[i]],
                  paste("continuous_sd_discrete_prob_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.2, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.8, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix2[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance2[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix2 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix2[[i]][[j]]
  }
  result_matrix_fix2[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance2, result_matrix_fix2[[i]],
                  paste("continuous_sd_discrete_prob_remove2",i,".png", sep = ""))
}

#what happen now?
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("continuous_sd_discrete_prob_remove",i,".png", sep = ""))
}


###########################################################################################
#CONTINUOUS (CHANGING STANDARD DEVIATION) VS DISCRETE VARIABLE (CHANGING NUMBER OF TRIAL)
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=8

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup3(1000, 4, 1, 4, 0.5, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix1[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance1[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix1 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix1[[i]][[j]]
  }
  result_matrix_fix1[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance1, result_matrix_fix1[[i]],
                  paste("continuous_sd_discrete_trial_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.2, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup3(1000, 4, 1, 7, 0.5, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix2[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance2[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix2 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix2[[i]][[j]]
  }
  result_matrix_fix2[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance2, result_matrix_fix2[[i]],
                  paste("continuous_sd_discrete_trial_remove2",i,".png", sep = ""))
}

#what happen now?
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("continuous_sd_discrete_trial_remove",i,".png", sep = ""))
}


###########################################################################################
#CONTINUOUS (CHANGING MEAN) VS CATEGORICAL VARIABLE
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=8

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.1, 1, 1)
  group21 <- group21[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 1, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 1, 1)
  
  group1 <- cbind(group11, group13)
  group2 <- cbind(group21, group23)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix1[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance1[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix1 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix1[[i]][[j]]
  }
  result_matrix_fix1[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance1, result_matrix_fix1[[i]],
                  paste("continuous_mean_categorical_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.2, 1, 1)
  group21 <- group21[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 2, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 2, 1)
  
  group1 <- cbind(group11, group13)
  group2 <- cbind(group21, group23)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix2[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance2[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix2 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix2[[i]][[j]]
  }
  result_matrix_fix2[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance2, result_matrix_fix2[[i]],
                  paste("continuous_mean_categorical_remove2",i,".png", sep = ""))
}

#what happen now?
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("continuous_mean_categorical_remove",i,".png", sep = ""))
}


###########################################################################################
#CONTINUOUS (CHANGING STANDARD DEVIATION) VS CATEGORICAL VARIABLE
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=8

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.1, 1)
  group21 <- group21[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 1, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 1, 1)
  
  group1 <- cbind(group11, group13)
  group2 <- cbind(group21, group23)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix1[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance1[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix1 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix1[[i]][[j]]
  }
  result_matrix_fix1[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance1, result_matrix_fix1[[i]],
                  paste("continuous_sd_categorical_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.2, 1)
  group21 <- group21[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 2, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 2, 1)
  
  group1 <- cbind(group11, group13)
  group2 <- cbind(group21, group23)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix2[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance2[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix2 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix2[[i]][[j]]
  }
  result_matrix_fix2[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance2, result_matrix_fix2[[i]],
                  paste("continuous_sd_categorical_remove2",i,".png", sep = ""))
}

#what happen now?
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("continuous_sd_categorical_remove",i,".png", sep = ""))
}


###########################################################################################
#DISCRETE VARIABLE (CHANGING PROBBILITY) VS CATEGORICAL
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=8

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group12 <- group12[,1:4]
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.7, 1)
  group22 <- group22[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 1, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 1, 1)
  
  group1 <- cbind(group12, group13)
  group2 <- cbind(group22, group23)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix1[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance1[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix1 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix1[[i]][[j]]
  }
  result_matrix_fix1[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance1, result_matrix_fix1[[i]],
                  paste("discrete_prob_categorical1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group12 <- group12[,1:4]
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.8, 1)
  group22 <- group22[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 2, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 2, 1)
  
  group1 <- cbind(group12, group13)
  group2 <- cbind(group22, group23)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix2[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance2[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix2 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix2[[i]][[j]]
  }
  result_matrix_fix2[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance2, result_matrix_fix2[[i]],
                  paste("discrete_prob_categorical2",i,".png", sep = ""))
}

#what happen now?
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("discrete_prob_categorical",i,".png", sep = ""))
}


###########################################################################################
#DISCRETE VARIABLE (CHANGING NUMBER OF TRIAL) VS CATEGORICAL
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=8

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group12 <- group12[,1:4]
  group22 <- binomgroup3(1000, 4, 1, 4, 0.5, 1)
  group22 <- group22[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 1, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 1, 1)
  
  group1 <- cbind(group12, group13)
  group2 <- cbind(group22, group23)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix1[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance1[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix1 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix1[[i]][[j]]
  }
  result_matrix_fix1[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance1, result_matrix_fix1[[i]],
                  paste("discrete_trial_categorical1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group12 <- group12[,1:4]
  group22 <- binomgroup3(1000, 4, 1, 7, 0.5, 1)
  group22 <- group22[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 2, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 2, 1)
  
  group1 <- cbind(group12, group13)
  group2 <- cbind(group22, group23)
  
  current_dataset <- rbind(group1, group2)
  rf_result <- list()
  for (i in 1:(ncol(current_dataset)-1)) {
    current_data <- current_dataset[,-i]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    rf_result[[i]] <- importance(current_rf, type = 1, scale = F)
  }
  result_matrix2[[j]] <- rf_result
  normal_rf <- randomForest(y~., data = current_dataset, importance = T,
                            localImp = T)
  result_importance2[[j]] <- importance(normal_rf, type = 1, scale = F)
}

#group similar variable list to the same group
result_matrix_fix2 <- list()
for (j in 1:m) {
  current_list <- list()
  for (i in 1:n) {
    current_list[[i]] <- result_matrix2[[i]][[j]]
  }
  result_matrix_fix2[[j]] <- current_list
}

#plot with comparation without removing the variable 
for (i in 1:m) {
  compareplot_imp(result_importance2, result_matrix_fix2[[i]],
                  paste("discrete_trial_categorical2",i,".png", sep = ""))
}

#what happen now?
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("discrete_trial_categorical",i,".png", sep = ""))
}
