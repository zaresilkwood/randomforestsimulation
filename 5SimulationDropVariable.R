###########################################################################################
#CONTINUOUS VARIABLE, CHANGING MEAN
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=5

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group1 <- normalgroup1(1500, m, 0, 1, 0)
  group2 <- normalgroup2(1000, m, 0, 0.1, 1, 1)
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
                  paste("continuous_mean_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group1 <- normalgroup1(1500, m, 0, 1, 0)
  group2 <- normalgroup2(1000, m, 0, 0.2, 1, 1)
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
                  paste("continuous_mean_remove2",i,".png", sep = ""))
}

#compare simulation 1 and simulation 2
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                paste("continuous_mean_remove",i,".png", sep = ""))
}

###########################################################################################
#CONTINUOUS VARIABLE, CHANGING SD
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=5

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group1 <- normalgroup1(1500, m, 0, 1, 0)
  group2 <- normalgroup3(1000, m, 0, 1, 0.1, 1)
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
                  paste("continuous_sd_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group1 <- normalgroup1(1500, m, 0, 1, 0)
  group2 <- normalgroup3(1000, m, 0, 1, 0.2, 1)
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
                  paste("continuous_sd_remove2",i,".png", sep = ""))
}

#compare simulation 1 and simulation 2
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("continuous_sd_remove",i,".png", sep = ""))
}

###########################################################################################
#DISRETE VARIABLE, CHANGING PROBABILITY
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=5

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group1 <- binomgroup1(1500,m,1,0.5,0)
  group2 <- binomgroup2(1000,m,1,0.5,0.7,1)
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
                  paste("discrete_prob_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group1 <- binomgroup1(1500,m,1,0.5,0)
  group2 <- binomgroup2(1000,m,1,0.5,0.8,1)
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
                  paste("discrete_prob_remove2",i,".png", sep = ""))
}

#compare simulation 1 and simulation 2
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("discrete_prob_remove",i,".png", sep = ""))
}

###########################################################################################
#DISCRETE VARIABLE, CHANGING NUMBER OF TRIAL
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=5

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group1 <- binomgroup1(1500,m,1,0.5,0)
  group2 <- binomgroup3(1000,m,1,5,0.5,1)
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
                  paste("discrete_trial_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group1 <- binomgroup1(1500,m,1,0.5,0)
  group2 <- binomgroup3(1000,m,1,9,0.5,1)
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
                  paste("discrete_trial_remove2",i,".png", sep = ""))
}

#compare simulation 1 and simulation 2
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("discrete_trial_remove",i,".png", sep = ""))
}

###########################################################################################
#CATEGORICAL VARIABLE
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=5

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group1 <- categoricalgroup2(1500, m, 1, 2, 1, 0)
  group2 <- categoricalgroup3(1000, m, 1, 2, 1, 1)
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
                  paste("categorical_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group1 <- categoricalgroup2(1500, m, 1, 2, 2, 0)
  group2 <- categoricalgroup3(1000, m, 1, 2, 2, 1)
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
                  paste("categorical_remove2",i,".png", sep = ""))
}

#compare simulation 1 and simulation 2
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("categorical_remove",i,".png", sep = ""))
}

###########################################################################################
#COMBINED VARIABLE FIRST VERSION
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=9

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 3, 0, 1, 0)
  group11 <- group11[,1:3]
  group21 <- normalgroup2(1000, 3, 0, 0.1, 1, 1)
  group21 <- group21[,1:3]
  
  group12 <- binomgroup1(1500, 3, 1, 0.5, 0)
  group12 <- group12[,1:3]
  group22 <- binomgroup2(1000, 3, 1, 0.5, 0.7, 1)
  group22 <- group22[,1:3]
  
  group13 <- categoricalgroup2(1500, 3, 1, 2, 1, 0)
  group23 <- categoricalgroup3(1000, 3, 1, 2, 1, 1)
  
  group1 <- cbind(group11, group12, group13)
  group2 <- cbind(group21, group22, group23)
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
                  paste("combined_1_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 3, 0, 1, 0)
  group11 <- group11[,1:3]
  group21 <- normalgroup2(1000, 3, 0, 0.2, 1, 1)
  group21 <- group21[,1:3]
  
  group12 <- binomgroup1(1500, 3, 1, 0.5, 0)
  group12 <- group12[,1:3]
  group22 <- binomgroup2(1000, 3, 1, 0.5, 0.8, 1)
  group22 <- group22[,1:3]
  
  group13 <- categoricalgroup2(1500, 3, 1, 2, 2, 0)
  group23 <- categoricalgroup3(1000, 3, 1, 2, 2, 1)
  
  group1 <- cbind(group11, group12, group13)
  group2 <- cbind(group21, group22, group23)
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
                  paste("combined_1_remove2",i,".png", sep = ""))
}

#compare simulation 1 and simulation 2
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("combined_1_remove",i,".png", sep = ""))
}

###########################################################################################
#COMBINED VARIABLE, ANOTHER VERSION
###########################################################################################

#n = number of loop
n=10
#m = number of variable
m=9

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 3, 0, 1, 0)
  group11 <- group11[,1:3]
  group21 <- normalgroup3(1000, 3, 0, 1, 0.1, 1)
  group21 <- group21[,1:3]
  
  group12 <- binomgroup1(1500, 3, 1, 0.5, 0)
  group12 <- group12[,1:3]
  group22 <- binomgroup3(1000, 3, 1, 3, 0.5, 1)
  group22 <- group22[,1:3]
  
  group13 <- categoricalgroup2(1500, 3, 1, 2, 1, 0)
  group23 <- categoricalgroup3(1000, 3, 1, 2, 1, 1)
  
  group1 <- cbind(group11, group12, group13)
  group2 <- cbind(group21, group22, group23)
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
                  paste("combined_2_remove1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group11 <- normalgroup1(1500, 3, 0, 1, 0)
  group11 <- group11[,1:3]
  group21 <- normalgroup3(1000, 3, 0, 1, 0.2, 1)
  group21 <- group21[,1:3]
  
  group12 <- binomgroup1(1500, 3, 1, 0.5, 0)
  group12 <- group12[,1:3]
  group22 <- binomgroup3(1000, 3, 1, 5, 0.5, 1)
  group22 <- group22[,1:3]
  
  group13 <- categoricalgroup2(1500, 3, 1, 2, 2, 0)
  group23 <- categoricalgroup3(1000, 3, 1, 2, 2, 1)
  
  group1 <- cbind(group11, group12, group13)
  group2 <- cbind(group21, group22, group23)
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
                  paste("combined_2_remove2",i,".png", sep = ""))
}

#compare simulation 1 and simulation 2
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("combined_2_remove",i,".png", sep = ""))
}

###########################################################################################
#CORRELATED VARIABLE
###########################################################################################
#m = number of variable
m <- 5
sigmaall<-matrix(runif(m^2, 0.7, 0.8), ncol=m)
sigmaall[lower.tri(sigmaall)] = t(sigmaall)[lower.tri(sigmaall)]
diag(sigmaall) <- rep(1, m)
sigmaall

sigmaall_cor <- data.frame(cov2cor(sigmaall)) %>% 
  round(3)
colnames(sigmaall_cor) <- paste("X",seq(1:ncol(sigmaall_cor)), sep = "")
sigmaall_cor <- cbind(Variable = colnames(sigmaall_cor), sigmaall_cor)
sigmaall_cor


#n = number of loop
n=10

#Simulation 1
result_matrix1 <- list()
result_importance1 <- list()
for (j in 1:n) {
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,m), Sigma = sigmaall,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0, by = 0.1, length.out = m),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  
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
                  paste("correlated_mean1",i,".png", sep = ""))
}

#############
#Simulation 2
result_importance2 <- list()
result_matrix2 <- list()
for (j in 1:n) {
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,m), Sigma = sigmaall,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0, by = 0.2, length.out = m),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  
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
                  paste("correlated_mean2",i,".png", sep = ""))
}

#compare simulation 1 and simulation 2
for (i in 1:m) {
  compareplot_imp(result_matrix_fix1[[i]], result_matrix_fix2[[i]],
                  paste("correlated_mean",i,".png", sep = ""))
}
