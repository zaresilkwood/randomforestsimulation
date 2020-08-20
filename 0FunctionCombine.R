########################################################################
#FUNCTION TO GENERATE DATA NORMAL AND UNIFORM
simulation_function <- function(n_observation,
                                n_normal_var, mean_normal_var, sd_normal_var,
                                n_uni_var, min_uni_var, max_uni_var){
  
  #Random variable for normal distribution
  list_normal <- list()
  variable_normal <- list()
  for (i in 1:n_normal_var) {
    variable_normal[[i]] <- rnorm(n_observation, mean = i*mean_normal_var,
                                  sd = i*sd_normal_var)
    list_normal[i] <- paste("xn",i, sep = "")
  }
  list_normal <- unlist(list_normal, use.names=FALSE)
  variable1 <- do.call(cbind, variable_normal)
  colnames(variable1) <- list_normal
  
  #Random variable for categorical data
  list_uni <- list()
  variable_uni <- list()
  for (i in 1:n_uni_var) {
    variable_uni[[i]] <- as.factor(round(runif(n_observation, min = min_uni_var,
                                               max = i*max_uni_var)))
    list_uni[i] <- paste("xu",i, sep = "")
  }
  list_uni <- unlist(list_uni, use.names=FALSE)
  variable2 <- data.frame(variable_uni)
  colnames(variable2) <- list_uni
  
  #COMBINE BOTH COLUMNS
  return(data.frame(variable1, variable2)) 
  
}
########################################################################

########################################################################
#FUNCTION TO GENERATE DATA NORMAL
normal_sim_function <- function(n_observation,
                                n_normal_var, mean_normal_var, sd_normal_var){
  
  #Random variable for normal distribution
  list_normal <- list()
  variable_normal <- list()
  for (i in 1:n_normal_var) {
    variable_normal[[i]] <- rnorm(n_observation, mean = i*mean_normal_var,
                                  sd = i*sd_normal_var)
    list_normal[i] <- paste("xn",i, sep = "")
  }
  list_normal <- unlist(list_normal, use.names=FALSE)
  variable1 <- do.call(cbind, variable_normal)
  colnames(variable1) <- list_normal
  return(data.frame(variable1))

}
########################################################################

#Test
simulationdata5normal <- normal_sim_function(10000, 5, 100,50)

########################################################################
#FUNCTION TO GENERATE DATA UNIFORM
cate_sim_function <- function(n_observation,
                              n_uni_var, min_uni_var, max_uni_var){

  #Random variable for categorical data
  list_uni <- list()
  variable_uni <- list()
  for (i in 1:n_uni_var) {
    variable_uni[[i]] <- as.factor(round(runif(n_observation, min = min_uni_var,
                                               max = i*max_uni_var)))
    list_uni[i] <- paste("xu",i, sep = "")
  }
  list_uni <- unlist(list_uni, use.names=FALSE)
  variable2 <- data.frame(variable_uni)
  colnames(variable2) <- list_uni
  return(variable2)
}
########################################################################
#TEST
simulationdata6uni <- cate_sim_function(10000, 5, 1, 2)
summary(simulationdata5normal)
summary(simulationdata6uni)


########################################################################
#FUNCTION TO COMPARE TWO DATASET WITH RANDOM FOREST
compare_function <- function(dataset1, dataset2){
  library(randomForest)
  simulate_random1 <- randomForest(y~., data = dataset1, importance = TRUE)
  simulate_random2 <- randomForest(y~., data = dataset2, importance = TRUE)
  ImportanceComparison <- data.frame(importance(simulate_random1, type = 1),
                                     importance(simulate_random2, type = 1),
                                     importance(simulate_random1, type = 2),
                                     importance(simulate_random2, type = 2))
  colnames(ImportanceComparison) <- c("MeanDecrAcc1", "MeanDecrAcc2",
                                      "MeanDecrGini1", "MeanDecrGini2")
  return(ImportanceComparison)
}
########################################################################

