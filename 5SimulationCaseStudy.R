library(MASS)
library(randomForest)
library(tidyverse)

#Basic information about data
str(sample_kaggle)
summary(sample_kaggle)

#Extract categorical data structure detail
cat_kaggle <- sample_kaggle[,-c(5,9,10)]
cat_kaggle0 <- cat_kaggle[cat_kaggle$TARGET == 0,]
cat_kaggle1 <- cat_kaggle[cat_kaggle$TARGET == 1,]

name_catkaggle0 <- colnames(cat_kaggle0[1:11])
prob_catkaggle0 <- list()
for (i in 1:(ncol(cat_kaggle0)-1)) {
  current_sample <- cat_kaggle0[[i]]
  prob_catkaggle0[[i]] <- summary(current_sample)/sum(summary(current_sample))
}

name_catkaggle1 <- colnames(cat_kaggle1[1:11])
prob_catkaggle1 <- list()
for (i in 1:(ncol(cat_kaggle1)-1)) {
  current_sample <- cat_kaggle1[[i]]
  prob_catkaggle1[[i]] <- summary(current_sample)/sum(summary(current_sample))
}

#Extract numerical data structure detail
num_analysisdata <- sample_kaggle[,c(5,9,10)]
cor_analysisdata <- cor(num_analysisdata)
cov_analysisdata <- cov(num_analysisdata)

num_kaggle <- sample_kaggle[,c(5,9,10,15)]
num_kaggle0 <- num_kaggle[num_kaggle$TARGET == 0,] 
num_kaggle1 <- num_kaggle[num_kaggle$TARGET == 1,]

mean_kaggle0 <- num_kaggle0[1:3] %>% colMeans()
mean_kaggle1 <- num_kaggle1[1:3] %>% colMeans()

simulation_case <- list()

#Loop the simulation
for (loop in 1:20) {
  #Generate 3 continuous variable
  #target variable 0
  numerical_datasim0 <- data.frame(mvrnorm(n = 1500, mu = mean_kaggle0, Sigma = cov_analysisdata,
                                           empirical = F))
  
  #target variable 1
  numerical_datasim1 <- data.frame(mvrnorm(n = 1000, mu = mean_kaggle1, Sigma = cov_analysisdata,
                                           empirical = F))
  
  #Generate 11 categorical variable with different level
  #target variable 0
  cat_datasim0 <- list() 
  for (i in 1:length(prob_catkaggle0)) {
    n_cat <- length(prob_catkaggle0[[i]])
    cat_datasim0[[i]] <- sample(LETTERS[1:n_cat], 1500, replace = TRUE, prob = prob_catkaggle0[[i]])
  }
  categorical_datasim0 <- data.frame((do.call("cbind", cat_datasim0)))
  colnames(categorical_datasim0) <- name_catkaggle0
  categorical_datasim0$TARGET <- factor(rep(0,1500))
  
  #target variable 1
  cat_datasim1 <- list() 
  for (i in 1:length(prob_catkaggle1)) {
    n_cat <- length(prob_catkaggle1[[i]])
    cat_datasim1[[i]] <- sample(LETTERS[1:n_cat], 1000, replace = TRUE, prob = prob_catkaggle1[[i]])
  }
  categorical_datasim1 <- data.frame((do.call("cbind", cat_datasim1)))
  colnames(categorical_datasim1) <- name_catkaggle1
  categorical_datasim1$TARGET <- factor(rep(1,1000))
  
  #Combine all variable to create full dataset
  variable0_sim <- cbind(numerical_datasim0, categorical_datasim0)
  variable1_sim <- cbind(numerical_datasim1, categorical_datasim1)
  
  simulation_cs <- rbind(variable0_sim, variable1_sim)
  
  #Apply the random forest and extract the importance result
  simulation_csrf <- randomForest(TARGET~., data = simulation_cs, importance = T,
                                  localImp = T)
  
  simulation_case[[loop]] <- importance(simulation_csrf, type = 1, scale = F)
}

plot_imp_list(simulation_case, "simulationcs.png")

#Taking the last simulated data and compare it with sample dataset
str(simulation_cs)
str(sample_kaggle)

#The result, there are some different on level, but that is because it has very small
#probability for EUDCATION TYPE and JOB