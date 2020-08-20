library(MASS)
library(randomForest)
library(tidyverse)

## Sigma ##
n <- 5
sigmaall<-matrix(runif(n^2, 0.7, 0.8), ncol=n)
sigmaall[lower.tri(sigmaall)] = t(sigmaall)[lower.tri(sigmaall)]
diag(sigmaall) <- rep(1, n)
sigmaall

sigmaall_cor <- data.frame(cov2cor(sigmaall)) %>% 
  round(3)
colnames(sigmaall_cor) <- paste("X",seq(1:ncol(sigmaall_cor)), sep = "")
sigmaall_cor <- cbind(Variable = colnames(sigmaall_cor), sigmaall_cor)
sigmaall_cor

write.csv(sigmaall_cor, "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/csv/cor_sigmaall.csv",
          col.names = T, row.names = F)

#FOR MANY ITERATION 1
cornorm1 <- list()

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmaall,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0, by = 0.1, length.out = n),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_data1 <- rbind(group1, group2)
  cornorm_rf1 <- randomForest(y~., data = cornorm_data1, importance = T,
                             localImp = T)
  cornorm1[[i]] <- importance(cornorm_rf1, type = 1, scale = F)
}

#FOR MANY ITERATION 2
cornorm2 <- list()

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmaall,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0, by = 0.2, length.out = n),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_data2 <- rbind(group1, group2)
  cornorm_rf2 <- randomForest(y~., data = cornorm_data2, importance = T,
                             localImp = T)
  cornorm2[[i]] <- importance(cornorm_rf2, type = 1, scale = F)
}

compareplot_imp(cornorm1,cornorm2,"corelatednorm.png")


#WHAT IF WE CHANGE THE ORDER??

#FOR MANY ITERATION 1
cornorm1a <- list()

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,5), Sigma = sigmaall,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0.4, by = -0.1,length.out = 5),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_data1a <- rbind(group1, group2)
  cornorm_rf1a <- randomForest(y~., data = cornorm_data1a, importance = T,
                              localImp = T)
  cornorm1a[[i]] <- importance(cornorm_rf1a, type = 1, scale = F)
}

#FOR MANY ITERATION 2
cornorm2a <- list()

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,5), Sigma = sigmaall,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0.8, by = -0.2, length.out = 5),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_data2a <- rbind(group1, group2)
  cornorm_rf2a <- randomForest(y~., data = cornorm_data2a, importance = T,
                              localImp = T)
  cornorm2a[[i]] <- importance(cornorm_rf2a, type = 1, scale = F)
}

compareplot_imp(cornorm1a,cornorm2a,"corelatednorm_desc.png")


#WHAT IF THE SIGMA FIX?

## Sigma Fix Check##

n <- 5
sigmaall <- matrix(rep(0.75, n^2), nrow = n)
diag(sigmaall) <- rep(1,n)
sigmaall

sigmaall_csv <- data.frame(sigmaall) %>% 
  round(3)
colnames(sigmaall_csv) <- paste("X",seq(1:ncol(sigmaall_csv)), sep = "")
sigmaall_csv <- cbind(Variable = colnames(sigmaall_csv), sigmaall_csv)
sigmaall_csv

write.csv(sigmaall_csv, "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/csv/sigmaall_fix.csv",
          col.names = T, row.names = F)

#FOR MANY ITERATION 1
cornorm1 <- list()

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmaall,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0, by = 0.1, length.out = n),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_data1 <- rbind(group1, group2)
  cornorm_rf1 <- randomForest(y~., data = cornorm_data1, importance = T,
                              localImp = T)
  cornorm1[[i]] <- importance(cornorm_rf1, type = 1, scale = F)
}

#FOR MANY ITERATION 2
cornorm2 <- list()

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmaall,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0, by = 0.2, length.out = n),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_data2 <- rbind(group1, group2)
  cornorm_rf2 <- randomForest(y~., data = cornorm_data2, importance = T,
                              localImp = T)
  cornorm2[[i]] <- importance(cornorm_rf2, type = 1, scale = F)
}

compareplot_imp(cornorm1,cornorm2,"corelatednorm_sigmafix.png")