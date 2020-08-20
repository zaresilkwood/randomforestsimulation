#SIGMA

n <- 5
sigmaall<-matrix(runif(n^2, 0.7, 0.8), ncol=n)
sigmaall[lower.tri(sigmaall)] = t(sigmaall)[lower.tri(sigmaall)]
diag(sigmaall) <- rep(1, n)
sigmaall

#FOR MANY ITERATION 1
cornormwithnoise1 <- list()

for (i in 1:10) {

  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmaall, empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000, mu = seq(from = 0, by = 0.1, length.out = n),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_datawithnoise1 <- rbind(group1, group2)
  cornorm_datawithnoise1$random <- rnorm(2500, mean = 0.2, sd = 1)
  cornormwithnoise_rf1 <- randomForest(y~., data = cornorm_datawithnoise1, importance = T,
                              localImp = T)
  cornormwithnoise1[[i]] <- importance(cornormwithnoise_rf1, type = 1, scale = F)
}


#FOR MANY ITERATION 2
cornormwithnoise2 <- list()

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmaall, empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000, mu = seq(from = 0, by = 0.2, length.out = n),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_datawithnoise2 <- rbind(group1, group2)
  cornorm_datawithnoise2$random <- rnorm(2500, mean = 0.4, sd = 1)
  cornormwithnoise_rf2 <- randomForest(y~., data = cornorm_datawithnoise2, importance = T,
                              localImp = T)
  cornormwithnoise2[[i]] <- importance(cornormwithnoise_rf2, type = 1, scale = F)
}


compareplot_imp(cornormwithnoise1,cornormwithnoise2,"corelatednormwithnoise.png")


#NOISE BUT WITH DIFFERENT DESCENDING
#FOR MANY ITERATION 1
cornormwithnoise1b <- list()

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmaall, empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000, mu = seq(from = 0.4, by = -0.1, length.out = n),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_datawithnoise1b <- rbind(group1, group2)
  cornorm_datawithnoise1b$random <- rnorm(2500, mean = 0.2, sd = 1)
  cornormwithnoise_rf1b <- randomForest(y~., data = cornorm_datawithnoise1b, importance = T,
                                       localImp = T)
  cornormwithnoise1b[[i]] <- importance(cornormwithnoise_rf1b, type = 1, scale = F)
}


#FOR MANY ITERATION 2
cornormwithnoise2b <- list()

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmaall, empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000, mu = seq(from = 0.8, by = -0.2, length.out = n),
                               Sigma = sigmaall, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_datawithnoise2b <- rbind(group1, group2)
  cornorm_datawithnoise2b$random <- rnorm(2500, mean = 0.4, sd = 1)
  cornormwithnoise_rf2b <- randomForest(y~., data = cornorm_datawithnoise2b, importance = T,
                                       localImp = T)
  cornormwithnoise2b[[i]] <- importance(cornormwithnoise_rf2b, type = 1, scale = F)
}


compareplot_imp(cornormwithnoise1b,cornormwithnoise2b,
                "corelatednormwithnoise_desc.png")


something <- cornorm_datawithnoise2b[,1:5]
something
cor(something)
