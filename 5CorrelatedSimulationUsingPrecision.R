library(randomForest)
library(tidyverse)
library(ggplot2)
library(MASS)
library(LaplacesDemon)
library(clusterGeneration)


n <- 5
sigmaall<-matrix(rdunif(n^2, 0, 8), ncol=n)
sigmaall[lower.tri(sigmaall)] = t(sigmaall)[lower.tri(sigmaall)]
diag(sigmaall) <- rep(50, n)
sigmaall


library(pracma)
k = 5
sparsity = .2
# generate the symmetric sparsity mask
mask = rand(k)
mask = mask * (mask < sparsity)
mask[lower.tri(mask, diag = TRUE)] = 0
mask = mask + t(mask) + eye(k)
mask[mask > 0] = 1

# generate the symmetric precision matrix
theta = matrix(rnorm(k^2), k)
theta[lower.tri(theta, diag = TRUE)] = 0
theta = theta + t(theta) + eye(k)

# apply the reqired sparsity
theta = theta * mask

# force it to be positive definite
theta = theta - (min(eig(theta))-.1) * eye(k)
theta

Prec2Cov(theta)


n <- 5
prectest<-matrix(runif(n^2, 0, 1), ncol=n)
prectest[lower.tri(prectest)] = t(prectest)[lower.tri(prectest)]
diag(prectest) <- rep(1, n)
prectest

Prec2Cov(prectest)
nolist <- c(4,5,8,10,15)

#FROM THIS THE CODE START!!!

x <- matrix(runif(25,-1,1), nrow = 5)
x[lower.tri(x)] = t(x)[lower.tri(x)]
diag(x) <- runif(5,3,4)
for (i in 1:5) {
  for (j in 1:5) {
    if (i == j){
      next
    }
    if ((i*j) %in% nolist) {
      x[i,j] <- 0
    }
  }
}

x
Prec2Cov(x)


x <- matrix(runif(25,-1,1), nrow = 5)
x[lower.tri(x)] = t(x)[lower.tri(x)]
diag(x) <- runif(5,3,4)
for (i in 1:5) {
  for (j in 1:5) {
    if (i == j){
      next
    }
    if ((i*j) %in% nolist) {
      x[i,j] <- 0
    }
  }
}

x

prec_matrix <- data.frame(x) %>% 
  round(3)
colnames(prec_matrix) <- paste("X",seq(1:ncol(prec_matrix)), sep = "")
prec_matrix <- cbind(Variable = colnames(prec_matrix), prec_matrix)
prec_matrix

write.csv(prec_matrix, "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/csv/prec_matrix.csv",
          col.names = T, row.names = F)

sigmax <- Prec2Cov(x)
sigmax
n <- 5

#Increasing mean
cornorm1 <- list()
for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmax,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0, by = 0.1, length.out = n),
                               Sigma = sigmax, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_data1 <- rbind(group1, group2)
  cornorm_rf1 <- randomForest(y~., data = cornorm_data1, importance = T,
                              localImp = T)
  cornorm1[[i]] <- importance(cornorm_rf1, type = 1, scale = F)
}

#FOR MANY ITERATION 2
cornorm2 <- list()

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmax,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0, by = 0.2, length.out = n),
                               Sigma = sigmax, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_data2 <- rbind(group1, group2)
  cornorm_rf2 <- randomForest(y~., data = cornorm_data2, importance = T,
                              localImp = T)
  cornorm2[[i]] <- importance(cornorm_rf2, type = 1, scale = F)
}

compareplot_imp(cornorm1,cornorm2,"corelatednorm_usingprec1.png")

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmax,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0.4, by = -0.1,length.out = n),
                               Sigma = sigmax, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_data1a <- rbind(group1, group2)
  cornorm_rf1a <- randomForest(y~., data = cornorm_data1a, importance = T,
                               localImp = T)
  cornorm1a[[i]] <- importance(cornorm_rf1a, type = 1, scale = F)
}

#FOR MANY ITERATION 2
cornorm2a <- list()

for (i in 1:10) {
  
  group1 <- data.frame(mvrnorm(n = 1500, mu = rep(0,n), Sigma = sigmax,
                               empirical = F))
  group1$y <- factor(rep(0,1500))
  group2 <- data.frame(mvrnorm(n = 1000,
                               mu = seq(from = 0.8, by = -0.2, length.out = n),
                               Sigma = sigmax, empirical = F))
  group2$y <- factor(rep(1,1000))
  cornorm_data2a <- rbind(group1, group2)
  cornorm_rf2a <- randomForest(y~., data = cornorm_data2a, importance = T,
                               localImp = T)
  cornorm2a[[i]] <- importance(cornorm_rf2a, type = 1, scale = F)
}

compareplot_imp(cornorm1a,cornorm2a,"corelatednorm_usingprec1desc.png")

cor(group2[1:5])
#minimum correlation data works fine.

#Explore this next time??
Posdef <- function (n, ev = runif(n, 0, 10)){
  Z <- matrix(ncol=n, rnorm(n^2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp) 
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}

pdmat <- Posdef(n=5, ev=1:5)
eigen(pdmat)$val
pdmat <- pdmat %*% t(pdmat)
Prec2Cov(pdmat)

genPositiveDefMat("unifcorrmat",dim=4) 
x <- genPositiveDefMat("eigen",dim=5, rangeVar = c(1,1))$Sigma
x <- x %*% t(x)
str(x)
Prec2Cov(x)
matrix(x,ncol = 5)
genPositiveDefMat()