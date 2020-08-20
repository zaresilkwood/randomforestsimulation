library(randomForest)
library(tidyverse)
library(MASS)


#continuous and discrete
#1. different mean and different probability
withoutnoisenormbinom1 <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.1, 1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.7, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                                       importance = T, localImp = T)
  withoutnoisenormbinom1[[i]] <- importance(current_rf, type = 1, scale = F)
}

withoutnoisenormbinom2 <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.2, 1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.8, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                                       importance = T, localImp = T)
  withoutnoisenormbinom2[[i]] <- importance(current_rf, type = 1, scale = F)
}

compareplot_imp(withoutnoisenormbinom1, withoutnoisenormbinom2,
                "withoutnoisenormbinom1.png")

##############################################################################
#2. different mean and different trial

withoutnoisenormbinom1 <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.1, 1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup3(1000, 4, 1, 4, 0.5, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                             importance = T, localImp = T)
  withoutnoisenormbinom1[[i]] <- importance(current_rf, type = 1, scale = F)
}

withoutnoisenormbinom2 <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.2, 1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup3(1000, 4, 1, 7, 0.5, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                             importance = T, localImp = T)
  withoutnoisenormbinom2[[i]] <- importance(current_rf, type = 1, scale = F)
}

compareplot_imp(withoutnoisenormbinom1, withoutnoisenormbinom2,
                "withoutnoisenormbinom2.png")

##############################################################################
#3. different sd and different probability

withoutnoisenormbinom1 <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.7, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                             importance = T, localImp = T)
  withoutnoisenormbinom1[[i]] <- importance(current_rf, type = 1, scale = F)
}

withoutnoisenormbinom2 <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.2, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.8, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                             importance = T, localImp = T)
  withoutnoisenormbinom2[[i]] <- importance(current_rf, type = 1, scale = F)
}

compareplot_imp(withoutnoisenormbinom1, withoutnoisenormbinom2,
                "withoutnoisenormbinom3.png")

##############################################################################
#4. different sd and different trial

withoutnoisenormbinom1a <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.1, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup3(1000, 4, 1, 4, 0.5, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                                       importance = T, localImp = T)
  withoutnoisenormbinom1a[[i]] <- importance(current_rf, type = 1, scale = F)
}

withoutnoisenormbinom2a <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.2, 1)
  group21 <- group21[,1:4]
  
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group22 <- binomgroup3(1000, 4, 1, 7, 0.5, 1)
  
  group1 <- cbind(group11, group12)
  group2 <- cbind(group21, group22)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                                       importance = T, localImp = T)
  
  withoutnoisenormbinom2a[[i]] <- importance(current_rf, type = 1, scale = F)
}


compareplot_imp(withoutnoisenormbinom1a, withoutnoisenormbinom2a,
                "withoutnoisenormbinom4.png")

##############################################################################

#1 Continuous and Categorical

withoutnoisenormcat1 <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.1, 1, 1)
  group21 <- group21[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 1, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 1, 1)
  
  group1 <- cbind(group11, group13)
  group2 <- cbind(group21, group23)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                                       importance = T, localImp = T)
  withoutnoisenormcat1[[i]] <- importance(current_rf, type = 1, scale = F)
}

withoutnoisenormcat2 <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup2(1000, 4, 0, 0.2, 1, 1)
  group21 <- group21[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 2, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 2, 1)
  
  group1 <- cbind(group11, group13)
  group2 <- cbind(group21, group23)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                             importance = T, localImp = T)
  withoutnoisenormcat2[[i]] <- importance(current_rf, type = 1, scale = F)
}


compareplot_imp(withoutnoisenormcat1, withoutnoisenormcat2,
                "withoutnoisenormcat1.png")

#######################################################################

#2. version 2 continuous and categorical, changing the SD

withoutnoisenormcat1a <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.1, 1)
  group21 <- group21[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 1, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 1, 1)
  
  group1 <- cbind(group11, group13)
  group2 <- cbind(group21, group23)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                             importance = T, localImp = T)
  withoutnoisenormcat1a[[i]] <- importance(current_rf, type = 1, scale = F)
}

withoutnoisenormcat2a <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 4, 0, 1, 0)
  group11 <- group11[,1:4]
  group21 <- normalgroup3(1000, 4, 0, 1, 0.2, 1)
  group21 <- group21[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 2, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 2, 1)
  
  group1 <- cbind(group11, group13)
  group2 <- cbind(group21, group23)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                             importance = T, localImp = T)
  withoutnoisenormcat2a[[i]] <- importance(current_rf, type = 1, scale = F)
}


compareplot_imp(withoutnoisenormcat1a, withoutnoisenormcat2a,
                "withoutnoisenormcat2.png")

##############################################################################

#1. Binomial and Categorical 

withoutnoisebincat1 <- list()

for (i in 1:10) {
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group12 <- group12[,1:4]
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.7, 1)
  group22 <- group22[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 1, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 1, 1)
  
  group1 <- cbind(group12, group13)
  group2 <- cbind(group22, group23)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                                       importance = T, localImp = T)
  withoutnoisebincat1[[i]] <- importance(current_rf, type = 1, scale = F)
}

withoutnoisebincat2 <- list()

for (i in 1:10) {
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group12 <- group12[,1:4]
  group22 <- binomgroup2(1000, 4, 1, 0.5, 0.8, 1)
  group22 <- group22[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 2, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 2, 1)
  
  group1 <- cbind(group12, group13)
  group2 <- cbind(group22, group23)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                                       importance = T, localImp = T)
  
  withoutnoisebincat2[[i]] <- importance(current_rf, type = 1, scale = F)
}

compareplot_imp(withoutnoisebincat1, withoutnoisebincat2,
                "withoutnoisebincat1.png")

##############################################################################
#2. version 2 Binomial and Categorical, changing the number of trial

withoutnoisebincat1a <- list()

for (i in 1:10) {
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group12 <- group12[,1:4]
  group22 <- binomgroup3(1000, 4, 1, 4, 0.5, 1)
  group22 <- group22[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 1, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 1, 1)
  
  group1 <- cbind(group12, group13)
  group2 <- cbind(group22, group23)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                             importance = T, localImp = T)
  withoutnoisebincat1a[[i]] <- importance(current_rf, type = 1, scale = F)
}

withoutnoisebincat2a <- list()

for (i in 1:10) {
  group12 <- binomgroup1(1500, 4, 1, 0.5, 0)
  group12 <- group12[,1:4]
  group22 <- binomgroup3(1000, 4, 1, 7, 0.5, 1)
  group22 <- group22[,1:4]
  
  group13 <- categoricalgroup2(1500, 4, 1, 2, 2, 0)
  group23 <- categoricalgroup3(1000, 4, 1, 2, 2, 1)
  
  group1 <- cbind(group12, group13)
  group2 <- cbind(group22, group23)
  
  current_data <- rbind(group1, group2)
  
  current_rf <- randomForest(y~., data = current_data,
                             importance = T, localImp = T)
  
  withoutnoisebincat2a[[i]] <- importance(current_rf, type = 1, scale = F)
}

compareplot_imp(withoutnoisebincat1a, withoutnoisebincat2a,
                "withoutnoisebincat2.png")