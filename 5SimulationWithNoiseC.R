###################################
### RANDOM CATEGORICAL VARIABLE ###
###################################

library(randomForest)
library(tidyverse)
library(ggplot2)

#with mean factor 0.1
withnoisenorm1 <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 0, 1, 0)
  group2 <- normalgroup2(1000, 5, 0, 0.1, 1, 1)
  withnoisenorm_data1 <- rbind(group1, group2)
  withnoisenorm_data1$random <- factor(sample(LETTERS[1:4], 2500, replace = TRUE))
  rf_withnoisenorm1 <- randomForest(y~., data = withnoisenorm_data1, importance = T,
                                    localImp = T)
  withnoisenorm1[[i]] <- importance(rf_withnoisenorm1, type = 1, scale = F)
}

#increasing mean factor
withnoisenorm2 <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 0, 1, 0)
  group2 <- normalgroup2(1000, 5, 0, 0.2, 1, 1)
  withnoisenorm_data2 <- rbind(group1, group2)
  withnoisenorm_data2$random <- factor(sample(LETTERS[1:4], 2500, replace = TRUE))
  rf_withnoisenorm2 <- randomForest(y~., data = withnoisenorm_data2, importance = T,
                                    localImp = T)
  withnoisenorm2[[i]] <- importance(rf_withnoisenorm2, type = 1, scale = F)
}

compareplot_imp(withnoisenorm1, withnoisenorm2, "Cwithnoisenorm.png")

#discrete variable
withnoisebinom1 <- list()
for (i in 1:10) {
  group1 <- binomgroup1(1500,5,1,0.5,0)
  group2 <- binomgroup2(1000,5,1,0.5,0.7,1)
  withnoisebinom_data1 <- rbind(group1, group2)
  withnoisebinom_data1$random <- factor(sample(LETTERS[1:4], 2500, replace = TRUE))
  rf_withnoisebinom1 <- randomForest(y~., data = withnoisebinom_data1,
                                     importance = T, localImp = T)
  withnoisebinom1[[i]] <- importance(rf_withnoisebinom1, type = 1, scale = F)
}

withnoisebinom2 <- list()
for (i in 1:10) {
  group1 <- binomgroup1(1500,5,1,0.5,0)
  group2 <- binomgroup2(1000,5,1,0.5,0.8,1)
  withnoisebinom_data2 <- rbind(group1, group2)
  withnoisebinom_data2$random <- factor(sample(LETTERS[1:4], 2500, replace = TRUE))
  rf_withnoisebinom2 <- randomForest(y~., data = withnoisebinom_data2,
                                     importance = T, localImp = T)
  withnoisebinom2[[i]] <- importance(rf_withnoisebinom2, type = 1, scale = F)
}

compareplot_imp(withnoisebinom1, withnoisebinom2, "Cwithnoisebinom.png")

#categorical variable
withnoisecat1 <- list()
for (i in 1:10) {
  group1 <- categoricalgroup2(1500, 5, 1, 2, 1, 0)
  group2 <- categoricalgroup3(1000, 5, 1, 2, 1, 1)
  withnoisecat_data1 <- rbind(group1, group2)
  withnoisecat_data1$random <- factor(sample(LETTERS[1:3], 2500, replace = TRUE))
  rf_withnoisecat1 <- randomForest(y~., data = withnoisecat_data1,
                                   importance = T, localImp = T)
  withnoisecat1[[i]] <- importance(rf_withnoisecat1, type = 1, scale = F)
}


#categorical variable with incremental 2
withnoisecat2 <- list()
for (i in 1:10) {
  group1 <- categoricalgroup2(1500, 5, 1, 2, 2, 0)
  group2 <- categoricalgroup3(1000, 5, 1, 2, 2, 1)
  withnoisecat_data2 <- rbind(group1, group2)
  withnoisecat_data2$random <- factor(sample(LETTERS[1:5], 2500, replace = TRUE))
  rf_withnoisecat2 <- randomForest(y~., data = withnoisecat_data2,
                                   importance = T, localImp = T)
  withnoisecat2[[i]] <- importance(rf_withnoisecat2, type = 1, scale = F)
}

compareplot_imp(withnoisecat1, withnoisecat2, "Cwithnoisecat.png")

#Combined variable version 1
withnoisecomb1 <- list()

for (i in 1:10) {
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
  
  withnoisecomb_data1 <- rbind(group1, group2)
  withnoisecomb_data1$random <- factor(sample(LETTERS[1:3], 2500, replace = TRUE))
  
  rf_withnoisecomb1 <- randomForest(y~., data = withnoisecomb_data1,
                                    importance = T, localImp = T)
  withnoisecomb1[[i]] <- importance(rf_withnoisecomb1, type = 1, scale = F)
}

#Combined variable version 2
withnoisecomb2 <- list()

for (i in 1:10) {
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
  
  withnoisecomb_data2 <- rbind(group1, group2)
  withnoisecomb_data2$random <- factor(sample(LETTERS[1:5], 2500, replace = TRUE))
  
  rf_withnoisecomb2 <- randomForest(y~., data = withnoisecomb_data2,
                                    importance = T, localImp = T)
  
  withnoisecomb2[[i]] <- importance(rf_withnoisecomb2, type = 1, scale = F)
}

compareplot_imp(withnoisecomb1, withnoisecomb2, "Cwithnoisecomb.png")
