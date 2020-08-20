library(randomForest)
library(MASS)
library(tidyverse)
#Combined variable version 1
withoutnoisecombig1 <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 3, 5000, 1000, 0)
  group11 <- group11[,1:3]
  group21 <- normalgroup3(1000, 3, 5000, 1000, 500, 1)
  group21 <- group21[,1:3]
  
  group12 <- binomgroup1(1500, 3, 1, 0.5, 0)
  group12 <- group12[,1:3]
  group22 <- binomgroup3(1000, 3, 1, 3, 0.5, 1)
  group22 <- group22[,1:3]
  
  group13 <- categoricalgroup2(1500, 3, 1, 2, 1, 0)
  group23 <- categoricalgroup3(1000, 3, 1, 2, 1, 1)
  
  group1 <- cbind(group11, group12, group13)
  group2 <- cbind(group21, group22, group23)
  
  combined_bigdata1 <- rbind(group1, group2)
  
  rf_withoutnoisecomb1 <- randomForest(y~., data = combined_bigdata1,
                                       importance = T, localImp = T)
  withoutnoisecombig1[[i]] <- importance(rf_withoutnoisecomb1, type = 1, scale = F)
}


#Combined variable version 2
withoutnoisecombig2 <- list()

for (i in 1:10) {
  group11 <- normalgroup1(1500, 3, 5000, 1000, 0)
  group11 <- group11[,1:3]
  group21 <- normalgroup3(1000, 3, 5000, 1000, 1000, 1)
  group21 <- group21[,1:3]
  
  group12 <- binomgroup1(1500, 3, 1, 0.5, 0)
  group12 <- group12[,1:3]
  group22 <- binomgroup3(1000, 3, 1, 5, 0.5, 1)
  group22 <- group22[,1:3]
  
  group13 <- categoricalgroup2(1500, 3, 1, 2, 2, 0)
  group23 <- categoricalgroup3(1000, 3, 1, 2, 2, 1)
  
  group1 <- cbind(group11, group12, group13)
  group2 <- cbind(group21, group22, group23)
  
  combined_bigdata2 <- rbind(group1, group2)
  
  rf_withoutnoisecombbig2 <- randomForest(y~., data = combined_bigdata2,
                                       importance = T, localImp = T)
  
  withoutnoisecombig2[[i]] <- importance(rf_withoutnoisecombbig2, type = 1, scale = F)
}


compareplot_imp(withoutnoisecombig1, withoutnoisecombig2, "withoutnoisecomb31.png")
