#Continuous Variable, all same mean and SD
library(randomForest)
library(ggplot2)
library(tidyverse)

#Code to check importance
nullsamplenorm_list1a <- list()
for (i in 1:10) {
  nullsamplenorm_1 <- normalgroup1(1500, 5, 0, 1, 0)
  nullsamplenorm_2 <- normalgroup1(1000, 5, 0, 1, 1)
  nullsamplenorm1a <- rbind(nullsamplenorm_1, nullsamplenorm_2)
  rf_nullsamplenorm1a <- randomForest(y~., data = nullsamplenorm1a, importance = T,
                                    localImp = T)
  nullsamplenorm_list1a[[i]] <- importance(rf_nullsamplenorm1a, type = 1, scale = F)
}


nullsamplenorm_list1b <- list()
for (i in 1:10) {
  nullsamplenorm_1 <- normalgroup1(1500, 5, 1, 1, 0)
  nullsamplenorm_2 <- normalgroup1(1000, 5, 1, 1, 1)
  nullsamplenorm1b <- rbind(nullsamplenorm_1, nullsamplenorm_2)
  rf_nullsamplenorm1b <- randomForest(y~., data = nullsamplenorm1b, importance = T,
                                    localImp = T)
  nullsamplenorm_list1b[[i]] <- importance(rf_nullsamplenorm1b, type = 1, scale = F)
}

compareplot_imp(nullsamplenorm_list1a, nullsamplenorm_list1b, "nullsamplenorm1.png")

#Using normalgroup2
nullsamplenorm2a_list <- list()
for (i in 1:10) {
  nullsamplenorm_1 <- normalgroup2(1500, 5, 0, 0.1, 1, 0)
  nullsamplenorm_2 <- normalgroup2(1000, 5, 0, 0.1, 1, 1)
  nullsamplenorm2a <- rbind(nullsamplenorm_1, nullsamplenorm_2)
  rf_nullsamplenorm2a <- randomForest(y~., data = nullsamplenorm2a, 
                                     importance = T, localImp = T)
  nullsamplenorm2a_list[[i]] <- importance(rf_nullsamplenorm2a, type = 1, scale = F)
}

nullsamplenorm2b_list <- list()
for (i in 1:10) {
  nullsamplenorm_1 <- normalgroup2(1500, 5, 0, 0.2, 1, 0)
  nullsamplenorm_2 <- normalgroup2(1000, 5, 0, 0.2, 1, 1)
  nullsamplenorm2 <- rbind(nullsamplenorm_1, nullsamplenorm_2)
  rf_nullsamplenorm2 <- randomForest(y~., data = nullsamplenorm2, 
                                     importance = T, localImp = T)
  nullsamplenorm2b_list[[i]] <- importance(rf_nullsamplenorm2, type = 1, scale = F)
}

compareplot_imp(nullsamplenorm2a_list, nullsamplenorm2b_list, "nullsamplenorm2.png")



#Using normalgroup3
nullsamplenorm3a_list <- list()
for (i in 1:10) {
  nullsamplenorm_1 <- normalgroup3(1500, 5, 0, 1, 0.1, 0)
  nullsamplenorm_2 <- normalgroup3(1000, 5, 0, 1, 0.1, 1)
  nullsamplenorm3a <- rbind(nullsamplenorm_1, nullsamplenorm_2)
  rf_nullsamplenorm3a <- randomForest(y~., data = nullsamplenorm3a, 
                                      importance = T, localImp = T)
  nullsamplenorm3a_list[[i]] <- importance(rf_nullsamplenorm3a, type = 1, scale = F)
}

nullsamplenorm3b_list <- list()
for (i in 1:10) {
  nullsamplenorm_1 <- normalgroup3(1500, 5, 0, 1, 0.2, 0)
  nullsamplenorm_2 <- normalgroup3(1000, 5, 0, 1, 0.2, 1)
  nullsamplenorm3b <- rbind(nullsamplenorm_1, nullsamplenorm_2)
  rf_nullsamplenorm3b <- randomForest(y~., data = nullsamplenorm3b, 
                                     importance = T, localImp = T)
  nullsamplenorm3b_list[[i]] <- importance(rf_nullsamplenorm3b, type = 1, scale = F)
}

compareplot_imp(nullsamplenorm3a_list, nullsamplenorm3b_list, "nullsamplenorm3.png")


#Using Discrete first group
nullsamplebinom1a <- list()
for (i in 1:10) {
  group1 <- binomgroup1(1500, 5, 1, 0.5, 0)
  group2 <- binomgroup1(1000, 5, 1, 0.5, 1)
  nullsamplebinomdata1a <- rbind(group1, group2)
  rf_nullsamplebinomdata1a <- randomForest(y~., data = nullsamplebinomdata1a,
                                         importance = T, localImp = T)
  nullsamplebinom1a[[i]] <- importance(rf_nullsamplebinomdata1a, type = 1, scale = F)
}

nullsamplebinom1b <- list()
for (i in 1:10) {
  group1 <- binomgroup1(1500, 5, 1, 0.7, 0)
  group2 <- binomgroup1(1000, 5, 1, 0.7, 1)
  nullsamplebinomdata1b <- rbind(group1, group2)
  rf_nullsamplebinomdata1b <- randomForest(y~., data = nullsamplebinomdata1b,
                                          importance = T, localImp = T)
  nullsamplebinom1b[[i]] <- importance(rf_nullsamplebinomdata1b, type = 1, scale = F)
}

compareplot_imp(nullsamplebinom1a, nullsamplebinom1b, "nullsamplebinom1.png")
  
  
#Using Discrete Second group
nullsamplebinom2a <- list()
for (i in 1:10) {
  group1 <- binomgroup2(1500, 5, 1, 0.5, 0.7, 0)
  group2 <- binomgroup2(1000, 5, 1, 0.5, 0.7, 1)
  nullsamplebinomdata2a <- rbind(group1, group2)
  rf_nullsamplebinomdata2a <- randomForest(y~., data = nullsamplebinomdata2a,
                                         importance = T, localImp = T)
  nullsamplebinom2a[[i]] <- importance(rf_nullsamplebinomdata2a, type = 1, scale = F)
}

nullsamplebinom2b <- list()
for (i in 1:10) {
  group1 <- binomgroup2(1500, 5, 1, 0.5, 0.8, 0)
  group2 <- binomgroup2(1000, 5, 1, 0.5, 0.8, 1)
  nullsamplebinomdata2b <- rbind(group1, group2)
  rf_nullsamplebinomdata2b <- randomForest(y~., data = nullsamplebinomdata2b,
                                          importance = T, localImp = T)
  nullsamplebinom2b[[i]] <- importance(rf_nullsamplebinomdata2b, type = 1, scale = F)
}

compareplot_imp(nullsamplebinom2a, nullsamplebinom2b, "nullsamplebinom2.png")

#Using Discrete Third group
nullsamplebinom3a <- list()
for (i in 1:10) {
  group1 <- binomgroup3(1500, 5, 1, 5, 0.5, 0)
  group2 <- binomgroup3(1000, 5, 1, 5, 0.5, 1)
  nullsamplebinomdata3a <- rbind(group1, group2)
  rf_nullsamplebinomdata3a <- randomForest(y~., data = nullsamplebinomdata3a,
                                           importance = T, localImp = T)
  nullsamplebinom3a[[i]] <- importance(rf_nullsamplebinomdata3a, type = 1, scale = F)
}

nullsamplebinom3b <- list()
for (i in 1:10) {
  group1 <- binomgroup3(1500, 5, 1, 9, 0.5, 0)
  group2 <- binomgroup3(1000, 5, 1, 9, 0.5, 1)
  nullsamplebinomdata3b <- rbind(group1, group2)
  rf_nullsamplebinomdata3b <- randomForest(y~., data = nullsamplebinomdata3b,
                                           importance = T, localImp = T)
  nullsamplebinom3b[[i]] <- importance(rf_nullsamplebinomdata3b, type = 1, scale = F)
}

compareplot_imp(nullsamplebinom3a, nullsamplebinom3b, "nullsamplebinom3.png")

#Using Categorical First Group
nullsamplecat1a <- list()
for (i in 1:10) {
  group1 <- categoricalgroup1(1500, 5, 1, 2, 0)
  group2 <- categoricalgroup1(1000, 5, 1, 2, 1)
  nullsamplecatdata1a <- rbind(group1, group2)
  rf_nullsamplecat1a <- randomForest(y~., data = nullsamplecatdata1a,
                                         importance = T, localImp = T)
  nullsamplecat1a[[i]] <- importance(rf_nullsamplecat1a, type = 1, scale = F)
}


nullsamplecat1b <- list()
for (i in 1:10) {
  group1 <- categoricalgroup1(1500, 5, 1, 4, 0)
  group2 <- categoricalgroup1(1000, 5, 1, 4, 1)
  nullsamplecatdata1b <- rbind(group1, group2)
  rf_nullsamplecat1b <- randomForest(y~., data = nullsamplecatdata1b,
                                    importance = T, localImp = T)
  nullsamplecat1b[[i]] <- importance(rf_nullsamplecat1b, type = 1, scale = F)
}

compareplot_imp(nullsamplecat1a, nullsamplecat1b, "nullsamplecat1.png")

#Using Categorical Second Group
nullsamplecat2a <- list()
for (i in 1:10) {
  group1 <- categoricalgroup2(1500, 5, 1, 2, 1, 0)
  group2 <- categoricalgroup2(1000, 5, 1, 2, 1, 1)
  nullsamplecatdata2a <- rbind(group1, group2)
  rf_nullsamplecat2a <- randomForest(y~., data = nullsamplecatdata2a,
                                    importance = T, localImp = T)
  nullsamplecat2a[[i]] <- importance(rf_nullsamplecat2a, type = 1, scale = F)
}

nullsamplecat2b <- list()
for (i in 1:10) {
  group1 <- categoricalgroup2(1500, 5, 1, 2, 2, 0)
  group2 <- categoricalgroup2(1000, 5, 1, 2, 2, 1)
  nullsamplecatdata2b <- rbind(group1, group2)
  rf_nullsamplecat2b <- randomForest(y~., data = nullsamplecatdata2b,
                                    importance = T, localImp = T)
  nullsamplecat2b[[i]] <- importance(rf_nullsamplecat2b, type = 1, scale = F)
}

compareplot_imp(nullsamplecat2a, nullsamplecat2b, "nullsamplecat2.png")

#Combined Variable First Group
nullsamplecomba <- list()
for (i in 1:10) {
  group11 <- nullsamplenorm_1 <- normalgroup1(1500, 3, 0, 1, 0)
  group11 <- group11[,1:3]
  group12 <- binomgroup1(1500, 3, 1, 0.5, 0)
  group12 <- group12[,1:3]
  group13 <- categoricalgroup1(1500, 3, 1, 2, 0)
  group1 <- data.frame(group11, group12, group13)
  
  group21 <- nullsamplenorm_1 <- normalgroup1(1000, 3, 0, 1, 1)
  group21 <- group21[,1:3]
  group22 <- binomgroup1(1000, 3, 1, 0.5, 1)
  group22 <- group22[,1:3]
  group23 <-categoricalgroup1(1000, 3, 1, 2, 1)
  group2 <- data.frame(group21, group22, group23)
  
  nullsamplecombdataa <- rbind(group1, group2)
  rf_nullsamplecomba <- randomForest(y~., data = nullsamplecombdataa,
                                    importance = T, localImp = T)
  nullsamplecomba[[i]] <- importance(rf_nullsamplecomba, type = 1, scale = F)
}

nullsamplecombb <- list()
for (i in 1:10) {
  group11 <- nullsamplenorm_1 <- normalgroup1(1500, 3, 1, 1, 0)
  group11 <- group11[,1:3]
  group12 <- binomgroup1(1500, 3, 1, 0.7, 0)
  group12 <- group12[,1:3]
  group13 <- categoricalgroup1(1500, 3, 1, 4, 0)
  group1 <- data.frame(group11, group12, group13)
  
  group21 <- nullsamplenorm_1 <- normalgroup1(1000, 3, 1, 1, 1)
  group21 <- group21[,1:3]
  group22 <- binomgroup1(1000, 3, 1, 0.7, 1)
  group22 <- group22[,1:3]
  group23 <-categoricalgroup1(1000, 3, 1, 4, 1)
  group2 <- data.frame(group21, group22, group23)
  
  nullsamplecombdatab <- rbind(group1, group2)
  rf_nullsamplecombb <- randomForest(y~., data = nullsamplecombdatab,
                                    importance = T, localImp = T)
  nullsamplecombb[[i]] <- importance(rf_nullsamplecombb, type = 1, scale = F)
}

compareplot_imp(nullsamplecomba, nullsamplecombb, "nullsamplecomb.png")
