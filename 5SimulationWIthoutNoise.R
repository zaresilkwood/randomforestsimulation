library(randomForest)
library(tidyverse)
library(MASS)

#with mean factor 0.1
withoutnoisenorm1 <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 0, 1, 0)
  group2 <- normalgroup2(1000, 5, 0, 0.1, 1, 1)
  withoutnoisenorm_data <- rbind(group1, group2)
  rf_withoutnoisenorm1 <- randomForest(y~., data = withoutnoisenorm_data, importance = T,
                                    localImp = T)
  withoutnoisenorm1[[i]] <- importance(rf_withoutnoisenorm1, type = 1, scale = F)
}

#increasing mean factor to 0.2
withoutnoisenorm2 <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 0, 1, 0)
  group2 <- normalgroup2(1000, 5, 0, 0.2, 1, 1)
  withoutnoisenorm_data2 <- rbind(group1, group2)
  rf_withoutnoisenorm2 <- randomForest(y~., data = withoutnoisenorm_data2, importance = T,
                                    localImp = T)
  withoutnoisenorm2[[i]] <- importance(rf_withoutnoisenorm2, type = 1, scale = F)
}

compareplot_imp(withoutnoisenorm1, withoutnoisenorm2, "withoutnoisenorm.png")

#Using different in SD

#with sd factor 0.1
withoutnoisenorm1b <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 0, 1, 0)
  group2 <- normalgroup3(1000, 5, 0, 1, 0.1, 1)
  withoutnoisenorm_datab <- rbind(group1, group2)
  rf_withoutnoisenorm1b <- randomForest(y~., data = withoutnoisenorm_datab, importance = T,
                                       localImp = T)
  withoutnoisenorm1b[[i]] <- importance(rf_withoutnoisenorm1b, type = 1, scale = F)
}

#increasing sd factor to 0.2
withoutnoisenorm2b <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 0, 1, 0)
  group2 <- normalgroup3(1000, 5, 0, 1, 0.2, 1)
  withoutnoisenorm_data2b <- rbind(group1, group2)
  rf_withoutnoisenorm2b <- randomForest(y~., data = withoutnoisenorm_data2b, importance = T,
                                       localImp = T)
  withoutnoisenorm2b[[i]] <- importance(rf_withoutnoisenorm2b, type = 1, scale = F)
}

compareplot_imp(withoutnoisenorm1b, withoutnoisenorm2b, "withoutnoisenorm2.png")

#Comparing change in mean and change in sd

#with mean factor 0.1
n <- 9
withoutnoisenorm1 <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, n, 0, 1, 0)
  group2 <- normalgroup2(1000, n, 0, 0.2, 1, 1)
  withoutnoisenorm_data <- rbind(group1, group2)
  rf_withoutnoisenorm1 <- randomForest(y~., data = withoutnoisenorm_data, importance = T,
                                       localImp = T)
  withoutnoisenorm1[[i]] <- importance(rf_withoutnoisenorm1, type = 1, scale = F)
}

#with sd factor 0.1
withoutnoisenorm1b <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, n, 0, 1, 0)
  group2 <- normalgroup3(1000, n, 0, 1, 0.2, 1)
  withoutnoisenorm_datab <- rbind(group1, group2)
  rf_withoutnoisenorm1b <- randomForest(y~., data = withoutnoisenorm_datab, importance = T,
                                        localImp = T)
  withoutnoisenorm1b[[i]] <- importance(rf_withoutnoisenorm1b, type = 1, scale = F)
}

compareplot_imp(withoutnoisenorm1, withoutnoisenorm1b,
                "withoutnoisenorm_meansd_9variable_factor2.png")


#Comparing change in mean and change in sd, bigger change in mean and sd

#with mean factor 0.1
withoutnoisenorm1 <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 0, 1, 0)
  group2 <- normalgroup2(1000, 5, 0, 0.2, 1, 1)
  withoutnoisenorm_data <- rbind(group1, group2)
  rf_withoutnoisenorm1 <- randomForest(y~., data = withoutnoisenorm_data, importance = T,
                                       localImp = T)
  withoutnoisenorm1[[i]] <- importance(rf_withoutnoisenorm1, type = 1, scale = F)
}

#with sd factor 0.1
withoutnoisenorm1b <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 0, 1, 0)
  group2 <- normalgroup3(1000, 5, 0, 1, 0.2, 1)
  withoutnoisenorm_datab <- rbind(group1, group2)
  rf_withoutnoisenorm1b <- randomForest(y~., data = withoutnoisenorm_datab, importance = T,
                                        localImp = T)
  withoutnoisenorm1b[[i]] <- importance(rf_withoutnoisenorm1b, type = 1, scale = F)
}

compareplot_imp(withoutnoisenorm1, withoutnoisenorm1b, "withoutnoisenorm_meansd.png")


#discrete variable
withoutnoisebinom1 <- list()
for (i in 1:10) {
  group1 <- binomgroup1(1500,5,1,0.5,0)
  group2 <- binomgroup2(1000,5,1,0.5,0.7,1)
  withoutnoisebinom_data1 <- rbind(group1, group2)
  rf_withoutnoisebinom1 <- randomForest(y~., data = withoutnoisebinom_data1,
                                        importance = T, localImp = T)
  withoutnoisebinom1[[i]] <- importance(rf_withoutnoisebinom1, type = 1, scale = F)
}

withoutnoisebinom2 <- list()
for (i in 1:10) {
  group1 <- binomgroup1(1500,5,1,0.5,0)
  group2 <- binomgroup2(1000,5,1,0.5,0.8,1)
  withoutnoisebinom_data2 <- rbind(group1, group2)
  rf_withoutnoisebinom2 <- randomForest(y~., data = withoutnoisebinom_data2,
                                        importance = T, localImp = T)
  withoutnoisebinom2[[i]] <- importance(rf_withoutnoisebinom2, type = 1, scale = F)
}

compareplot_imp(withoutnoisebinom1, withoutnoisebinom2, "withoutnoisebinom.png")

#discrete variable
withoutnoisebinom1b <- list()
for (i in 1:10) {
  group1 <- binomgroup1(1500,5,1,0.5,0)
  group2 <- binomgroup3(1000,5,1,5,0.5,1)
  withoutnoisebinom_data1b <- rbind(group1, group2)
  rf_withoutnoisebinom1b <- randomForest(y~., data = withoutnoisebinom_data1b,
                                        importance = T, localImp = T)
  withoutnoisebinom1b[[i]] <- importance(rf_withoutnoisebinom1b, type = 1, scale = F)
}

withoutnoisebinom2b <- list()
for (i in 1:10) {
  group1 <- binomgroup1(1500,5,1,0.5,0)
  group2 <- binomgroup3(1000,5,1,9,0.5,1)
  withoutnoisebinom_data2b <- rbind(group1, group2)
  rf_withoutnoisebinom2b <- randomForest(y~., data = withoutnoisebinom_data2b,
                                        importance = T, localImp = T)
  withoutnoisebinom2b[[i]] <- importance(rf_withoutnoisebinom2b, type = 1, scale = F)
}

compareplot_imp(withoutnoisebinom1b, withoutnoisebinom2b, "withoutnoisebinom2.png")

#ANOTHER TRY FOR DISCRETE
#discrete variable, change the constant to 5 and 10
withoutnoisebinom1b <- list()
for (i in 1:10) {
  group1 <- binomgroup1(1500,5,1,0.5,0)
  group2 <- binomgroup3(1000,5,1,5,0.5,1)
  withoutnoisebinom_data1b <- rbind(group1, group2)
  rf_withoutnoisebinom1b <- randomForest(y~., data = withoutnoisebinom_data1b,
                                         importance = T, localImp = T)
  withoutnoisebinom1b[[i]] <- importance(rf_withoutnoisebinom1b, type = 1, scale = F)
}

withoutnoisebinom2b <- list()
for (i in 1:10) {
  group1 <- binomgroup1(1500,5,3,0.5,0)
  group2 <- binomgroup3(1000,5,1,5,0.5,1)
  withoutnoisebinom_data2b <- rbind(group1, group2)
  rf_withoutnoisebinom2b <- randomForest(y~., data = withoutnoisebinom_data2b,
                                         importance = T, localImp = T)
  withoutnoisebinom2b[[i]] <- importance(rf_withoutnoisebinom2b, type = 1, scale = F)
}

compareplot_imp(withoutnoisebinom1b, withoutnoisebinom2b, "withoutnoisebinom3.png")
#THE gist from this is, when the nunmber of trial from variable that has Y = 1 is 
#is equal to number of trial variable that has Y = 0, it will become less important
#of course, because it will be less important



#categorical variable
n <- 5
withoutnoisecat1 <- list()
for (i in 1:10) {
  group1 <- categoricalgroup2(1500, n, 1, 2, 1, 0)
  group2 <- categoricalgroup3(1000, n, 1, 2, 1, 1)
  withoutnoisecat_data1 <- rbind(group1, group2)
  rf_withoutnoisecat1 <- randomForest(y~., data = withoutnoisecat_data1,
                                        importance = T, localImp = T)
  withoutnoisecat1[[i]] <- importance(rf_withoutnoisecat1, type = 1, scale = F)
}

#categorical variable with incremental 2
withoutnoisecat2 <- list()
for (i in 1:10) {
  group1 <- categoricalgroup2(1500, n, 1, 2, 2, 0)
  group2 <- categoricalgroup3(1000, n, 1, 2, 2, 1)
  withoutnoisecat_data2 <- rbind(group1, group2)
  rf_withoutnoisecat2 <- randomForest(y~., data = withoutnoisecat_data2,
                                      importance = T, localImp = T)
  withoutnoisecat2[[i]] <- importance(rf_withoutnoisecat2, type = 1, scale = F)
}

compareplot_imp(withoutnoisecat1, withoutnoisecat2, "withoutnoisecat5variable1.png")



#ANOTHER METHOD for CATEGORICAL DATA
n <- 5
withoutnoisecat1a <- list()
for (i in 1:10) {
  group1 <- categoricalgroup1(1500, n, 1, 2, 0)
  group2 <- categoricalgroup2(1000, n, 1, 2, 1, 1)
  withoutnoisecat_data1 <- rbind(group1, group2)
  rf_withoutnoisecat1 <- randomForest(y~., data = withoutnoisecat_data1,
                                      importance = T, localImp = T)
  withoutnoisecat1a[[i]] <- importance(rf_withoutnoisecat1, type = 1, scale = F)
}

#categorical variable with incremental 2
withoutnoisecat2a <- list()
for (i in 1:10) {
  group1 <- categoricalgroup1(1500, n, 1, 2, 0)
  group2 <- categoricalgroup2(1000, n, 1, 2, 2, 1)
  withoutnoisecat_data2 <- rbind(group1, group2)
  rf_withoutnoisecat2 <- randomForest(y~., data = withoutnoisecat_data2,
                                      importance = T, localImp = T)
  withoutnoisecat2a[[i]] <- importance(rf_withoutnoisecat2, type = 1, scale = F)
}

compareplot_imp(withoutnoisecat1a, withoutnoisecat2a, "withoutnoisecat5variable2.png")



#Combined variable version 1
withoutnoisecomb1 <- list()

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
  
  withoutnoisecomb_data1 <- rbind(group1, group2)
  
  rf_withoutnoisecomb1 <- randomForest(y~., data = withoutnoisecomb_data1,
                                       importance = T, localImp = T)
  withoutnoisecomb1[[i]] <- importance(rf_withoutnoisecomb1, type = 1, scale = F)
}


#Combined variable version 2
withoutnoisecomb2 <- list()

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
  
  withoutnoisecomb_data2 <- rbind(group1, group2)
  
  rf_withoutnoisecomb2 <- randomForest(y~., data = withoutnoisecomb_data2,
                                       importance = T, localImp = T)
  
  withoutnoisecomb2[[i]] <- importance(rf_withoutnoisecomb2, type = 1, scale = F)
}


compareplot_imp(withoutnoisecomb1, withoutnoisecomb2, "withoutnoisecomb.png")



#CHANGING SD AND CHANGING NUMBER OF TRIAL

#Combined variable version 1
withoutnoisecomb1a <- list()

for (i in 1:10) {
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
  
  withoutnoisecomb_data1 <- rbind(group1, group2)
  
  rf_withoutnoisecomb1 <- randomForest(y~., data = withoutnoisecomb_data1,
                                       importance = T, localImp = T)
  withoutnoisecomb1a[[i]] <- importance(rf_withoutnoisecomb1, type = 1, scale = F)
}


#Combined variable version 2
withoutnoisecomb2a <- list()

for (i in 1:10) {
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
  
  withoutnoisecomb_data2 <- rbind(group1, group2)
  
  rf_withoutnoisecomb2 <- randomForest(y~., data = withoutnoisecomb_data2,
                                       importance = T, localImp = T)
  
  withoutnoisecomb2a[[i]] <- importance(rf_withoutnoisecomb2, type = 1, scale = F)
}


compareplot_imp(withoutnoisecomb1a, withoutnoisecomb2a, "withoutnoisecomb2a.png")
