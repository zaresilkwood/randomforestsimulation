#with mean factor 0.1
withoutnoisenorm1c <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 500, 25, 0)
  group2 <- normalgroup2(1000, 5, 500, 50, 25, 1)
  withoutnoisenorm_data <- rbind(group1, group2)
  rf_withoutnoisenorm1 <- randomForest(y~., data = withoutnoisenorm_data, importance = T,
                                       localImp = T)
  withoutnoisenorm1c[[i]] <- importance(rf_withoutnoisenorm1, type = 1, scale = F)
}

#increasing mean factor to 0.2
withoutnoisenorm2c <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 500, 25, 0)
  group2 <- normalgroup2(1000, 5, 500, 60, 25, 1)
  withoutnoisenorm_data2 <- rbind(group1, group2)
  rf_withoutnoisenorm2 <- randomForest(y~., data = withoutnoisenorm_data2, importance = T,
                                       localImp = T)
  withoutnoisenorm2c[[i]] <- importance(rf_withoutnoisenorm2, type = 1, scale = F)
}

compareplot_imp(withoutnoisenorm1c, withoutnoisenorm2c, "withoutnoisenorm3c.png")


#CHANGING THE SD
#WITH OTHER DATA TYPE

#with mean factor 0.1
withoutnoisenorm1d <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 500, 25, 0)
  group2 <- normalgroup2(1000, 5, 500, 100, 25, 1)
  withoutnoisenorm_data <- rbind(group1, group2)
  rf_withoutnoisenorm1 <- randomForest(y~., data = withoutnoisenorm_data, importance = T,
                                       localImp = T)
  withoutnoisenorm1d[[i]] <- importance(rf_withoutnoisenorm1, type = 1, scale = F)
}

#increasing mean factor to 0.2
withoutnoisenorm2d <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 500, 25, 0)
  group2 <- normalgroup2(1000, 5, 500, 200, 25, 1)
  withoutnoisenorm_data2 <- rbind(group1, group2)
  rf_withoutnoisenorm2 <- randomForest(y~., data = withoutnoisenorm_data2, importance = T,
                                       localImp = T)
  withoutnoisenorm2d[[i]] <- importance(rf_withoutnoisenorm2, type = 1, scale = F)
}

compareplot_imp(withoutnoisenorm1d, withoutnoisenorm2d, "withoutnoisenorm4d.png")


#with sd factor 0.1
withoutnoisenorm5a <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 500, 25, 0)
  group2 <- normalgroup3(1000, 5, 500, 25, 10, 1)
  withoutnoisenorm_datab <- rbind(group1, group2)
  rf_withoutnoisenorm1b <- randomForest(y~., data = withoutnoisenorm_datab, importance = T,
                                        localImp = T)
  withoutnoisenorm5a[[i]] <- importance(rf_withoutnoisenorm1b, type = 1, scale = F)
}

#increasing sd factor to 0.2
withoutnoisenorm5b <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 500, 25, 0)
  group2 <- normalgroup3(1000, 5, 500, 25, 30, 1)
  withoutnoisenorm_data2b <- rbind(group1, group2)
  rf_withoutnoisenorm2b <- randomForest(y~., data = withoutnoisenorm_data2b, importance = T,
                                        localImp = T)
  withoutnoisenorm5b[[i]] <- importance(rf_withoutnoisenorm2b, type = 1, scale = F)
}

compareplot_imp(withoutnoisenorm5a, withoutnoisenorm5b, "withoutnoisenorm5big.png")


#ANOTHER AGAIN!!

#with sd factor 0.1
withoutnoisenorm6a <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 500, 25, 0)
  group2 <- normalgroup3(1000, 5, 500, 25, 50, 1)
  withoutnoisenorm_datab <- rbind(group1, group2)
  rf_withoutnoisenorm1b <- randomForest(y~., data = withoutnoisenorm_datab, importance = T,
                                        localImp = T)
  withoutnoisenorm6a[[i]] <- importance(rf_withoutnoisenorm1b, type = 1, scale = F)
}

#increasing sd factor to 0.2
withoutnoisenorm6b <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 500, 25, 0)
  group2 <- normalgroup3(1000, 5, 500, 25, 100, 1)
  withoutnoisenorm_data2b <- rbind(group1, group2)
  rf_withoutnoisenorm2b <- randomForest(y~., data = withoutnoisenorm_data2b, importance = T,
                                        localImp = T)
  withoutnoisenorm6b[[i]] <- importance(rf_withoutnoisenorm2b, type = 1, scale = F)
}

compareplot_imp(withoutnoisenorm6a, withoutnoisenorm6b, "withoutnoisenorm6big.png")


#SMALLER NUMBER BUT DIFFERENT MEAN
withoutnoisenorm1e <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 5, 1, 0)
  group2 <- normalgroup2(1000, 5, 5, 5, 1, 1)
  withoutnoisenorm_data <- rbind(group1, group2)
  rf_withoutnoisenorm1 <- randomForest(y~., data = withoutnoisenorm_data, importance = T,
                                       localImp = T)
  withoutnoisenorm1e[[i]] <- importance(rf_withoutnoisenorm1, type = 1, scale = F)
}

#increasing mean factor to 0.2
withoutnoisenorm2e <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 5, 1, 0)
  group2 <- normalgroup2(1000, 5, 5, 5, 1, 1)
  withoutnoisenorm_data2 <- rbind(group1, group2)
  rf_withoutnoisenorm2 <- randomForest(y~., data = withoutnoisenorm_data2, importance = T,
                                       localImp = T)
  withoutnoisenorm2e[[i]] <- importance(rf_withoutnoisenorm2, type = 1, scale = F)
}

compareplot_imp(withoutnoisenorm1e, withoutnoisenorm2e, "withoutnoisenorm7.png")
