#with mean factor 0.1
withoutnoisenorm1x <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 10, 1, 1, 0)
  group1_colnames <- colnames(group1)
  group2a <- normalgroup2(1000, 5, 1, 1, 1, 1)
  group2a$y <- NULL
  group2b <- normalgroup3(1000, 5, 1, 1, 1, 1)
  group2 <- cbind(group2a, group2b)
  colnames(group2) <- group1_colnames
  withoutnoisenorm_data <- rbind(group1, group2)
  rf_withoutnoisenorm1 <- randomForest(y~., data = withoutnoisenorm_data, importance = T,
                                       localImp = T)
  withoutnoisenorm1x[[i]] <- importance(rf_withoutnoisenorm1, type = 1, scale = F)
}

#increasing mean factor to 0.2
withoutnoisenorm2x <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 10, 1, 1, 0)
  group1_colnames <- colnames(group1)
  group2a <- normalgroup2(1000, 5, 1, 2, 1, 1)
  group2a$y <- NULL
  group2b <- normalgroup3(1000, 5, 1, 1, 2, 1)
  group2 <- cbind(group2a, group2b)
  colnames(group2) <- group1_colnames
  withoutnoisenorm_data2 <- rbind(group1, group2)
  rf_withoutnoisenorm2 <- randomForest(y~., data = withoutnoisenorm_data2, importance = T,
                                       localImp = T)
  withoutnoisenorm2x[[i]] <- importance(rf_withoutnoisenorm2, type = 1, scale = F)
}

compareplot_imp(withoutnoisenorm1x, withoutnoisenorm2x, "testing_withoutnoisenorm_combine.png")