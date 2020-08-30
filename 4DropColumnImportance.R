#Function to see whether dropping a column would change the importance
full_rf <- randomForest(TARGET~., data = sample_kaggle, importance = T, localImp = T)
full_imp <- importance(full_rf, type = 1, scale = F)
full_colnames <- colnames(sample_kaggle) %>% head(-1)

kaggle_imp_list <- list()
for (col in 1:length(full_colnames)) {
  current_kaggle <- sample_kaggle[,!(colnames(sample_kaggle) == full_colnames[col])]
  current_kaggle_rf <- randomForest(TARGET~., data = current_kaggle, importance = T,
                             localImp = T)
  kaggle_imp_list[[col]] <- importance(current_kaggle_rf, type = 1, scale = F)
}
#plotting the comparation for all column in sample_kaggle
for (i in 1:length(full_colnames)) {
  compareplot_imp_wolist(full_imp, kaggle_imp_list[[i]], 
                  paste("sample_kaggle_column",i,".png", sep = ""))
}
