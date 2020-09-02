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



#Function to Check Plot Importance ==> Drop Column Importance Function
drop_column_function <- function(rftable, plotname){
  full_rf <- randomForest(TARGET~., data = rftable, importance = T, localImp = T)
  full_rsq <- -1*mean(full_rf$err.rate)
  
  full_importance <- c()
  full_colnames <- colnames(rftable) %>% head(-1)
  
  for (col in full_colnames) {
    current_data <- rftable[,!(colnames(rftable) == col)]
    current_rf <- randomForest(TARGET~., data = current_data, importance = T,
                               localImp = T)
    current_rsq <- -1*mean(current_rf$err.rate)
    diff_rsq <- full_rsq - current_rsq
    full_importance <- c(full_importance, diff_rsq)
  }
  all_importance <- data.frame(variable = full_colnames, importance = full_importance)
  
  all_importance %>% 
    mutate(name = fct_reorder(variable, importance)) %>% 
    ggplot(aes(x=name, y=importance)) +
    geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
    coord_flip() +
    labs(x = "Variable", y = "Importance") +
    theme_bw() +
    ggsave(plotname, width = 6,
           path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")
}

str(sample_kaggle)
drop_column_function(sample_kaggle, "samplewithoutrnadom.png")

#NEXT STEP, CREATE RANDOM SAMPLE FOR THIS, and then APPLY TO THE FUNCTION
set.seed(2020)
random_data1 <- data.frame(rnorm(2500, mean = 1000, sd = 100))
random_data2 <- data.frame(rnorm(2500, mean = 1000, sd = 100),
                           rnorm(2500, mean = 2000, sd = 200),
                           rnorm(2500, mean = 3000, sd = 300))
sample_random1 <- cbind(random_data1, sample_kaggle)
names(sample_random1)[1] <- c("RANDOM")

drop_column_function(sample_random1, "samplerandom1.png")

sample_random2 <- cbind(random_data2, sample_kaggle)
names(sample_random2)[1:3] <- c("RANDOM1", "RANDOM2", "RANDOM3")

drop_column_function(sample_random2, "samplerandom2.png")

#ADDING MORE RANDOM VARIABLE 
#BINOMIAL VARIABLE
random_data3 <- rbinom(2500, 1, 0.5)
sample_random3 <- cbind(random_data3, sample_kaggle)
names(sample_random3)[1] <- c("RANDOM")

drop_column_function(sample_random3, "samplerandom3.png")

#CATEGORICAL VARIABLE
random_data4 <- factor(sample(LETTERS[1:4], 2500, replace = TRUE))
sample_random4 <- cbind(random_data4, sample_kaggle)
names(sample_random4)[1] <- c("RANDOM")

drop_column_function(sample_random4, "samplerandom4.png")

#ALL RANDOM VARIABLE
sample_randomall <- cbind(random_data1, random_data3, random_data4, sample_kaggle)
names(sample_randomall)[1:3] <- c("NORMAL", "BINOMIAL", "CATEGORICAL")

drop_column_function(sample_randomall, "samplerandomall.png")
