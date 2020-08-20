drop_column_importance <- function(rftable){
  #y is the last column in rftable
  library(tidyverse)
  library(randomForest)
  full_rf <- randomForest(y~., data = rftable, importance = T, localImp = T)
  full_rsq <- -1*mean(full_rf$err.rate)
  
  full_importance <- c()
  full_colnames <- colnames(rftable) %>% head(-1)
  
  for (col in full_colnames) {
    current_data <- rftable[,!(colnames(rftable) == col)]
    current_rf <- randomForest(y~., data = current_data, importance = T,
                               localImp = T)
    current_rsq <- -1*mean(current_rf$err.rate)
    diff_rsq <- full_rsq - current_rsq
    full_importance <- c(full_importance, diff_rsq)
  }
  all_importance <- data.frame(variable = full_colnames, importance = full_importance)
  row.names(all_importance) <- all_importance[,1]
  all_importance[,1] <- NULL
  return(all_importance)
}

testing <- drop_column_importance(withoutnoisenorm_data)
testing
plot(testing)

testing <- list()
for (i in 1:10) {
  group1 <- normalgroup1(1500, 5, 0, 1, 0)
  group2 <- normalgroup2(1000, 5, 0, 0.1, 1, 1)
  testing_data <- rbind(group1, group2)
  testing[[i]] <- drop_column_importance(testing_data)
}
testing
compareplot_imp(testing, testing, "testing.png")
