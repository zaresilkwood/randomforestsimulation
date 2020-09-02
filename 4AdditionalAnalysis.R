
library(data.table)

kaggle_imp_list2 <- kaggle_imp_list

for (i in 1:length(kaggle_imp_list2)) {
  colnames(kaggle_imp_list2[[i]]) <- paste("MDA",i,sep = "")
}
table_imp_list <- Reduce(function(kaggle_imp_list2, y) merge(kaggle_imp_list2, y, all = TRUE), 
       lapply(kaggle_imp_list2, function(y) data.table(y, keep.rownames=TRUE, key = "rn")))
table_imp_list <- table_imp_list[,c(1, 6, 5, 2, 10, 11, 14, 3, 4, 13, 12, 15, 7, 8, 9)]
table_imp_list$MDAAll <- full_imp

difference_imp_list <- list()
for (i in 1:(length(table_imp_list)-2)) {
  difference_imp_list[[i]] <- table_imp_list[[length(table_imp_list)]] - table_imp_list[[i+1]]
}

difference_imp_table <- data.frame(table_imp_list[[1]], do.call("cbind", difference_imp_list))
difference_imp_table[is.na(difference_imp_table)] <- 0

colSums(difference_imp_table[,2:15])
