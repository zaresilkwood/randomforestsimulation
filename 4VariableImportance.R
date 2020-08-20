#This command is for Drop Importance
library(randomForest)
library(forcats)
library(dplyr)
library(ggplot2)
library(tidyverse)

#
str(datasample)
sample_rfall <- randomForest(TARGET~., data = datasample, importance = T, localImp = T)
plotimportance(sample_rfall, "importanceall.png")

#Removing Status because it cause imbalance in data 
sample_kaggle <- subset(datasample, select = -c(STATUS))
sample_rf <- randomForest(TARGET~., data = sample_kaggle, importance = T, localImp = T)
plotimportance(sample_rf, "importancewithoutstatus.png")

#Add random variable 
random_data5 <- data.frame(rnorm(2500, mean = 1000, sd = 100),
                           rbinom(2500, 1, 0.5),
                           factor(sample(LETTERS[1:4], 2500, replace = TRUE)))

sample_kaggle_wrandom <- cbind(random_data5, sample_kaggle)
names(sample_kaggle_wrandom)[1:3] <- c("RANDOMN", "RANDOMD", "RANDOMC")

sample_rf_wrandom <- randomForest(TARGET~., data = sample_kaggle_wrandom,
                                  importance = T, localImp = T)
plotimportance(sample_rf_wrandom, "importancewithoutstatuswrandom.png")
