### NORMAL FUNCTION FOR CONTINUOUS VARIABLE ###

### Function to generate normal dataset ###
normalgroup1 <- function(nobservation, nvariable, mean_variable, sd_variable, yvalue){
  normal_name <- list()
  normal_list <- list()
  for (i in 1:nvariable) {
    normal_name[i] <- paste("Xn", sprintf("%02d", i), sep = "")
    normal_list[[i]] <- rnorm(nobservation, mean = mean_variable, sd = sd_variable)
  }
  normal_name <- unlist(normal_name, use.names = FALSE)
  normal_list <- do.call(cbind, normal_list)
  colnames(normal_list) <- normal_name
  y <- rep(yvalue, nobservation)
  normal_list <- data.frame(cbind(normal_list, y))
  normal_list$y <- as.factor(normal_list$y)
  return(normal_list)
}


### Function to generate normal dataset, with different mean factor ###
## mean factor increase by i*mean_factor, from 0 ##
normalgroup2 <- function(nobservation, nvariable, mean_begin, meanfactor,
                         sd_variable, yvalue){
  normal_name <- list()
  normal_list <- list()
  for (i in 1:nvariable) {
    normal_name[i] <- paste("Xn", sprintf("%02d", i), sep = "")
    normal_list[[i]] <- rnorm(nobservation, mean = mean_begin + (i-1)*meanfactor, 
                              sd = sd_variable)
  }
  normal_name <- unlist(normal_name, use.names = FALSE)
  normal_list <- do.call(cbind, normal_list)
  colnames(normal_list) <- normal_name
  y <- rep(yvalue, nobservation)
  normal_list <- data.frame(cbind(normal_list, y))
  normal_list$y <- as.factor(normal_list$y)
  return(normal_list)
}


### Function to generate normal dataset, with different sd ###
## sd increase by i*sdfactor, from 0 ##
normalgroup3 <- function(nobservation, nvariable, mean_variable, sd_beginning,
                         sd_factor, yvalue){
  normal_name <- list()
  normal_list <- list()
  for (i in 1:nvariable) {
    normal_name[i] <- paste("Xn", sprintf("%02d", i), sep = "")
    normal_list[[i]] <- rnorm(nobservation, mean = mean_variable, 
                              sd = sd_beginning + (i-1)*sd_factor)
  }
  normal_name <- unlist(normal_name, use.names = FALSE)
  normal_list <- do.call(cbind, normal_list)
  colnames(normal_list) <- normal_name
  y <- rep(yvalue, nobservation)
  normal_list <- data.frame(cbind(normal_list, y))
  normal_list$y <- as.factor(normal_list$y)
  return(normal_list)
}


### BINOMIAL FUNCTION FOR DISCRETE VARIABLE ###
### Function to generate binomial dataset ###
binomgroup1 <- function(nobservation, nvariable, trials, probability, yvalue){
  binom_name <- list()
  binom_list <- list()
  for (i in 1:nvariable) {
    binom_name[i] <- paste("Xb", sprintf("%02d", i), sep = "")
    binom_list[[i]] <- rbinom(nobservation, size = trials, prob = probability)
  }
  binom_name <- unlist(binom_name, use.names = FALSE)
  binom_list <- do.call(cbind, binom_list)
  colnames(binom_list) <- binom_name
  y <- rep(yvalue, nobservation)
  binom_list <- data.frame(cbind(binom_list, y))
  binom_list$y <- as.factor(binom_list$y)
  return(binom_list)
}


### Function to generate binomial dataset, with different trial factor ##
binomgroup2 <- function(nobservation, nvariable, trials, prob_begin, prob_end, yvalue){
  binom_name <- list()
  binom_list <- list()
  probability <- seq(prob_begin, prob_end, length.out = nvariable)
  for (i in 1:nvariable) {
    binom_name[i] <- paste("Xb", sprintf("%02d", i), sep = "")
    binom_list[[i]] <- rbinom(nobservation, size = trials, prob = probability[i])
  }
  binom_name <- unlist(binom_name, use.names = FALSE)
  binom_list <- do.call(cbind, binom_list)
  colnames(binom_list) <- binom_name
  y <- rep(yvalue, nobservation)
  binom_list <- data.frame(cbind(binom_list, y))
  binom_list$y <- as.factor(binom_list$y)
  return(binom_list)
}


### Function to generate binomial dataset, with different probability factor ##
binomgroup3 <- function(nobservation, nvariable, trials_begin, trials_end, 
                        probability, yvalue){
  binom_name <- list()
  binom_list <- list()
  trials <- seq(trials_begin, trials_end, length.out = nvariable)
  for (i in 1:nvariable) {
    binom_name[i] <- paste("Xb", sprintf("%02d", i), sep = "")
    binom_list[[i]] <- rbinom(nobservation, size = trials[i], prob = probability)
  }
  binom_name <- unlist(binom_name, use.names = FALSE)
  binom_list <- do.call(cbind, binom_list)
  colnames(binom_list) <- binom_name
  y <- rep(yvalue, nobservation)
  binom_list <- data.frame(cbind(binom_list, y))
  binom_list$y <- as.factor(binom_list$y)
  return(binom_list)
}


### CATEGORICAL VARIABLE ###

### First Group (Y = 0) ###

categoricalgroup1 <- function(nobservation, nvariable, ncategory1, ncategory2,
                              yvalue){
  categ_name <- list()
  categ_list <- list()
  for (i in 1:nvariable) {
    categ_name[i] <- paste("Xc", sprintf("%02d", i), sep = "")
    categ_list[[i]] <- sample(LETTERS[ncategory1:ncategory2],
                              nobservation, replace = TRUE)
  }
  categ_name <- unlist(categ_name, use.names = FALSE)
  categ_list <- do.call(cbind, categ_list)
  colnames(categ_list) <- categ_name
  y <- rep(yvalue, nobservation)
  categ_list <- data.frame(cbind(categ_list, y))
  col_names <- names(categ_list)
  categ_list[,col_names] <- lapply(categ_list[,col_names], factor)
  return(categ_list)
}


### Second Group - Number of Categorical data Increase by incr_factor (Y = 0) ###

categoricalgroup2 <- function(nobservation, nvariable, ncategory1, ncategory2,
                              incr_factor, yvalue){
  categ_name <- list()
  categ_list <- list()
  for (i in 1:nvariable) {
    categ_name[i] <- paste("Xc", sprintf("%02d", i), sep = "")
    categ_list[[i]] <- sample(LETTERS[ncategory1:(ncategory2+(i-1)*incr_factor)],
                              nobservation, replace = TRUE)
  }
  categ_name <- unlist(categ_name, use.names = FALSE)
  categ_list <- do.call(cbind, categ_list)
  colnames(categ_list) <- categ_name
  y <- rep(yvalue, nobservation)
  categ_list <- data.frame(cbind(categ_list, y))
  col_names <- names(categ_list)
  categ_list[,col_names] <- lapply(categ_list[,col_names], factor)
  return(categ_list)
}


categoricalgroup3 <- function(nobservation, nvariable, ncategory1, ncategory2,
                              incr_factor, yvalue){
  categ_name <- list()
  categ_list <- list()
  for (i in 1:nvariable) {
    categ_name[i] <- paste("Xc", sprintf("%02d", i), sep = "")
    x <- seq(ncategory1:(ncategory2+(i-1)*incr_factor))
    sumx <- sum(x)
    categ_list[[i]] <- sample(LETTERS[ncategory1:(ncategory2+(i-1)*incr_factor)],
                              nobservation, replace = TRUE, prob = x/sumx)
  }
  categ_name <- unlist(categ_name, use.names = FALSE)
  categ_list <- do.call(cbind, categ_list)
  colnames(categ_list) <- categ_name
  y <- rep(yvalue, nobservation)
  categ_list <- data.frame(cbind(categ_list, y))
  col_names <- names(categ_list)
  categ_list[,col_names] <- lapply(categ_list[,col_names], factor)
  return(categ_list)
}

#####################################################################################
### COMBINING Y = 0 and Y = 1 ### ##NB. PROBABLY DONT NEED THIS AND BELOW FUNCTION!##

## NORMAL ##
normalcombine <- function(nobservation, nvariable, mean1stgroup, meanfactor, sdall){
  datacombined <- data.frame(rbind(normalgroup1(nobservation/2, nvariable,
                                                mean1stgroup, sdall),
                                   normalgroup2(nobservation/2, nvariable,
                                                meanfactor, sdall)))
  datacombined$y <- as.factor(datacombined$y)
  return(datacombined)
} 

group1 <- normalcombine(10000, 10, 0, 0.1, 1) 
summary(group1)
str(group1)

## BINOMIAL ##
binomcombine <- function(nobservation, nvariable, trials, probability,
                         prob_begin, prob_end){
  datacombined <- data.frame(rbind(binomgroup1(nobservation/2, nvariable, trials,
                                               probability),
                                   binomgroup2(nobservation/2, nvariable, trials,
                                               prob_begin, prob_end)))
  datacombined$y <- as.factor(datacombined$y)
  return(datacombined)
}

binom1 <- binomcombine(10000, 10, 1, 0.5, 0.5, 0.6)
summary(binom1)
str(binom1)


## CATEGORICAL ##
## ON PROGRESS ##

### COMPARATION FUNCTION ###
compare_function <- function(dataset11, dataset12, dataset21, dataset22){
  library(randomForest)
  dataset1 <- data.frame(rbind(dataset11, dataset12))
  dataset2 <- data.frame(rbind(dataset21, dataset22))
  dataset1$y <- as.factor(dataset1$y)
  dataset2$y <- as.factor(dataset2$y)
  simulate_random1 <- randomForest(y~., data = dataset1, importance = TRUE)
  simulate_random2 <- randomForest(y~., data = dataset2, importance = TRUE)
  ImportanceComparison <- data.frame(importance(simulate_random1, type = 1),
                                     importance(simulate_random2, type = 1),
                                     importance(simulate_random1, type = 2),
                                     importance(simulate_random2, type = 2))
  colnames(ImportanceComparison) <- c("MeanDecrAcc1", "MeanDecrAcc2",
                                      "MeanDecrGini1", "MeanDecrGini2")
  return(ImportanceComparison)
}


