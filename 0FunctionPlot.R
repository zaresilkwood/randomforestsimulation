#Function to Plot Random Forest Importance from List of Result from Loop
########### Plot Simulation Importance Function ############


### REMINDER ###
### plotimportance >> require random forest object
### plot_imp_from_list >> require list of many random forest trial
### compareplot_imp >> comparing two list of result from random forest
### plot_imp_list >> require list of many random forest trial, result will be ordered

#Function to Create Plot from Random Forest
plotimportance <- function(rfname, saverf){
  importance(rfname, type = 1, scale = F) %>%
    data.frame() %>%
    rownames_to_column(var = "Variable") %>%
    mutate(name = fct_reorder(Variable, MeanDecreaseAccuracy)) %>%
    ggplot(aes(x=name, y=MeanDecreaseAccuracy)) +
    geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
    coord_flip() +
    labs(x = "Variable", y = "Mean Decrease Accuracy") +
    theme_bw() +
    ggsave(saverf, width = 6, height = 4.2,
           path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")
}

plot_imp_from_list <- function(list_table, figure_name){
  library(ggplot2)
  library(tidyverse)
  list_ave <- data.frame(do.call("cbind", list_table))
  list_mean <- rowMeans(list_ave)
  list_sd <- apply(list_ave, 1, sd)
  
  rdy_to_plot <- data.frame(variable = rownames(list_ave),
                            mean = list_mean,
                            sd = list_sd)
  
  rdy_to_plot %>%
    ggplot(aes(x=variable, y=mean)) +
    geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
    geom_errorbar(aes(x=variable, ymin=mean-sd, ymax=mean+sd), width=0.2,
                  colour="orange", alpha=0.9, size=0.8) +
    coord_flip() +
    labs(x = "Variable", y = "Average Mean Decrease Accuracy") +
    theme_bw() +
    ggsave(figure_name, width = 6, height = 4.2,
           path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")
}

compareplot_imp <- function(list_table1, list_table2, figure_name){
  library(ggplot2)
  library(tidyverse)
  list_ave1 <- data.frame(do.call("cbind", list_table1))
  list_mean1 <- rowMeans(list_ave1)
  list_sd1 <- apply(list_ave1, 1, sd)
  simulation <- rep("1", length(list_mean1))
  
  rdy_to_plot1 <- data.frame(simulation, variable = rownames(list_ave1),
                             mean = list_mean1,
                             sd = list_sd1)
  
  list_ave2 <- data.frame(do.call("cbind", list_table2))
  list_mean2 <- rowMeans(list_ave2)
  list_sd2 <- apply(list_ave2, 1, sd)
  simulation <- rep("2", length(list_mean2))
  
  rdy_to_plot2 <- data.frame(simulation, variable = rownames(list_ave2),
                             mean = list_mean2,
                             sd = list_sd2)
  
  rdy_to_plot <- rbind(rdy_to_plot1, rdy_to_plot2)
  
  rdy_to_plot %>%
    ggplot(aes(fill = simulation, x=variable, y=mean)) +
    geom_bar(position="dodge", stat="identity", alpha=.6) +
    geom_errorbar(aes(x=variable, ymin=mean-sd, ymax=mean+sd),
                  position=position_dodge(.9),
                  width=0.2, colour="orange", alpha=0.9, size=0.8) +
    coord_flip() +
    labs(x = "Variable", y = "Average Mean Decrease Accuracy") +
    theme_bw() +
    ggsave(figure_name, width = 6, height = 4.2,
           path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")
}

plot_imp_list <- function(list_table, figure_name){
  library(ggplot2)
  library(tidyverse)
  list_ave <- data.frame(do.call("cbind", list_table))
  list_mean <- rowMeans(list_ave)
  list_sd <- apply(list_ave, 1, sd)
  
  rdy_to_plot <- data.frame(variable = rownames(list_ave),
                            mean = list_mean,
                            sd = list_sd)
  
  rdy_to_plot %>%
    mutate(name = fct_reorder(variable, mean)) %>%
    ggplot(aes(x=name, y=mean)) +
    geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
    geom_errorbar(aes(x=variable, ymin=mean-sd, ymax=mean+sd), width=0.2,
                  colour="orange", alpha=0.9, size=0.8) +
    coord_flip() +
    labs(x = "Variable", y = "Average Mean Decrease Accuracy") +
    theme_bw() +
    ggsave(figure_name, width = 6, height = 4.2,
           path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")
}

compareplot_imp_wolist <- function(list_table1, list_table2, figure_name){
  library(ggplot2)
  library(tidyverse)
  simulation <- rep("1", length(list_table1))
  rdy_to_plot1 <- data.frame(simulation, variable = rownames(list_table1), list_table1)
  
  simulation <- rep("2", length(list_table2))
  rdy_to_plot2 <- data.frame(simulation, variable = rownames(list_table2), list_table2)
  
  rdy_to_plot <- rbind(rdy_to_plot1, rdy_to_plot2)
  
  rdy_to_plot %>%
    mutate(name = fct_reorder(variable, MeanDecreaseAccuracy)) %>% 
    ggplot(aes(fill = simulation, x=name, y=MeanDecreaseAccuracy)) +
    geom_bar(position="dodge", stat="identity", alpha=.6) +
    coord_flip() +
    labs(x = "Variable", y = "Average Mean Decrease Accuracy") +
    theme_bw() +
    ggsave(figure_name, width = 6, height = 4.2,
           path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")
}

################################################