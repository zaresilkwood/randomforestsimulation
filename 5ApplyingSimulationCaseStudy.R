#From Simulation Data

simulation_data <- data.frame(do.call("cbind", simulation_case))
imp_sim_mean <- rowMeans(data.frame(simulation_data))*10
imp_sim_sd <- apply(data.frame(simulation_data), 1, sd)*10
datatype <- rep("Simulation", length(imp_sim_mean))

rdy_to_plot1 <- data.frame(datatype, variable = rownames(data.frame(simulation_data)),
                           mean = imp_sim_mean,
                           sd = imp_sim_sd)

#From Sample Random Forest
imp_sample_mean <- importance(sample_rf, type = 1, scale = F)
colnames(imp_sample_mean) <- c("mean")
imp_sample_sd <- rep(0, length(imp_real))
datatype <- rep("Sample Data", length(imp_real))

rdy_to_plot2 <- data.frame(datatype, variable = rownames(imp_real),
                           mean = imp_sample_mean,
                           sd = imp_sample_sd)

rdy_to_plot <- rbind(rdy_to_plot1, rdy_to_plot2)

rdy_to_plot %>%
  mutate(name = fct_reorder(variable, mean)) %>%
  ggplot(aes(fill = datatype, x=name, y=mean)) +
  geom_bar(position="dodge", stat="identity", alpha=.6) +
  geom_errorbar(aes(x=variable, ymin=mean-sd, ymax=mean+sd),
                position=position_dodge(.9),
                width=0.2, colour="orange", alpha=0.9, size=0.8) +
  coord_flip() +
  labs(x = "Variable", y = "Average Mean Decrease Accuracy") +
  theme_bw() +
  ggsave("samplevssimulation.png", width = 6, height = 4.2,
         path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")
