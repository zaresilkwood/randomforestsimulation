summary(analysisdata)
str(analysisdata)
boxplot(analysisdata$AMT_INCOME_TOTAL)

#Income
#Boxplot 
analysisdata %>%
  ggplot(aes(y=AMT_INCOME_TOTAL)) +
  geom_boxplot() + 
  labs(y ="Income") +
  ggsave("incomeboxplot.png", width = 2,
         path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")

#Histogram
analysisdata %>%
  ggplot(aes(x=AMT_INCOME_TOTAL)) + 
  geom_histogram(binwidth = 100000) +
  labs(y = "Number of People", x ="Income") +
  ggsave("incomehistogram.png", width = 6,
         path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")

analysisdata %>%
  str()

#Days Birth
#Boxplot 
analysisdata %>%
  ggplot(aes(y=DAYS_BIRTH)) +
  geom_boxplot() +
  labs(y = "Days of Birth") +
  ggsave("birthboxplot.png", width = 2,
         path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")

#Histogram
analysisdata %>%
  ggplot(aes(x=DAYS_BIRTH)) + 
  geom_histogram(binwidth = 2000) +
  labs(y = "Number of People", x = "Days of Birth") +
  ggsave("birthhistogram.png", width = 6,
         path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")


#Days Employed
#Boxplot 
analysisdata %>%
  ggplot(aes(y=DAYS_EMPLOYED)) +
  geom_boxplot() +
  labs(y = "Days Employed") +
  ggsave("employedboxplot.png", width = 2,
         path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")

#Histogram
analysisdata %>%
  ggplot(aes(x=DAYS_EMPLOYED)) + 
  geom_histogram(binwidth = 1000) +
  labs(y = "Number of People", x = "Days Employed") +
  ggsave("employedhistogram.png", width = 6,
         path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")