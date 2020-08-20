library(readr)
credit_card_approval <- read_csv("credit_card_approval.csv")
str(credit_card_approval)

#Data Cleaning
colSums(is.na(credit_card_approval))
#No NA collumns

#remove ID and BEGIN_MONTHS
drops <- c("ID", "BEGIN_MONTHS", "FLAG_MOBIL")
analysisdata <- credit_card_approval [,!(names(credit_card_approval) %in% drops)]

summary(analysisdata)
str(analysisdata)

analysisdata$DAYS_BIRTH <- abs(analysisdata$DAYS_BIRTH)
analysisdata$DAYS_EMPLOYED <- abs(analysisdata$DAYS_EMPLOYED)
analysisdata$CODE_GENDER <- as.factor(analysisdata$CODE_GENDER)
analysisdata$FLAG_OWN_CAR <- as.factor(analysisdata$FLAG_OWN_CAR)
analysisdata$FLAG_OWN_REALTY <- as.factor(analysisdata$FLAG_OWN_REALTY)

#CHILDREN ORDINAL LEVEL
unique(analysisdata$CNT_CHILDREN)
analysisdata$CNT_CHILDREN <- factor(analysisdata$CNT_CHILDREN, ordered = TRUE,
                                       levels = c("No children", "1 children",
                                                  "2+ children"))

analysisdata$NAME_EDUCATION_TYPE <- as.factor(analysisdata$NAME_EDUCATION_TYPE)
analysisdata$NAME_FAMILY_STATUS <- as.factor(analysisdata$NAME_FAMILY_STATUS)
analysisdata$NAME_HOUSING_TYPE <- as.factor(analysisdata$NAME_HOUSING_TYPE)
analysisdata$FLAG_WORK_PHONE <- as.factor(analysisdata$FLAG_WORK_PHONE)
analysisdata$FLAG_PHONE <- as.factor(analysisdata$FLAG_PHONE)
analysisdata$FLAG_EMAIL <- as.factor(analysisdata$FLAG_EMAIL)
analysisdata$JOB <- as.factor(analysisdata$JOB)
analysisdata$STATUS <- as.factor(analysisdata$STATUS)
analysisdata$TARGET <- as.factor(analysisdata$TARGET)

str(analysisdata)

#DIVIDE THE SAMPLE TO TARGET = 1 and TARGET = 0
datatarget1 <- analysisdata[analysisdata$TARGET == "1",]
datatarget0 <- analysisdata[analysisdata$TARGET == "0",]

#SAMPLING FROM DATA
#40% target, 60% non target. We will use n = 2500, target = 1000, non target = 1500
set.seed(2020)
sampletarget1 <- datatarget1[sample(nrow(datatarget1), 1000, replace = F), ]
sampletarget0 <- datatarget0[sample(nrow(datatarget0), 1500, replace = F), ]

datasample <- rbind(sampletarget1, sampletarget0)
