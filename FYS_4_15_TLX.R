invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
################################################################################
# Get relevant data and variables like Accuracy, from raw data

library(reshape2)
library(tidyverse)

dataWide <- read.csv("AirTrafficControllers-ΒehaviouralData.csv", header=TRUE)
dataWide <- dataWide[, c("Subject", "Session", "SessionTime", "EstimationSessionTime", "PJOT",
                         "MentalTLX", "PhysicalTLX", "TemporalTLX", "PerformanceTLX", "EffortTLX", "FrustrationTLX")]
dataWide$EstimationTime <- dataWide$EstimationSessionTime*60
dataWide$Estimation_Accuracy <- dataWide$EstimationTime/dataWide$SessionTime
dataWide$TLX <- (dataWide$MentalTLX+dataWide$PhysicalTLX+dataWide$TemporalTLX+dataWide$PerformanceTLX+dataWide$EffortTLX+dataWide$FrustrationTLX)/6


colnames(dataWide)[colnames(dataWide) == "Session"] ="SessionTemp"

dataWide<-dataWide%>%mutate(Session = case_when(
  SessionTemp=="ee1" ~ "1H-Gr",
  SessionTemp=="ee2" ~ "1H-Eng",
  SessionTemp=="ee3" ~ "2H-Gr",
  SessionTemp=="ee4" ~ "2H-Eng"
))

dataWide<-dataWide%>%mutate(Helicopters = case_when(
  SessionTemp=="ee1" ~ "one",
  SessionTemp=="ee2" ~ "one",
  SessionTemp=="ee3" ~ "two",
  SessionTemp=="ee4" ~ "two"
))

dataWide<-dataWide%>%mutate(Language = case_when(
  SessionTemp=="ee1" ~ "Greek",
  SessionTemp=="ee2" ~ "English",
  SessionTemp=="ee3" ~ "Greek",
  SessionTemp=="ee4" ~ "English"
))


dataWide$Subject <- factor(dataWide$Subject)
dataWide$Language <- factor(dataWide$Language, levels = c("Greek", "English"))
cols <- c("Subject", "Helicopters")
dataWide[cols] <- lapply(dataWide[cols], factor)
colnames(dataWide)[colnames(dataWide) == "PJOT"] ="Passage_Of_Time"
dataWide <- dataWide[, c("Subject", "Helicopters", "Language", "Estimation_Accuracy","Passage_Of_Time",
                         "MentalTLX", "PhysicalTLX", "TemporalTLX", "PerformanceTLX", "EffortTLX", "FrustrationTLX", "TLX")]


dataLong <- dataWide
dataLong <- dataLong[order(dataLong$Subject, dataLong$Helicopters), ]

###############################################################################

library(ggpubr)
library(rstatix)

# Assumptions
# Outliers
# Use the Rosner Test for small samples
library(EnvStats)

library(nlme)
library(car)

Model.eo1 <- lm(TLX ~ Helicopters, data = dataLong)

influenceIndexPlot(Model.eo1)
outlierTest(Model.eo1)

# ANOVA

library(nlme)

res.aov1 <- lme(
  TLX ~ Helicopters,          # Fixed effects for main effects and interaction
  random = ~ 1 | Subject,                    # Random intercepts for each Subject
  data = dataLong
)
summary(res.aov1)

# Post-hocs

pwc1 <- dataLong %>%
  pairwise_t_test(
    TLX ~ Helicopters, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
pwc1

dataLong %>%
  group_by(Helicopters) %>%
  get_summary_stats(TLX, type = "mean_sd")

dataLong %>%
  group_by(Language) %>%
  get_summary_stats(TLX, type = "mean_sd")
