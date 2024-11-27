invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
################################################################################
# Get relevant data and variables like Accuracy, from raw data

library(reshape2)
library(tidyverse)

data <- read.csv("AirTrafficControllers-PhysiologicalData.csv", header=TRUE)

data<-data%>%mutate(Language = case_when(
  Language=="greek" ~ "Greek",
  Language=="english" ~ "English"
))

data<-data%>%mutate(Label = case_when(
  Label=="p1" ~ "cruising",
  Label=="p2" ~ "landing"
))

data$Language <- factor(data$Language, levels = c("Greek", "English"))
colnames(data)[colnames(data) == "Label"] ="Phase"
colnames(data)[colnames(data) == "SCR_Peaks_N"] ="SCR_Peaks"
colnames(data)[colnames(data) == "sdnn"] ="HRV"

data$Subject <- factor(data$Subject)
data$Helicopters <- factor(data$Helicopters)
data$Phase <- factor(data$Phase, levels = c("cruising", "landing"))
data<- data[order(data$Subject, data$Helicopters, data$Phase), ]

################################################################################
library(ggpubr)

bxp1 <- ggboxplot(
  data, x = "Helicopters", y = "SCR_Peaks",
  color = "Phase", palette = "npg", short.panel.labs = FALSE
)
bxp1


bxp2 <- ggboxplot(
  data, x = "Helicopters", y = "HRV",
  color = "Phase", palette = "npg", short.panel.labs = FALSE
)
bxp2

# Checken voor uitbijters met residual statistics

library(nlme)
library(car)

Model.eo1 <- lm(SCR_Peaks ~ Helicopters*Language*Phase, data = data)

influenceIndexPlot(Model.eo1)
outlierTest(Model.eo1)

Model.eo2 <- lm(HRV ~ Helicopters*Language*Phase, data = data)

influenceIndexPlot(Model.eo2)
outlierTest(Model.eo2)

dataHRVno <- data[-c(95), ]

# ANOVA

library(EnvStats)
library(ggpubr)
library(rstatix)

res.aov1 <- anova_test(
  data = data, dv = SCR_Peaks, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov1)


res.aov2 <- anova_test(
  data = data, dv = HRV, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov2)

res.aov2.1 <- anova_test(
  data = dataHRVno, dv = HRV, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov2.1)
