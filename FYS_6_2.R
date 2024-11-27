invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
################################################################################
# Get relevant data and variables like Accuracy, from raw data

library(reshape2)
library(tidyverse)

data <- read.csv("AirTrafficControllers-PhysiologicalData.csv", header=TRUE)


data<-data%>%mutate(Label = case_when(
  Label=="p1" ~ "cruising",
  Label=="p2" ~ "landing"
))

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

# Rosner's test voor uitbijters

library(EnvStats)
library(rstatix)
library(stats)

dat1 <- data$SCR_Peaks
hist(dat1, col='steelblue')
dev.new()
qqPlot(dat1)
rosnerTest(dat1, k = 10, alpha = 0.01)

dat2 <- data$HRV
hist(dat2, col='steelblue')
dev.new()
qqPlot(dat2)
rosnerTest(dat2, k = 10, alpha = 0.01)

datano <- data[-c(95), ]
qqPlot(datano$HRV)
rosnerTest(datano$HRV, k = 10, alpha = 0.01)

datarosner <- data[-c(12, 42, 44, 82, 84, 95), ]
qqPlot(datarosner$HRV)
rosnerTest(datarosner$HRV, k = 10, alpha = 0.01)

# ANOVA

library(ggpubr)
library(afex)

aov1 <- aov_ez(data = data, dv = "SCR_Peaks", id = "Subject",
       within = c("Helicopters", "Phase"))

summary(aov1)

aov2 <- aov_ez(data = data, dv = "HRV", id = "Subject",
       within = c("Helicopters", "Phase"))

summary(aov2)

aov2.1 <- aov_ez(data = datano, dv = "HRV", id = "Subject",
       within = c("Helicopters", "Phase"))

summary(aov2.1)

aov2.2 <- aov_ez(data = datarosner, dv = "HRV", id = "Subject",
       within = c("Helicopters", "Phase"))

summary(aov2.2)
