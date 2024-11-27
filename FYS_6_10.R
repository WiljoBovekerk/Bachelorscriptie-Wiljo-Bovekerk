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
colnames(data)[colnames(data) == "bpm"] ="BPM"
colnames(data)[colnames(data) == "ibi"] ="IBI"
colnames(data)[colnames(data) == "breathingrate"] ="BR"


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


bxp3 <- ggboxplot(
  data, x = "Helicopters", y = "BPM",
  color = "Phase", palette = "npg", short.panel.labs = FALSE
)
bxp3


bxp4 <- ggboxplot(
  data, x = "Helicopters", y = "IBI",
  color = "Phase", palette = "npg", short.panel.labs = FALSE
)
bxp4


bxp5 <- ggboxplot(
  data, x = "Helicopters", y = "BR",
  color = "Phase", palette = "npg", short.panel.labs = FALSE
)
bxp5

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

# dataframes zonder uitbijters voor HRV

dataHRVno <- data[-c(95), ]
dataHRVrosner <- data[-c(12, 42, 44, 82, 84, 95), ]

dat3 <- data$BPM
hist(dat3, col='steelblue')
dev.new()
qqPlot(dat3)
rosnerTest(dat3, k = 10, alpha = 0.01)

dataBPMno <- data[-c(35), ]

dat4 <- data$IBI
hist(dat4, col='steelblue')
dev.new()
qqPlot(dat4)
rosnerTest(dat4, k = 10, alpha = 0.01)

dataIBIno <- data[-c(35), ]

dat5 <- data$BR
hist(dat5, col='steelblue')
dev.new()
qqPlot(dat5)
rosnerTest(dat5, k = 10, alpha = 0.01)

# ANOVA

library(afex)

aov1 <- aov_ez(data = data, dv = "SCR_Peaks", id = "Subject",
               within = c("Helicopters", "Phase"))

summary(aov1)

aov2 <- aov_ez(data = data, dv = "HRV", id = "Subject",
               within = c("Helicopters", "Phase"))

summary(aov2)

aov2.1 <- aov_ez(data = dataHRVno, dv = "HRV", id = "Subject",
               within = c("Helicopters", "Phase"))

summary(aov2.1)

aov2.2 <- aov_ez(data = dataHRVrosner, dv = "HRV", id = "Subject",
               within = c("Helicopters", "Phase"))

summary(aov2.2)

aov3 <- aov_ez(data = data, dv = "BPM", id = "Subject",
               within = c("Helicopters", "Phase"))

summary(aov3)

aov3.1 <- aov_ez(data = dataBPMno, dv = "BPM", id = "Subject",
               within = c("Helicopters", "Phase"))

summary(aov3.1)

aov4 <- aov_ez(data = data, dv = "IBI", id = "Subject",
               within = c("Helicopters", "Phase"))

summary(aov4)

aov4.1 <- aov_ez(data = dataIBIno, dv = "IBI", id = "Subject",
               within = c("Helicopters", "Phase"))

summary(aov4.1)

aov5 <- aov_ez(data = data, dv = "BR", id = "Subject",
               within = c("Helicopters", "Phase"))

summary(aov5)