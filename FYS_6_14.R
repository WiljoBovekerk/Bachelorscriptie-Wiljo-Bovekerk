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

# Checken voor uitbijters met residual statistics

library(nlme)
library(car)

Model.eo1 <- lm(SCR_Peaks ~ Helicopters*Phase, data = data)

influenceIndexPlot(Model.eo1)

Model.eo2 <- lm(HRV ~ Helicopters*Phase, data = data)

influenceIndexPlot(Model.eo2)
outlierTest(Model.eo2)

dataHRVno <- data[-c(95), ]

Model.eo3 <- lm(BPM ~ Helicopters*Phase, data = data)

influenceIndexPlot(Model.eo3)
outlierTest(Model.eo3)

dataBPMno <- data[-c(34), ]

Model.eo4 <- lm(IBI ~ Helicopters*Phase, data = data)

influenceIndexPlot(Model.eo4)
outlierTest(Model.eo4)

dataIBIno <- data[-c(34), ]

Model.eo5 <- lm(BR ~ Helicopters*Phase, data = data)

influenceIndexPlot(Model.eo5)
outlierTest(Model.eo5)


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