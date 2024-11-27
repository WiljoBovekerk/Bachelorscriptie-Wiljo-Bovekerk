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
data<- data[order(data$Subject, data$Helicopters), ]

################################################################################
library(ggpubr)

bxp1 <- ggboxplot(
  data, x = "Helicopters", y = "SCR_Peaks",
  color = "Phase", palette = "npg", 
  short.panel.labs = FALSE
)
bxp1


bxp2 <- ggboxplot(
  data, x = "Helicopters", y = "HRV",
  color = "Phase", palette = "npg", 
  short.panel.labs = FALSE
)
bxp2

# Outliers vinden via de IQR regel

q1SCR <- quantile(data$SCR, 0.25)
q3SCR <- quantile(data$SCR, 0.75)
iqrSCR <- IQR(data$SCR)

outliers_SCR <- data$SCr[data$SCR < q1SCR - 1.5*iqrSCR | data$SCR > q3SCR + 1.5*iqrSCR]
outliers_SCR

q1HRV <- quantile(data$HRV, 0.25)
q3HRV <- quantile(data$HRV, 0.75)
iqrHRV <- IQR(data$HRV)

outliers_HRV <- data$HRV[data$HRV < q1HRV - 1.5*iqrHRV | data$HRV > q3HRV + 1.5*iqrHRV]
outliers_HRV

# uitbijters uit de data halen - haal de outliers uit de data die hierboven weergegeven worden.

dataHRVno <- data[-c(6, 12, 43, 44, 83, 84, 94), ]

# ANOVA

library(ggpubr)
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
