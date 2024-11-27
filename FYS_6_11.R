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
colnames(data)[colnames(data) == "bpm"] ="BPM"
colnames(data)[colnames(data) == "ibi"] ="IBI"
colnames(data)[colnames(data) == "breathingrate"] ="BR"


data$Subject <- factor(data$Subject)
data$Helicopters <- factor(data$Helicopters)
data<- data[order(data$Subject, data$Helicopters, data$Language), ]

################################################################################
library(ggpubr)

bxp1 <- ggboxplot(
  data, x = "Helicopters", y = "SCR_Peaks",
  color = "Phase", palette = "npg", 
  facet.by = "Language", short.panel.labs = FALSE
)
bxp1


bxp2 <- ggboxplot(
  data, x = "Helicopters", y = "HRV",
  color = "Phase", palette = "npg", 
  facet.by = "Language", short.panel.labs = FALSE
)
bxp2


bxp3 <- ggboxplot(
  data, x = "Helicopters", y = "BPM",
  color = "Phase", palette = "npg", 
  facet.by = "Language", short.panel.labs = FALSE
)
bxp3


bxp4 <- ggboxplot(
  data, x = "Helicopters", y = "IBI",
  color = "Phase", palette = "npg", 
  facet.by = "Language", short.panel.labs = FALSE
)
bxp4


bxp5 <- ggboxplot(
  data, x = "Helicopters", y = "BR",
  color = "Phase", palette = "npg", 
  facet.by = "Language", short.panel.labs = FALSE
)
bxp5

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

q1BPM <- quantile(data$BPM, 0.25)
q3BPM <- quantile(data$BPM, 0.75)
iqrBPM <- IQR(data$BPM)

outliers_BPM <- data$BPM[data$BPM < q1BPM - 1.5*iqrBPM | data$BPM > q3BPM + 1.5*iqrBPM]
outliers_BPM

q1IBI <- quantile(data$IBI, 0.25)
q3IBI <- quantile(data$IBI, 0.75)
iqrIBI <- IQR(data$IBI)

outliers_IBI <- data$IBI[data$IBI < q1IBI - 1.5*iqrIBI | data$IBI > q3IBI + 1.5*iqrIBI]
outliers_IBI

q1BR <- quantile(data$BR, 0.25, na.rm = TRUE)
q3BR <- quantile(data$BR, 0.75, na.rm = TRUE)
iqrBR <- IQR(data$BR, na.rm = TRUE)

outliers_BR <- data$BR[data$BR < q1BR - 1.5*iqrBR | data$BR > q3BR + 1.5*iqrBR]
outliers_BR

# uitbijters uit de data halen - haal de outliers uit de data die hierboven weergegeven worden.

dataHRVno <- data[-c(6, 12, 43, 44, 83, 84, 94), ]
dataBPMno <- data[-c(34), ]
dataIBIno <- data[-c(34), ]
dataBRno <- data[-c(7), ]

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

res.aov3 <- anova_test(
  data = data, dv = BPM, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov3)

res.aov3.1 <- anova_test(
  data = dataBPMno, dv = BPM, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov3.1)

res.aov4 <- anova_test(
  data = data, dv = IBI, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov4)

res.aov4.1 <- anova_test(
  data = dataIBIno, dv = IBI, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov4.1)

res.aov5 <- anova_test(
  data = data, dv = BR, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov5)

res.aov5.1 <- anova_test(
  data = dataBRno, dv = BR, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov5.1)

bxp6 <- ggboxplot(
  dataBRno, x = "Helicopters", y = "BR",
  color = "Phase", palette = "npg", 
  facet.by = "Language", short.panel.labs = FALSE
)
bxp6
