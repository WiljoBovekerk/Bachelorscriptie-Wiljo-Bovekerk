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

res1 <-cor.test(data$Accuracy, data$SCR_Peaks,  method = "spearman", exact=FALSE)
res1
