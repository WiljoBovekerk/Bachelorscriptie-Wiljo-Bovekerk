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

# Rosner's test voor uitbijters

library(EnvStats)
library(rstatix)
library(stats)

dat1 <- data$SCR_Peaks
hist(dat1, col='steelblue')
dev.new()
qqPlot(dat1)
rosnerTest(dat1, k = 10, alpha = 0.01)

# Normality check - I need p>0.05
data %>%
  group_by(Helicopters, Language, Phase) %>%
  shapiro_test(SCR_Peaks)

ggqqplot(data, "SCR_Peaks", ggtheme = theme_bw()) +
  facet_grid(Language + Helicopters ~ Phase, labeller = "label_both")

dat2 <- data$HRV
hist(dat2, col='steelblue')
dev.new()
qqPlot(dat2)
rosnerTest(dat2, k = 10, alpha = 0.01)

datano <- data[-c(94), ]
datarosner <- data[-c(12, 43, 44, 83, 84, 94), ]

# ANOVA

library(nlme)

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

res.aov3 <- anova_test(
  data = datano, dv = HRV, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov3)

res.aov4 <- anova_test(
  data = datarosner, dv = HRV, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov4)
