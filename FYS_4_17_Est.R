invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
################################################################################
# Get relevant data and variables like Accuracy, from raw data

library(reshape2)
library(tidyverse)

dataWide <- read.csv("AirTrafficControllers-ΒehaviouralData.csv", header=TRUE)
dataWide <- dataWide[, c("Subject", "Session", "SessionTime", "EstimationSessionTime")]
dataWide$EstimationTime <- dataWide$EstimationSessionTime*60
dataWide$Estimation_Accuracy <- dataWide$EstimationTime-dataWide$SessionTime


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
dataWide <- dataWide[, c("Subject", "Helicopters", "Language", "Estimation_Accuracy")]


dataLong <- dataWide
dataLong <- dataLong[order(dataLong$Subject, dataLong$Helicopters, dataLong$Language), ]

###############################################################################
#Do this before turning variables to factors

library(WRS)

Lyster2=fac2list(dataLong[,4], dataLong[,c(2, 3)])

wwtrim(2, 2, Lyster2, tr=.01)



#################################################################################


library(tidyverse)
library(ggpubr)
library(rstatix)

# Do the statistical analysis


# Outliers vinden via de IQR regel

q1 <- quantile(dataLong$Estimation_Accuracy, 0.25)
q3 <- quantile(dataLong$Estimation_Accuracy, 0.75)
iqr <- IQR(dataLong$Estimation_Accuracy)

outliers <- dataLong$Estimation_Accuracy[dataLong$Estimation_Accuracy < q1 - 1.5*iqr | dataLong$Estimation_Accuracy > q3 + 1.5*iqr]
outliers

# ANOVA
library(rstatix)

library(nlme)

res.aov1 <- lme(
  Estimation_Accuracy ~ Helicopters,          # Fixed effects for main effects and interaction
  random = ~ 1 | Subject,                    # Random intercepts for each Subject
  data = dataLong
)
summary(res.aov1)


# Find the mean Accuracy for the Helicopters*Phase, Helicopters, Phase groups

dataLong %>%
  group_by(Helicopters) %>%
  get_summary_stats(Estimation_Accuracy, type = "mean_sd")

# Post-hocs

pwc <- dataLong %>%
  pairwise_t_test(
    Estimation_Accuracy ~ Helicopters, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
pwc


pwc <- pwc %>% add_xy_position(x = "Helicopters")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    caption = get_pwc_label(pwc)
  )


################################################################################

