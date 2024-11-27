invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
################################################################################
# Get relevant data and variables like Accuracy, from behavioral data

library(reshape2)
library(tidyverse)
library(readxl)

dataWide <- read_excel("Bachelor Psychologie/Jaar 3/Bachelorproject/R studio/R script/Analyses/Exc_Beh_Fys.xlsx")
dataWide <- dataWide[, c("Subject", "P1SCR", "P2SCR", "P1HRV", "P2HRV", "P1Asked", "P1Produced", "P2Asked", "P2Produced", "Session", "SessionTime", "EstimationSessionTime",
                         "MentalTLX", "PhysicalTLX", "TemporalTLX", "PerformanceTLX", "EffortTLX", "FrustrationTLX")]
dataWide$EstimationTime <- dataWide$EstimationSessionTime*60
dataWide$Estimation_Accuracy <- dataWide$EstimationTime-dataWide$SessionTime
dataWide$TLX <- (dataWide$MentalTLX+dataWide$PhysicalTLX+dataWide$TemporalTLX+dataWide$PerformanceTLX+dataWide$EffortTLX+dataWide$FrustrationTLX)/6

dataWide$Subject <- factor(dataWide$Subject)

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

dataWide$Language <- factor(dataWide$Language, levels = c("Greek", "English"))
cols <- c("Subject", "Helicopters")
dataWide[cols] <- lapply(dataWide[cols], factor)
colnames(dataWide)[colnames(dataWide) == "PJOT"] ="Passage_Of_Time"

dataLong <- reshape(dataWide, direction="long", 
                    varying=c("P1Asked", "P1SCR", "P1HRV", "P1Produced", "P2Asked", "P2SCR", "P2HRV", "P2Produced"),
                    timevar="Phase",
                    times=c("cruising", "landing"),
                    v.names=c("Asked", "Produced", "SCR", "HRV"),
                    idvar=c("Subject", "Session"))

dataLong$Production_Accuracy <- dataLong$Produced/dataLong$Asked
colnames(dataLong)[colnames(dataLong) == "Session"] ="Sessiontime"

dataLong$Language <- factor(dataLong$Language, levels = c("Greek", "English"))
cols <- c("Subject", "Helicopters", "Phase")
dataLong[cols] <- lapply(dataLong[cols], factor)

dataLong <- dataLong[order(dataLong$Subject, dataLong$Helicopters, dataLong$Language, dataLong$Phase), ]

################################################################
library(ggpubr)
library(rstatix)

# Outliers vinden via de IQR regel

# production_accuracy

q1Prod <- quantile(dataLong$Production_Accuracy, 0.25)
q3Prod <- quantile(dataLong$Production_Accuracy, 0.75)
iqrProd <- IQR(dataLong$Production_Accuracy)

outliers_Prod <- dataLong$Production_Accuracy[dataLong$Production_Accuracy < q1Prod - 1.5*iqrProd | dataLong$Production_Accuracy > q3Prod + 1.5*iqrProd]
outliers_Prod

# estimation_accuracy

q1Est <- quantile(dataLong$Estimation_Accuracy, 0.25)
q3Est <- quantile(dataLong$Estimation_Accuracy, 0.75)
iqrEst <- IQR(dataLong$Estimation_Accuracy)

outliers_Est <- dataLong$Estimation_Accuracy[dataLong$Estimation_Accuracy < q1Est - 1.5*iqrEst | dataLong$Estimation_Accuracy > q3Est + 1.5*iqrEst]
outliers_Est

# TLX

q1TLX <- quantile(dataLong$TLX, 0.25)
q3TLX <- quantile(dataLong$TLX, 0.75)
iqrTLX <- IQR(dataLong$TLX)

outliers_TLX <- dataLong$TLX[dataLong$TLX < q1TLX - 1.5*iqrTLX | dataLong$TLX > q3TLX + 1.5*iqrTLX]
outliers_TLX

# SCR

q1SCR <- quantile(dataLong$SCR, 0.25)
q3SCR <- quantile(dataLong$SCR, 0.75)
iqrSCR <- IQR(dataLong$SCR)

outliers_SCR <- dataLong$SCR[dataLong$SCR < q1SCR - 1.5*iqrSCR | dataLong$SCR > q3SCR + 1.5*iqrSCR]
outliers_SCR

# HRV

q1HRV <- quantile(dataLong$HRV, 0.25)
q3HRV <- quantile(dataLong$HRV, 0.75)
iqrHRV <- IQR(dataLong$HRV)

outliers_HRV <- dataLong$HRV[dataLong$HRV < q1HRV - 1.5*iqrHRV | dataLong$HRV > q3HRV + 1.5*iqrHRV]
outliers_HRV

dataHRVno <- dataLong[-c(6, 12, 43, 44, 83, 84, 94), ]

# Multivariate ANOVA

library(nlme)

MANOVA2 <- lm( cbind(Production_Accuracy, Estimation_Accuracy, TLX, SCR, HRV) ~ Helicopters * Phase, data = dataLong)

anova(MANOVA2)
summary(MANOVA2)

MANOVA2no <- lm( cbind(Production_Accuracy, Estimation_Accuracy, TLX, SCR, HRV) ~ Helicopters * Phase, data = dataHRVno)

Manova(MANOVA2no, test.statistic = "Pillai")
Manova(MANOVA2no, test.statistic = "Wilks")
Manova(MANOVA2no, test.statistic = "Hotelling-Lawley")
Manova(MANOVA2no, test.statistic = "Roy")

summary(MANOVA2no)

library(emmeans)

emmeans(MANOVA2, list(pairwise ~ Helicopters * Phase), adjust="bonferroni")

################################################################################

# PW voor production accuracy

pwc1.1 <- dataLong %>%
  group_by(Phase) %>%
  pairwise_t_test(
    Production_Accuracy ~ Helicopters, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1.1

pwc1.2 <- dataLong %>%
  group_by(Helicopters) %>%
  pairwise_t_test(
    Production_Accuracy ~ Phase, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1.2


# pw voor Estimation Accuracy

dataLong %>%
  pairwise_t_test(
    Estimation_Accuracy ~ Helicopters, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

# pw voor TLX

dataLong %>%
  pairwise_t_test(
    TLX ~ Helicopters, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
