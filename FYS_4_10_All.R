invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
################################################################################
# Get relevant data and variables like Accuracy, from behavioral data

library(reshape2)
library(tidyverse)
library(readxl)

dataWide <- read_excel("Exc_Beh_Fys.xlsx")
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

dataLong <- reshape(dataWide, direction="long", 
                    varying=c("P1Asked", "P1SCR", "P1HRV", "P1Produced", "P2Asked", "P2SCR", "P2HRV", "P2Produced"),
                    timevar="Phase",
                    times=c("cruising", "landing"),
                    v.names=c("Asked", "Produced", "SCR", "HRV"),
                    idvar=c("Subject", "Session"))

dataLong$Production_Accuracy <- dataLong$Produced-dataLong$Asked
colnames(dataLong)[colnames(dataLong) == "Session"] ="Sessiontime"

dataLong$Language <- factor(dataLong$Language, levels = c("Greek", "English"))
cols <- c("Subject", "Helicopters", "Phase")
dataLong[cols] <- lapply(dataLong[cols], factor)

dataLong <- dataLong[order(dataLong$Subject, dataLong$Helicopters, dataLong$Language, dataLong$Phase), ]

################################################################

library(ggpubr)
library(rstatix)

# Assumptions
# Outliers
# Use the Rosner Test for small samples
library(EnvStats)
library(base)

#Plot the QQ Plot to evaluate the number of possible outliers

dat1 <- dataLong$Production_Accuracy
hist(dat1, col='steelblue')
dev.new()
qqPlot(dat1)
rosnerTest(dat1, k = 10, alpha = 0.01)

dat2 <- dataLong$Estimation_Accuracy
hist(dat2, col='steelblue')
dev.new()
qqPlot(dat2)
rosnerTest(dat2, k = 10, alpha = 0.01)

dat3 <- dataLong$TLX
hist(dat3, col='steelblue')
dev.new()
qqPlot(dat3)
rosnerTest(dat3, k = 10, alpha = 0.01)

dat4 <- dataLong$SCR
hist(dat4, col='steelblue')
dev.new()
qqPlot(dat4)
rosnerTest(dat4, k = 10, alpha = 0.01)

dat5 <- dataLong$HRV
hist(dat5, col='steelblue')
dev.new()
qqPlot(dat5)
rosnerTest(dat5, k = 10, alpha = 0.01)

dataHRVno <- dataLong[-c(94), ]
dataHRVrosner <- dataLong[-c(12, 43, 44, 84, 84, 94), ]

# Multivariate ANOVA

library(nlme)

MANOVA2 <- lm( cbind(Production_Accuracy, Estimation_Accuracy, TLX, SCR, HRV) ~ Helicopters * Language * Phase, data = dataLong)

anova(MANOVA2)
summary(MANOVA2)

MANOVA2.1 <- lm( cbind(Production_Accuracy, Estimation_Accuracy, TLX, SCR, HRV) ~ Helicopters * Language * Phase, data = dataHRVno)

anova(MANOVA2.1)
summary(MANOVA2.1)

MANOVA2.2 <- lm( cbind(Production_Accuracy, Estimation_Accuracy, TLX, SCR, HRV) ~ Helicopters * Language * Phase, data = dataHRVrosner)

Manova(MANOVA2.2, test.statistic = "Pillai")
Manova(MANOVA2.2, test.statistic = "Wilks")
Manova(MANOVA2.2, test.statistic = "Hotelling-Lawley")
Manova(MANOVA2.2, test.statistic = "Roy")

summary(MANOVA2.2)

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

