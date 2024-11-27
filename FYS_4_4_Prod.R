invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
################################################################################
# Get relevant data and variables like Accuracy, from raw data

library(reshape2)
library(tidyverse)

dataWide <- read.csv("AirTrafficControllers-ΒehaviouralData.csv", header=TRUE)
dataWide <- dataWide[, c("Subject", "Session", "P1Asked", "P1Produced", "P2Asked", "P2Produced")]

dataWide$Subject <- factor(dataWide$Subject)

dataLong <- reshape(dataWide, direction="long", 
                    varying=c("P1Asked", "P1Produced", "P2Asked", "P2Produced"), 
                    timevar="Phase",
                    times=c("cruising", "landing"),
                    v.names=c("Asked", "Produced"),
                    idvar=c("Subject", "Session"))

dataLong$Production_Accuracy <- dataLong$Produced-dataLong$Asked
colnames(dataLong)[colnames(dataLong) == "Session"] ="SessionTemp"

dataLong<-dataLong%>%mutate(Session = case_when(
  SessionTemp=="ee1" ~ "1H-Gr",
  SessionTemp=="ee2" ~ "1H-Eng",
  SessionTemp=="ee3" ~ "2H-Gr",
  SessionTemp=="ee4" ~ "2H-Eng"
))

dataLong<-dataLong%>%mutate(Helicopters = case_when(
  SessionTemp=="ee1" ~ "one",
  SessionTemp=="ee2" ~ "one",
  SessionTemp=="ee3" ~ "two",
  SessionTemp=="ee4" ~ "two"
))

dataLong<-dataLong%>%mutate(Language = case_when(
  SessionTemp=="ee1" ~ "Greek",
  SessionTemp=="ee2" ~ "English",
  SessionTemp=="ee3" ~ "Greek",
  SessionTemp=="ee4" ~ "English"
))

dataLong <- dataLong[, c("Subject", "Session", "Helicopters", "Language", "Phase",  "Asked", "Produced", "Production_Accuracy")]
dataLong$Language <- factor(dataLong$Language, levels = c("Greek", "English"))
cols <- c("Subject", "Helicopters", "Phase")
dataLong[cols] <- lapply(dataLong[cols], factor)

dataLong <- dataLong[order(dataLong$Subject, dataLong$Helicopters, dataLong$Language, dataLong$Phase), ]

################################################################################
library(ggpubr)
library(rstatix)

# Summary stats
dataLong %>%
  group_by(Helicopters, Language, Phase) %>%
  get_summary_stats(Production_Accuracy, type = "mean_sd")


ggline.interaction <- ggline(dataLong, x = "Helicopters", y = "Production_Accuracy", color = "Phase",
                  short.panel.labs = FALSE,
                  add = "mean_se",
                  palette = "npg") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 1, linetype = 2) +
  ylab("Production Accuracy")
ggline.interaction


# Assumptions
# Outliers
# Use the Rosner Test for small samples
library(EnvStats)

#Plot the QQ Plot to evaluate the number of possible outliers

dat <- dataLong$Production_Accuracy
hist(dat, col='steelblue')
dev.new()
qqPlot(dat)
rosnerTest(dat, k = 10, alpha = 0.01)

# Normality check - I need p>0.05
dataLong %>%
  group_by(Helicopters, Language, Phase) %>%
  shapiro_test(Production_Accuracy)

ggqqplot(dataLong, "Production_Accuracy", ggtheme = theme_bw()) +
  facet_grid(Language + Helicopters ~ Phase, labeller = "label_both")

# ANOVA

res.aov <- anova_test(
  data = dataLong, dv = Production_Accuracy, wid = Subject,
  within = c(Helicopters, Language, Phase)
)
get_anova_table(res.aov)

################################################################################

# Pairwise comparisons between Helicopters groups

dataLong %>%
  group_by(Helicopters, Phase) %>%
  get_summary_stats(Production_Accuracy, type = "mean_sd")

dataLong %>%
  group_by(Phase) %>%
  get_summary_stats(Production_Accuracy, type = "mean_sd")

dataLong %>%
  group_by(Helicopters) %>%
  get_summary_stats(Production_Accuracy, type = "mean_sd")



pwc1 <- dataLong %>%
  group_by(Phase) %>%
  pairwise_t_test(
    Production_Accuracy ~ Helicopters, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1

# Pairwise comparisons between Phase points
pwc2 <- dataLong %>%
  group_by(Helicopters) %>%
  pairwise_t_test(
    Production_Accuracy ~ Phase, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2


bxp1 <- ggboxplot(
  dataLong, x = "Phase", y = "Production_Accuracy",
  color = "Helicopters", palette = "npg"
)
bxp1

bxp2 <- ggboxplot(
  dataLong, x = "Helicopters", y = "Production_Accuracy",
  color = "Phase", palette = "npg"
)
bxp2

# comparisons for Helicopters variable
dataLong %>%
  pairwise_t_test(
    Production_Accuracy ~ Helicopters, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
# comparisons for Phase variable
dataLong %>%
  pairwise_t_test(
    Production_Accuracy ~ Phase, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

# Visualization: box plots with p-values
pwc1 <- pwc1 %>% add_xy_position(x = "Phase")
bxp1 + stat_pvalue_manual(pwc1, tip.length = 0, hide.ns = TRUE)


pwc2 <- pwc2 %>% add_xy_position(x = "Helicopters")
bxp2 + stat_pvalue_manual(pwc2, tip.length = 0, hide.ns = TRUE)


