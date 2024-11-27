invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
################################################################################
# Get relevant data and variables like Accuracy, from raw data

library(reshape2)
library(tidyverse)

dataWide <- read_csv("Bachelor Psychologie/Jaar 3/Bachelorproject/R studio/R script/Analyses/AirTrafficControllers-ΒehaviouralData.csv")
dataWide <- dataWide[, c("Subject", "Session", "SessionTime", "EstimationSessionTime", "PJOT",
                         "MentalTLX", "PhysicalTLX", "TemporalTLX", "PerformanceTLX", "EffortTLX", "FrustrationTLX")]
dataWide$EstimationTime <- dataWide$EstimationSessionTime*60
dataWide$Estimation_Accuracy <- dataWide$EstimationTime/dataWide$SessionTime
dataWide$TLX <- (dataWide$MentalTLX+dataWide$PhysicalTLX+dataWide$TemporalTLX+dataWide$PerformanceTLX+dataWide$EffortTLX+dataWide$FrustrationTLX)/6


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
colnames(dataWide)[colnames(dataWide) == "PJOT"] ="Passage_Of_Time"
dataWide <- dataWide[, c("Subject", "Helicopters", "Language", "Estimation_Accuracy","Passage_Of_Time",
                         "MentalTLX", "PhysicalTLX", "TemporalTLX", "PerformanceTLX", "EffortTLX", "FrustrationTLX", "TLX")]


dataLong <- dataWide
dataLong <- dataLong[order(dataLong$Subject, dataLong$Helicopters, dataLong$Language), ]

###############################################################################

library(ggpubr)
library(rstatix)

# Assumptions
# Outliers
# Use the Rosner Test for small samples
library(EnvStats)

#Plot the QQ Plot to evaluate the number of possible outliers

dat <- dataLong$TLX
hist(dat, col='steelblue')
dev.new()
qqPlot(dat)
rosnerTest(dat, k = 5, alpha = 0.01)

# Normality check - I need p>0.05
dataLong %>%
  group_by(Helicopters, Language) %>%
  shapiro_test(TLX)

ggqqplot(dataLong, "TLX", ggtheme = theme_bw()) +
  facet_grid(Language ~ Helicopters, labeller = "label_both")


# ANOVA

res.aov <- anova_test(
  data = dataLong, dv = TLX, wid = Subject,
  within = c(Helicopters, Language)
)
get_anova_table(res.aov)


# Post-hocs

pwc1 <- dataLong %>%
  pairwise_t_test(
    TLX ~ Helicopters, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
pwc1


pwc2 <- dataLong %>%
  pairwise_t_test(
    TLX ~ Language, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
pwc2

bxp1 <- ggboxplot(
  dataLong, x = "Helicopters", y = "TLX",
  palette = "npg"
)
bxp1

bxp2 <- ggboxplot(
  dataLong, x = "Language", y = "TLX",
  palette = "npg"
)
bxp2

pwc1 <- pwc1 %>% add_xy_position(x = "Helicopters")
bxp1 + stat_pvalue_manual(pwc1, tip.length = 0, hide.ns = TRUE)


pwc2 <- pwc2 %>% add_xy_position(x = "Language")
bxp2 + stat_pvalue_manual(pwc2, tip.length = 0, hide.ns = TRUE)

dataLong %>%
  group_by(Helicopters) %>%
  get_summary_stats(TLX, type = "mean_sd")

dataLong %>%
  group_by(Language) %>%
  get_summary_stats(TLX, type = "mean_sd")

lmTLX <- lm( TLX ~ Helicopters + Language, data = dataLong)

summary(lmTLX)

coeff_TLX <- coef(lmTLX)[c(1,2)]

plot(dataLong$Helicopters, dataLong$TLX, pch = 16, col = "blue", 
     xlab = "Helicopters", ylab = "TLX") 

abline(a = coeff_TLX[1], b = coeff_TLX[2], col = "red", lwd = 2)

coeff_Lang <- coef(lmTLX)[c(1,3)]

plot(dataLong$Language, dataLong$TLX, pch = 16, col = "blue", 
     xlab = "Language", ylab = "TLX") 

abline(a = coeff_Lang[1], b = coeff_Lang[2], col = "red", lwd = 2)

################################################################################
library(Hmisc)

dataCorr <- dataLong[, c("Estimation_Accuracy", "Passage_Of_Time", "MentalTLX", "PhysicalTLX", "TemporalTLX", "PerformanceTLX", "EffortTLX", "FrustrationTLX","TLX")]
corrs2 <- rcorr(as.matrix(dataCorr), type="spearman")
corrs2$P

library(corrplot)
corrs1 <- cor(dataCorr, method = "spearman")
round(corrs1, 2)

# Positive correlations are displayed in blue and negative correlations in red color.
# Color intensity and the size of the circle are proportional to the correlation coefficients.
# In the right side of the correlogram, the legend color shows the correlation coefficients and the corresponding colors.


cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(dataCorr)
corrplot(corrs1, type="upper", 
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         mar=c(0,0,1,0),
         tl.col = "black", tl.srt = 45, diag=FALSE)
