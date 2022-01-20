this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(simr)
library(lmerTest)
library(brms)
library(bootstrap)
library(ggpubr)
`%notin%` <- Negate(`%in%`)


d <- read.csv("../data/exp1b_cleaned.csv")
d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ","UNGRAM", "POLAR"))
d$phase <- factor(d$phase, levels = c("exposure", "test"))

d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_ungram$item_type <- factor(d_no_ungram$item_type, levels = c("FILL", "POLAR", "SUBJ", "WH") )
d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("POLAR", "SUBJ", "WH"))
d_no_fillers$exposure_condition <- factor(d_no_fillers$exposure_condition, c("POLAR", "SUBJ", "WH"))


d_subj <- subset(d_no_fillers, test_condition=="SUBJ")

# set reference level to test and polar
d_subj$phase <- factor(d_subj$phase, c("test", "exposure"))
d_subj$exposure_condition <- factor(d_subj$exposure_condition, c("POLAR", "SUBJ", "WH"))

phase_model_subj1 <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_subj,
  # verbose = 100
)

summary(phase_model_subj1)


# fixef(phase_model_subj1)["exposure_conditionWH"] #<- 0.058
# fixef(phase_model_subj1)["exposure_conditionSUBJ"]
powerSim(phase_model_subj1, nsim=100, test = fcompare(response~phase))
model_ext_subj <- extend(phase_model_subj1, within="phase+exposure_condition", n=720)


p_curve_treat <- powerCurve(model_ext_subj, test=fcompare(response~phase), within="phase+exposure_condition", breaks=c(360,480,600,720))




d_wh <- subset(d_no_fillers, test_condition=="WH")

# set reference level to test and polar
d_wh$phase <- factor(d_wh$phase, c("test", "exposure"))
d_wh$exposure_condition <- factor(d_wh$exposure_condition, c("POLAR", "SUBJ", "WH"))

phase_model_wh1 <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_wh,
  # verbose = 100
)

summary(phase_model_wh1)

powerSim(phase_model_wh1, nsim=100, test = fcompare(response~phase))
model_ext_wh <- extend(phase_model_wh1, within="phase+exposure_condition", n=720)


p_curve_treat <- powerCurve(model_ext_wh, test=fcompare(response~phase), within="phase+exposure_condition", breaks=c(360,480,600,720))

sink(file="pcurve.txt")
p_curve_treat