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
d$phase2 <- factor(d$phase2, levels= c("pre-exposure", "exposure", "test"))


d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_ungram$item_type <- factor(d_no_ungram$item_type, levels = c("FILL", "POLAR", "SUBJ", "WH") )

d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("POLAR", "SUBJ", "WH"))

d_no_fillers$exposure_condition <- factor(d_no_fillers$exposure_condition, c("POLAR", "SUBJ", "WH"))

# Subject test condition
##############################

d_subj <- subset(d_no_fillers, test_condition=="SUBJ")

# set reference level to test and polar
d_subj$phase <- factor(d_subj$phase, c("test", "exposure"))
d_subj$exposure_condition <- factor(d_subj$exposure_condition, c("WH", "SUBJ", "POLAR"))

phase_model_subj_bayes <- brm(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_subj,
)

summary(phase_model_subj_bayes)
# save output to a txt file
sink(file="bayes_subj_neg.txt")
summary(phase_model_subj_bayes)
sink(file=NULL)


# whether test condition
##########################

d_wh <- subset(d_no_fillers, test_condition=="WH")

# set reference level to test and polar
d_wh$phase <- factor(d_wh$phase, c("test", "exposure"))
d_wh$exposure_condition <- factor(d_wh$exposure_condition, c("POLAR", "SUBJ", "WH"))

phase_model_wh1 <- brm(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_wh,
)

summary(phase_model_wh1)
# save output to a txt file
sink(file="bayes_wh_neg.txt")
summary(phase_model_wh1)
sink(file=NULL)

# set reference level to within-category
d_wh$phase <- factor(d_wh$phase, c("test", "exposure"))
d_wh$exposure_condition <- factor(d_wh$exposure_condition, c("WH", "SUBJ", "POLAR"))
phase_model_wh2 <- brm(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_wh,
  # verbose = 100
)

summary(phase_model_wh2)
# save output to a txt file
sink(file="bayes_wh_pos.txt")
summary(phase_model_wh2)
sink(file=NULL)