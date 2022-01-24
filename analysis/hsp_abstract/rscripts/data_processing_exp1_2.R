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
# raw_data_path <- "../data/exp1c_raw.csv"
# data<-read.csv(raw_data_path)

d_s <- read.csv("../data/exp1c_cleaned.csv")
d_w <- read.csv("../data/exp2_cleaned.csv")
d <- rbind(d_s, d_w)

d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ","UNGRAM", "POLAR"))
d$phase <- factor(d$phase, levels = c("exposure", "test"))
d$phase2 <- factor(d$phase2, levels= c("pre-exposure", "exposure", "test"))


d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_ungram$item_type <- factor(d_no_ungram$item_type, levels = c("FILL", "POLAR", "SUBJ", "WH") )

d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("POLAR", "SUBJ", "WH"))


d_no_fillers$exposure_condition <- factor(d_no_fillers$exposure_condition, c("POLAR", "SUBJ", "WH"))


#############################
# Statistics
#############################

#model_block <- lmer(response~block_sequence*condition + (1+block_sequence*condition|workerid)+(1+condition|item_number), data = d)
#summary(model_block)

# model_global2 <- lmer(response~trial_sequence_total*item_type +
#                         (1+trial_sequence_total*item_type|workerid)+(1|item_number), data = d, verbose = 100)
#   summary(model_global2)
#   anova(model_global2)


# option 1: fully-crossed model
# data: all
# ref. levels: test phase
# run twice with ref levels as each control
# generalization: pos. exposure_condition effect wrt negative control, no
# significant effect wrt positive control
#
# What does the interaction term tell us?


# Subject test condition
##############################

d_subj <- subset(d_no_fillers, test_condition=="SUBJ")
d_subj <- subset(d, test_condition=="SUBJ")
# d_subj <- subset(d_subj, trial_sequence_total<=30)

# set reference level to between and polar
d_subj$phase <- factor(d_subj$phase, c("test", "exposure"))
d_subj$exposure_condition <- factor(d_subj$exposure_condition, c("WH", "POLAR", "SUBJ"))
# d_subj$exposure_condition <- factor(d_subj$exposure_condition, c("POLAR", "WH", "SUBJ"))
phase_model_subj1 <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_subj,
  # verbose = 100
)


summary(phase_model_subj1)
# save output to a txt file
sink(file="generalization_subj_neg.txt")
summary(phase_model_subj1)
sink(file=NULL)

library(mixedpower)
power_subj <- mixedpower(model = phase_model_subj1, data = d_subj,
                         fixed_effects = c("exposure_condition", "phase"),
                         simvar = "workerid", steps = c(720,780,840,900),
                         critical_value = 2, n_sim = 1000)

sink(file="power.txt")
power_subj
sink(file=NULL)

# set reference level to within-category
d_subj$phase <- factor(d_subj$phase, c("test", "exposure"))
d_subj$exposure_condition <- factor(d_subj$exposure_condition, c("SUBJ", "WH", "POLAR"))
phase_model_subj2 <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_subj,
  # verbose = 100
)

summary(phase_model_subj2)
# save output to a txt file
sink(file="generalization_subj_pos.txt")
summary(phase_model_subj2)
sink(file=NULL)


# whether test condition
##########################

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
# save output to a txt file
sink(file="generalization_wh_neg.txt")
summary(phase_model_wh1)
sink(file=NULL)

# set reference level to within-category
d_wh$phase <- factor(d_wh$phase, c("test", "exposure"))
d_wh$exposure_condition <- factor(d_wh$exposure_condition, c("WH", "SUBJ", "POLAR"))
phase_model_wh2 <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_wh,
  # verbose = 100
)

summary(phase_model_wh2)
# save output to a txt file
sink(file="generalization_wh_pos.txt")
summary(phase_model_wh2)
sink(file=NULL)




# option 2: simpler model
# data: only test phase
# run twice with ref levels as each control
# generalization: pos. main effect wrt negative control, no
# significant effect wrt positive control
# phase_model <- lmer(
#   response ~ exposure_condition +
#     (1 | workerid) +
#     (1 + exposure_condition | item_number), # with enough participants this works
#   data = d_no_fillers,
#   # verbose = 100
# )


# satiation models

d_exposure <- subset(d_no_ungram, phase == "exposure")

trial_model <- lmer(
  response ~ trial_sequence_total * item_type +
    (1 + trial_sequence_total*item_type | workerid) +
    (1 + trial_sequence_total*item_type | item_number),
  data = d_exposure,
  # verbose = 100
)

summary(trial_model)
# save output to a file
sink(file="satiation_model.txt")
summary(trial_model)
sink(file=NULL)


#############################
# plots and graphs
#############################

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

#cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")
cbPalette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#d <- read.csv("../data/exp1c_cleaned.csv")
d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ", "POLAR", "UNGRAM"))
d$phase <- factor(d$phase, levels = c("exposure", "test"))

d = subset(d, block_sequence != "practice")
#d <- subset(d, group == "mismatch")

d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_ungram$item_type <- factor(d_no_ungram$item_type, levels = c("FILL", "SUBJ","POLAR", "WH") )
d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("WH", "SUBJ", "POLAR"))

#calculate and plot trial/cumulative average
trial_avg <- aggregate(d[,"response"],list(d$trial_sequence_total), mean)
names(trial_avg)[names(trial_avg) == "Group.1"] <- "trial"
names(trial_avg)[names(trial_avg) == "x"] <- "avg"

#trial_average plot
trial_avg <- trial_avg[order(trial_avg$trial),]
cum <- cumsum(trial_avg$avg) / seq_along(trial_avg$avg)
trial_avg$cum <- cum

a= ggplot(trial_avg, aes(x=trial, y=avg)) +
  geom_smooth(method = lm, se = F) + geom_point()+
  xlab("Trial Sequence") +
  ylab("Average acceptability rating")+
  theme_bw()
a


#cum_average plot
b=ggplot(trial_avg, aes(x=trial, y=cum)) +

  geom_smooth (se = F) + geom_point()+
  xlab("Trial number") +
  ylab("Cumulative mean acceptability")+
  geom_hline(yintercept=0.5, linetype="dashed",
             size=1)+
  theme( plot.margin = margin(0, 0, 0, 0, "cm"))+
  geom_vline(xintercept=24.5, linetype="dashed",
             size=0.5)+
  annotate("label", x = 12, y = 0.48, label = "Exposure phase") +
  annotate("label", x = 34, y = 0.48, label = "Test phase") +
  theme_bw()
b
ggsave("../graphs/exp12_cum_avg.png",width=4,height=3)
ab <- ggarrange(a,b,
                labels = c("By-trial Average", "Cumulative Average"),
                ncol = 2, nrow = 1)




# bar chart

theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}


phase_avg = d_no_fillers %>%
  group_by(exposure_condition, test_condition, phase, group) %>%
  mutate(exposure_condition = fct_recode(exposure_condition,"Subject island"="SUBJ", "Whether island"="WH", "Polar question"="POLAR")) %>% 
  mutate(group = fct_recode(group,"Control"="control", "Between-category"="mismatch", "Within-category"="match")) %>% 
  mutate(group = fct_relevel(group, "Control", "Between-category", "Within-category")) %>%
  mutate(test_condition = fct_recode(test_condition,"Subject island (Exp. 1)"="SUBJ", "Whether island (Exp. 2)"="WH")) %>% 
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

# both combined
phase_avg = subset(phase_avg, phase == "test")
phase_graph<- ggplot(phase_avg, aes(x=test_condition,y=Mean, fill=group)) +
  geom_bar(data=phase_avg, stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=CILow,ymax=CIHigh), position=position_dodge2(width=0.2, padding=0.5)) +
  scale_fill_manual(name="Exposure group", values=cbPalette) +
  #scale_alpha_manual(values=c(0.6,1)) +
  xlab("Test condition") +
  ylab("Mean acceptability") +
  theme_bw() 

phase_graph

ggsave("../graphs/exp12_bars.png",width=5,height=2)
# 
# 
# phase_subj <- subset(phase_avg, test_condition=="subject island (EXP 1)")
# phase_wh  <- subset(phase_avg, test_condition=="whether island (EXP 2)")
# 
# ggplot(phase_subj, aes(x=group,y=Mean, fill=exposure_condition)) +
#   coord_cartesian(ylim=c(0,0.65))+
#   geom_bar(data=phase_subj, stat="identity") +
#   geom_errorbar(aes(ymin=CILow,ymax=CIHigh), width=0.4, position="dodge") +
#   scale_fill_manual(name="exposure condition", values=cbPalette) +
#   xlab("experiment group") +
#   ylab("average acceptability") +
#   theme_bw() 
# 
# ggsave("../graphs/exp12_bars_subj.pdf",width=10,height=5)
# 
# 
# ggplot(phase_wh, aes(x=group,y=Mean, fill=exposure_condition)) +
#   geom_bar(data=phase_wh, stat="identity") +
#   coord_cartesian(ylim=c(0,0.65))+
#   geom_errorbar(aes(ymin=CILow,ymax=CIHigh), width=0.4, position="dodge") +
#   scale_fill_manual(name="exposure condition", values=cbPalette) +
#   xlab("experiment group") +
#   ylab("average acceptability") +
#   theme_bw() 
# 
# ggsave("../graphs/exp12_bars_wh.pdf",width=10,height=5)


#by-subject Plot
# ggplot(d, aes(x=trial_sequence_total, y=response, color = item_type, shape = item_type)) +
#   geom_point() +
#   geom_smooth(method=lm, aes(fill=item_type))+facet_wrap(~workerid)
# ggsave("../graphs/subject_variability_pilot2.pdf", width=20, height = 25)
# ggsave("../graphs/subject_variability_pilot2.png", width=20, height = 25)
# 
# 

# overall plot
nd = d %>%
  mutate(item_type = fct_recode(item_type,"grammatical"="FILL","ungrammatical"="UNGRAM", "Subject island"="SUBJ", "Whether island"="WH", "Polar question"="POLAR")) %>% 
  #, "complex-NP island"="CNPC", "whether island"="WH")) %>%
  mutate(item_type = fct_relevel(item_type,"grammatical","Polar question","Subject island","Whether island","ungrammatical")) %>%
  mutate(exposure_condition = fct_recode(exposure_condition,"Subject island"="SUBJ", "Whether island"="WH", "Polar question"="POLAR"))  %>%
mutate(test_condition = fct_recode(test_condition,"Subject island"="SUBJ", "Whether island"="WH")) %>%
  mutate(group = fct_recode(group, "Control"="control", "Within-category"="match", "Between-category"="mismatch"))

trial_means = nd %>%
  group_by(exposure_condition,item_type, test_condition, phase,group,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()

trial_means$condition <- ""
trial_means$condition[trial_means$test_condition == "Whether island"] <- "Whether islands" 
trial_means$condition[trial_means$test_condition == "Subject island"] <- "Subject islands" 

trial_means$condition[trial_means$item_type == "grammatical" | trial_means$item_type=="ungrammatical"] <- "fillers"


trial_means = trial_means %>%
  group_by(test_condition, condition,phase, item_type, group, trial_sequence_total, exposure_condition) %>%
  summarize(response = mean(response)) %>%
  ungroup()


trial_means$condition <- factor(trial_means$condition, levels = c("fillers", "Whether islands", "Subject islands"))
trial_means <- subset(trial_means, condition != "fillers")

trial_means_s <- subset(trial_means, condition=="Subject islands")
trial_means <- subset(trial_means, condition=="Whether islands")


# cbPalette = c("#e69d00", "#009e74","#d55e00",  "#cc79a7", "#0071b2")

trial_means$item_type <- factor(trial_means$item_type, levels= c( "Whether island", "Subject island", "Polar question"))
# color as exposure condition
overall_w <- ggplot(nd, aes(x=trial_sequence_total, y=response, color = exposure_condition, fill=exposure_condition, linetype=item_type, shape=item_type)) +
  geom_point(data=trial_means,alpha=.9) +
  xlab("Trial number") +
  ylab("Mean acceptability")+
  # geom_smooth(data=subset(trial_means, item_type == "grammatical" ),method=lm) +
  # geom_smooth(data=subset(trial_means, item_type == "ungrammatical"),method=lm) +
  geom_smooth(data=subset(trial_means, condition != "fillers" & phase == "exposure"), method=lm) +
  geom_smooth(data=subset(trial_means, condition != "fillers" & phase == "test"), method=lm) +
  
  geom_vline(xintercept=24.5, linetype="dashed",
             size=0.5)+
  scale_color_manual(name="Exposure group", labels=c("Control",  "Between-category", "Within-category"), values=cbPalette) +
  scale_fill_manual(name="Exposure group", labels=c("Control", "Between-category", "Within-category"), values=cbPalette) +
  scale_shape(name="Sentence type") +
  scale_linetype(name="Sentence type") +
  annotate("label", x = 12, y = 0.8, label = "Exposure phase") +
  annotate("label", x = 34, y = 0.8, label = "Test phase\n(Whether)") +
  theme_bw()

ggsave("../graphs/exp12_overall_wh.png",width=6,height=4)



trial_means_s$item_type <- factor(trial_means_s$item_type, levels= c( "Whether island", "Subject island", "Polar question"))
trial_means_s$exposure_condition <- factor(trial_means_s$exposure_condition, levels=c("Polar question", "Whether island", "Subject island" ))
# color as exposure condition
overall_s <- ggplot(nd, aes(x=trial_sequence_total, y=response, color = exposure_condition, fill=exposure_condition, linetype=item_type, shape=item_type)) +
  geom_point(data=trial_means_s,alpha=.9) +
  xlab("Trial number") +
  ylab("Mean acceptability")+

  geom_smooth(data=subset(trial_means_s, condition != "fillers" & phase == "exposure"), method=lm) +
  geom_smooth(data=subset(trial_means_s, condition != "fillers" & phase == "test"), method=lm) +
  
  geom_vline(xintercept=24.5, linetype="dashed",
             size=0.5)+
  scale_color_manual(name="Exposure group", labels=c("Control",  "Between-category", "Within-category"), values=cbPalette) +
  scale_fill_manual(name="Exposure group", labels=c("Control", "Between-category", "Within-category"), values=cbPalette) +
  scale_shape(name="Sentence type") +
  scale_linetype(name="Sentence type") +
  annotate("label", x = 12, y = 0.8, label = "Exposure phase") +
  annotate("label", x = 34, y = 0.8, label = "Test phase\n(Subject)") +
  theme_bw()

ggsave("../graphs/exp12_overall_subj.png",width=6,height=4)

ov <- ggarrange(overall_s,overall_w,
                ncol = 1, nrow = 2)
ov

ggsave("../graphs/exp12_overall_combined.png",width=5,height=6)

# # color as sentence type
# ggplot(nd, aes(x=trial_sequence_total, y=response, color = item_type, fill=item_type, linetype=exposure_condition, shape=item_type)) +
#   geom_point(data=trial_means,alpha=.9) +
#   xlab("trial sequence") +
#   ylab("average acceptability")+
#   # geom_smooth(data=subset(trial_means, item_type == "grammatical" ),method=lm) +
#   # geom_smooth(data=subset(trial_means, item_type == "ungrammatical"),method=lm) +
#   geom_smooth(data=subset(trial_means, condition != "fillers" & phase == "exposure"), method=lm) +
#   geom_smooth(data=subset(trial_means, condition != "fillers" & phase == "test"), method=lm) +
#   
#   geom_vline(xintercept=24.5, linetype="dashed",
#              size=0.5)+
#   scale_color_manual(name="sentence type", values=cbPalette) +
#   scale_fill_manual(name="sentence type", values=cbPalette) +
#   scale_shape(name="sentence type") +
#   scale_linetype(name="exposure condition") +
#   annotate("label", x = 12, y = 0.8, label = "exposure phase") +
#   annotate("label", x = 34, y = 0.8, label = "test phase") +
#   theme_bw()
# 
# ggsave("../graphs/exp12_overall_subj_b.pdf",width=10,height=5)


