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
raw_data_path <- "../data/exp1b_raw.csv"
data<-read.csv(raw_data_path)

# remove subject information from data
data <- data %>% select(-(catch_trials:system.screenW)) 

mean(data$time_in_minutes)

ggplot(data, aes(x=time_in_minutes)) +
  geom_histogram(binwidth=1)
theme_bw()


head(data)

# create pseudo-pre-exposure phase
data$phase2 <- data$phase
data$phase2 <- factor(data$phase2, levels = c("pre-exposure", "exposure", "test"))
data$phase2[as.integer(data$trial_sequence_total) <= 6 & data$block_sequence != "practice"] <- "pre-exposure"

#############################
# Step 1: Filter out the participants who responded incorrectly more than once to the practice questions:
#############################

practice_data=subset(data,block_sequence == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
data=subset(data, is.element(workerid, practice_good_data$workerid))


#############################
# Step 2: filter: no overlap of 95%CI of FILL and UNGRAM
#############################

filler_data = subset(data, item_type == "FILL")
ungram_data = subset(data, item_type == "UNGRAM")

theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}

filler_by_subject = aggregate(filler_data[,"response"],list(filler_data$workerid), ci.low)
ungram_by_subject = aggregate(ungram_data[,"response"],list(ungram_data$workerid), ci.high)

names(filler_by_subject)[names(filler_by_subject) == "Group.1"] <- "subject"
names(filler_by_subject)[names(filler_by_subject) == "x"] <- "fill_ci_low"

names(ungram_by_subject)[names(ungram_by_subject) == "Group.1"] <- "subject"
names(ungram_by_subject)[names(ungram_by_subject) == "x"] <- "ungram_ci_high"

all_filler <- merge(ungram_by_subject, filler_by_subject, by.x="subject")

eligible_subjects = c()
for (i in (1:length(all_filler$subject))){
  row = all_filler[i,]
  if (row$ungram_ci_high < row$fill_ci_low){
    eligible_subjects <- c(eligible_subjects, row$subject)
  }
}
data = subset(data, workerid %in% eligible_subjects)

#############################
# Step 3: exclude non-English speakers
#############################

non_Eng <- c()

data = subset(data, workerid %notin% non_Eng)


# how many excluded participants?
n_participants <- 12 
length(unique(data$workerid))
exclusion_rate <- (n_participants - length(unique(data$workerid)))/n_participants





#############################
# save cleaned data
#############################

#Clean practice trials and control trials.
data = subset(data, block_sequence != "practice")
data$item_type <- factor(data$item_type, levels = c("FILL", "UNGRAM","SUBJ","WH", "POLAR"))
#data = subset(data, condition != "UNGRAM")
#data = subset(data, condition != "FILL")
#data = subset(data, condition != "CNPC")
#data = subset(data, condition != "SUBJ")
d=transform(data, block_sequence = as.numeric(block_sequence))
write.csv(d,"../data/exp1b_cleaned.csv", row.names = FALSE)


#############################
# Step 6: Statistics
#############################

#model_block <- lmer(response~block_sequence*condition + (1+block_sequence*condition|workerid)+(1+condition|item_number), data = d)
#summary(model_block)

# model_global2 <- lmer(response~trial_sequence_total*item_type +
#                         (1+trial_sequence_total*item_type|workerid)+(1|item_number), data = d, verbose = 100)
#   summary(model_global2)
#   anova(model_global2)

d <- read.csv("../data/exp1b_cleaned.csv")
d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ","UNGRAM", "POLAR"))
d$phase <- factor(d$phase, levels = c("exposure", "test"))
d$phase2 <- factor(d$phase2, levels= c("pre-exposure", "exposure", "test"))


d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_ungram$item_type <- factor(d_no_ungram$item_type, levels = c("FILL", "SUBJ", "WH", "POLAR") )

d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("POLAR", "WH", "SUBJ"))

d_no_fillers$exposure_condition <- factor(d_no_fillers$exposure_condition, c("POLAR", "SUBJ", "WH"))



# option 1: fully-crossed model
# data: all
# ref. levels: test phase
# run twice with ref levels as each control
# generalization: pos. exposure_condition effect wrt negative control, no
# significant effect wrt positive control
#
# What does the interaction term tell us?

d_subj <- subset(d_no_fillers, test_condition=="SUBJ")

# set reference level to test and polar
d_subj$phase <- factor(d_subj$phase, c("test", "exposure"))
d_subj$exposure_condition <- factor(d_subj$exposure_condition, c("POLAR", "SUBJ", "WH"))

phase_model1 <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_subj,
  # verbose = 100
)

summary(phase_model1)
# save output to a txt file
sink(file="generalization_model1.txt")
summary(phase_model1)
sink(file=NULL)




# set reference level to within-category
d_subj$phase <- factor(d_subj$phase, c("test", "exposure"))
d_subj$exposure_condition <- factor(d_subj$exposure_condition, c("SUBJ", "WH", "POLAR"))
phase_model2 <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_subj,
  # verbose = 100
)

summary(phase_model2)
# save output to a txt file
sink(file="generalization_model2.txt")
summary(phase_model2)
sink(file=NULL)


###################

d_wh <- subset(d_no_fillers, test_condition=="WH")


# set reference level to test and polar
d_wh$phase <- factor(d_wh$phase, c("test", "exposure"))
d_wh$exposure_condition <- factor(d_wh$exposure_condition, c("POLAR", "SUBJ", "WH"))

phase_model1 <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_wh,
  # verbose = 100
)

summary(phase_model1)
# save output to a txt file
sink(file="generalization_model1.txt")
summary(phase_model1)
sink(file=NULL)




# set reference level to within-category
d_wh$phase <- factor(d_wh$phase, c("test", "exposure"))
d_wh$exposure_condition <- factor(d_wh$exposure_condition, c("SUBJ", "WH", "POLAR"))
phase_model2 <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_wh,
  # verbose = 100
)

summary(phase_model2)
# save output to a txt file
sink(file="generalization_model2.txt")
summary(phase_model2)
sink(file=NULL)






# option 2: simpler model
# data: only test phase
# run twice with ref levels as each control
# generalization: pos. main effect wrt negative control, no
# significant effect wrt positive control


phase_model <- lmer(
  response ~ exposure_condition +
    (1 | workerid) +
    (1 + exposure_condition | item_number), # with enough participants this works
  data = d_no_fillers,
  # verbose = 100
)






# # model to compare control's pre-exposure with experiment's test
# dd <- subset(d_no_fillers, phase2 != "exposure")
# dd <- subset(dd, !(phase2 == "pre-exposure" & group == "mismatch"))
# dd <- subset(dd, !(phase2 == "test" & group == "match"))
# dd$phase2 <- factor(dd$phase2, levels = c("pre-exposure", "test"))
# 
# phase_model2 <- lmer(
#   response ~ phase2 +
#     (1 | workerid) +
#     (1 | item_number),
#   data = dd
# )
# 
# summary(phase_model2)
# # save output to a txt file
# sink(file="generalization_model2.txt")
# summary(phase_model2)
# sink(file=NULL)

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

cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")
d <- read.csv("../data/pilot2_cleaned.csv")
d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ","UNGRAM"))
d$phase <- factor(d$phase, levels = c("exposure", "test"))

d = subset(d, block_sequence != "practice")
#d <- subset(d, group == "mismatch")

d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_ungram$item_type <- factor(d_no_ungram$item_type, levels = c("FILL", "SUBJ", "WH") )
d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("WH", "SUBJ"))

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
  ylab("Cumulative average acceptability rating")+
  geom_hline(yintercept=0.5, linetype="dashed",
             size=1)+
  theme( plot.margin = margin(0, 0, 0, 0, "cm"))+
  theme_bw()
b

ab <- ggarrange(a,b,
                labels = c("By-trial Average", "Cumulative Average"),
                ncol = 2, nrow = 1)



# phase chart

theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}

phase_avg = d_no_fillers %>%
  group_by(phase2, exposure_condition) %>%
  mutate(exposure_condition = fct_recode(exposure_condition,"subject island"="SUBJ", "whether island"="WH")) %>% 
  mutate(phase2 = fct_recode(phase2,"first 6 trials"="pre-exposure", "exposure"="exposure","test"="test")) %>% 
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)


phase_avg = subset(phase_avg, phase2 != "exposure")
phase_graph<- ggplot(phase_avg, aes(x=phase2,y=Mean, fill=exposure_condition)) +
  geom_bar(data=phase_avg, stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=CILow,ymax=CIHigh), position=position_dodge2(width=0.2, padding=0.5)) +
  scale_fill_manual(name="exposure condition", values=cbPalette) +
  #scale_alpha_manual(values=c(0.6,1)) +
  xlab("phase") +
  ylab("average acceptability") +
  theme_bw() 

phase_graph
ggsave("../graphs/pilot2_phase_bars.pdf",width=10,height=5)
ggsave("../graphs/pilot2_phase_bars.png",width=10,height=5)



# satiation curves

#calculate and plot trial average
trial_avg_control <- aggregate(d_no_fillers_control[,"response"],list(d_no_fillers_control$trial_sequence_total), mean)
names(trial_avg_control)[names(trial_avg_control) == "Group.1"] <- "trial"
names(trial_avg_control)[names(trial_avg_control) == "x"] <- "avg"

trial_avg_mismatch <- aggregate(d_no_fillers_mismatch[,"response"],list(d_no_fillers_mismatch$trial_sequence_total), mean)
names(trial_avg_mismatch)[names(trial_avg_mismatch) == "Group.1"] <- "trial"
names(trial_avg_mismatch)[names(trial_avg_mismatch) == "x"] <- "avg"

#trial_average plot

trial_avg_control <- trial_avg_control[order(trial_avg_control$trial),]
# cum <- cumsum(trial_avg$avg) / seq_along(trial_avg$avg)
# trial_avg$cum <- cum

curve= ggplot(trial_avg_control, aes(x=trial, y=avg)) +
  geom_smooth(method = lm, se = F) + geom_point()+  
  xlab("Trial Sequence") +
  ylab("Average acceptability rating")+
  theme_bw()

curve


new_d <- d_no_fillers
trial_means_p = d_no_fillers %>%
  group_by(exposure_condition, phase2,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()

  curve_split_p <- ggplot(new_d, aes(x=trial_sequence_total, y=response, color = exposure_condition)) +
    geom_point(data=trial_means_p,alpha=.9) +
    scale_color_manual(values=cbPalette) +
    scale_fill_manual(values=cbPalette) +
    xlab("Trial Sequence") +
    ylab("Acceptability rating")+
    geom_smooth(data=subset(trial_means_p, phase2=="exposure"),method=lm, aes(fill=exposure_condition))+
    geom_smooth(data=subset(trial_means_p, phase2=="pre-exposure"),method=lm, aes(fill=exposure_condition))+
    geom_smooth(data=subset(trial_means_p, phase2=="test"),method=lm, aes(fill=exposure_condition))+
    geom_vline(xintercept=6, linetype="dashed", size=1) +
    geom_vline(xintercept=40, linetype="dashed",
               size=1)+
    theme_bw()

curve_split_p
  
ggsave("../graphs/pilot2_phase_p_curves.pdf",width=10,height=5)
ggsave("../graphs/pilot2_phase_p_curves.png",width=10,height=5)

trial_means = d_no_fillers %>%
  group_by(exposure_condition, phase,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()

curve_split <- ggplot(new_d, aes(x=trial_sequence_total, y=response, color = exposure_condition)) +
  geom_point(data=trial_means,alpha=.9) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  xlab("Trial Sequence") +
  ylab("Acceptability rating")+
  geom_smooth(data=subset(trial_means, phase=="exposure"),method=lm, aes(fill=exposure_condition))+
  geom_smooth(data=subset(trial_means, phase=="test"),method=lm, aes(fill=exposure_condition))+
  geom_vline(xintercept=40, linetype="dashed",
             size=1)+
  theme_bw()

curve_split

ggsave("../graphs/pilot2_phase_curves.pdf",width=10,height=5)
ggsave("../graphs/pilot2_phase_curves.png",width=10,height=5)

curve_grouped <- ggplot(new_d, aes(x=trial_sequence_total, y=response, color = exposure_condition)) +
  geom_point(data=trial_means,alpha=.9) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  xlab("Trial Sequence") +
  ylab("Acceptability rating")+
  geom_smooth(method=lm, aes(fill=exposure_condition))+
  geom_vline(xintercept=40, linetype="dashed",
             size=1)+
  theme_bw()

ggsave("../graphs/pilot2_curves.pdf",width=10,height=5)
ggsave("../graphs/pilot2_curves.png",width=10,height=5)

curve_split_p
curve_grouped


#overall plot:
nd = d %>%
  mutate(item_type = fct_recode(item_type,"grammatical"="FILL","ungrammatical"="UNGRAM", "subject island"="SUBJ", "whether island"="WH")) %>% 
  #, "complex-NP island"="CNPC", "whether island"="WH")) %>%
  mutate(item_type = fct_relevel(item_type,"grammatical","whether island","subject island", "ungrammatical")) %>%
  mutate(exposure_condition = fct_recode(exposure_condition,"subject island"="SUBJ", "whether island"="WH")) 
#complex-NP island","subject island", "ungrammatical"))

trial_means = nd %>%
  group_by(item_type,phase,trial_sequence_total, exposure_condition) %>%
  summarize(response = mean(response)) %>%
  ungroup()

# ggplot(nd, aes(x=trial_sequence_total, y=response, color = item_type)) +
#   geom_point(data=trial_means,alpha=.9) +
#   xlab("Trial Sequence") +
#   ylab("Acceptability rating")+
# 
#   # fillers
#   geom_smooth(data=subset(trial_means, item_type == "grammatical" ),method=lm, aes(fill="grammatical")) +
#   geom_smooth(data=subset(trial_means, item_type == "ungrammatical"),method=lm, aes(fill="ungrammatical")) +
#   # wh items
#   geom_smooth(data=subset(trial_means, !(item_type == "ungrammatical" | item_type == "grammatical") & exposure_condition=="WH"),method=lm, aes(fill="match")) +
#   # exposure
#   geom_smooth(data=subset(trial_means, !(item_type == "ungrammatical" | item_type == "grammatical" | exposure_condition=="WH") & phase== "exposure"),method=lm, aes(fill="mismatch")) +
#   # test
#   geom_smooth(data=subset(trial_means, !(item_type == "ungrammatical" | item_type == "grammatical" | exposure_condition=="WH") & phase== "test"),method=lm, aes(fill="mismatch")) +
#   
#   scale_color_manual(name="item type", values=cbPalette) +
#   scale_fill_manual(name="experiment group", values=cbPalette) +
# 
#   theme_bw()

ggplot(nd, aes(x=trial_sequence_total, y=response, color = item_type, shape=exposure_condition)) +
  geom_point(data=trial_means,alpha=.9) +
  xlab("Trial Sequence") +
  ylab("Acceptability rating")+
  geom_smooth(data=trial_means, method=lm, aes(fill=exposure_condition))+

  scale_color_manual(name="item type", values=cbPalette) +
  scale_fill_manual(name="exposure condition", values=cbPalette, guide=FALSE) +
  scale_shape(name="exposure condition") +
  theme_bw()

ggsave("../graphs/satiation_pilot2_plot.pdf",width=10,height=5)
ggsave("../graphs/satiation_pilot2_plot.png",width=10,height=5)

#by-subject Plot
ggplot(d, aes(x=trial_sequence_total, y=response, color = item_type, shape = item_type)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=item_type))+facet_wrap(~workerid)
ggsave("../graphs/subject_variability_pilot2.pdf", width=20, height = 25)
ggsave("../graphs/subject_variability_pilot2.png", width=20, height = 25)
