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

# install mixedpower
if (!require("devtools")) {
  install.packages("devtools", dependencies = TRUE)}
devtools::install_github("DejanDraschkow/mixedpower") # mixedpower is hosted on GitHub

# load library
library(mixedpower)
`%notin%` <- Negate(`%in%`)
raw_data_path <- "../data/exp1c_raw.csv"
data<-read.csv(raw_data_path)

# remove subject information from data
#data <- data %>% select(-(catch_trials:system.screenW)) 

d2 <- data

mean(data$time_in_minutes)

times <- ggplot(data, aes(x=time_in_minutes)) +
  geom_histogram(binwidth=1) +
theme_bw()
times

n_parts <- length(unique(data$workerid))

head(data)

# # create pseudo-pre-exposure phase
# # this isn't used in this version of the experiment
# data$phase2 <- data$phase
# data$phase2 <- factor(data$phase2, levels = c("pre-exposure", "exposure", "test"))
# data$phase2[as.integer(data$trial_sequence_total) <= 6 & data$block_sequence != "practice"] <- "pre-exposure"

#############################
# Step 1: Filter out the participants who responded incorrectly more than once to the practice questions:
#############################

excluded_subjects <- c()
practice_data=subset(data,block_sequence == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- c(excluded_subjects, subset(data, !is.element(workerid, practice_good_data$workerid))$workerid)
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
  else{
    excluded_subjects <- c(excluded_subjects, row$subject)
  }
}
data = subset(data, workerid %in% eligible_subjects)

length(unique(data$workerid))

#############################
# Step 3: exclude non-English speakers
#############################

non_Eng <- c("1719", "1408", "1091", "1308", "1404")

data = subset(data, workerid %notin% non_Eng)


# examine excluded subjects
##################

excluded_subjects <- c(excluded_subjects, as.integer(non_Eng))

excluded_data <- subset(d2, workerid %in% excluded_subjects)

# how many excluded participants?
n_participants <- 973
length(unique(data$workerid))
exclusion_rate <- (n_participants - length(unique(data$workerid)))/n_participants

# look at excluded participants
# which conditions did excluded participants see?
excluded <- subset(excluded_data, trial_sequence_total=="1") %>%
  group_by(test_condition, exposure_condition) %>%
  summarise(cnt = n()) %>%
  ungroup()

ee <- subset(excluded_data, trial_sequence_total=="1")


# plot of excluded participants by time to completion
ggplot(ee, aes(x=time_in_minutes)) +
  geom_histogram(binwidth=1) +
  theme_bw()

# plot of included participants by time to completion
ggplot(data, aes(x=time_in_minutes)) +
  geom_histogram(binwidth=1) +
  theme_bw()

mean(excluded_data$time_in_minutes)

ee$subject_information.problems

head(data)

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
write.csv(d,"../data/exp1c_cleaned.csv", row.names = FALSE)


#############################
# Statistics
#############################

d <- read.csv("../data/exp1c_cleaned.csv")
d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ","UNGRAM", "POLAR"))
d$phase <- factor(d$phase, levels = c("exposure", "test"))
d$phase2 <- factor(d$phase2, levels= c("pre-exposure", "exposure", "test"))


d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_ungram$item_type <- factor(d_no_ungram$item_type, levels = c("FILL", "POLAR", "SUBJ", "WH") )

d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("POLAR", "SUBJ", "WH"))

d_no_fillers$exposure_condition <- factor(d_no_fillers$exposure_condition, c("POLAR", "SUBJ", "WH"))



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

# d_subj <- subset(d_subj, trial_sequence_total<=30)

# set reference level to test phase and between-category
d_subj$phase <- factor(d_subj$phase, c("test", "exposure"))
d_subj$exposure_condition <- factor(d_subj$exposure_condition, c("WH", "SUBJ", "POLAR"))

phase_model_subj1 <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_subj,
  # verbose = 100
)

summary(phase_model_subj1)
# save output to a txt file
sink(file="generalization_subj.txt")
summary(phase_model_subj1)
sink(file=NULL)

power_subj <- mixedpower(model = phase_model_subj1, data = d_subj,
                        fixed_effects = c("phase", "exposure_condition"),
                        simvar = "workerid", steps = c(60,360,480,600),
                        critical_value = 2, n_sim = 10)
power_subj
plots <- multiplotPower(power_subj)

plots


# whether test condition
##########################

d_wh <- subset(d_no_fillers, test_condition=="WH")

# set reference level to test and polar
d_wh$phase <- factor(d_wh$phase, c("test", "exposure"))
d_wh$exposure_condition <- factor(d_wh$exposure_condition, c("SUBJ", "POLAR", "WH"))

phase_model_wh1 <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_wh,
  # verbose = 100
)

summary(phase_model_wh1)
# save output to a txt file
sink(file="generalization_wh.txt")
summary(phase_model_wh1)
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

cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")
d <- read.csv("../data/exp1c_cleaned.csv")
d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ", "POLAR", "UNGRAM"))
d$phase <- factor(d$phase, levels = c("exposure", "test"))

d = subset(d, block_sequence != "practice")
#d <- subset(d, group == "mismatch")

d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_ungram$item_type <- factor(d_no_ungram$item_type, levels = c("FILL", "SUBJ","POLAR", "WH") )
d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("WH", "SUBJ", "POLAR"))

#calculate and plot trial/cumulative average

# trial_avg <- aggregate(d[,"response"],list(d$trial_sequence_total), mean)
# names(trial_avg)[names(trial_avg) == "Group.1"] <- "trial"
# names(trial_avg)[names(trial_avg) == "x"] <- "avg"
# 
# #trial_average plot
# trial_avg <- trial_avg[order(trial_avg$trial),]
# cum <- cumsum(trial_avg$avg) / seq_along(trial_avg$avg)
# trial_avg$cum <- cum
# 
# a= ggplot(trial_avg, aes(x=trial, y=avg)) +
#   geom_smooth(method = lm, se = F) + geom_point()+  
#   xlab("Trial Sequence") +
#   ylab("Average acceptability rating")+
#   theme_bw()
# a
# 
# 
# #cum_average plot
# b=ggplot(trial_avg, aes(x=trial, y=cum)) +
#   
#   geom_smooth (se = F) + geom_point()+
#   xlab("Trial number") +
#   ylab("Cumulative average acceptability rating")+
#   geom_hline(yintercept=0.5, linetype="dashed",
#              size=1)+
#   theme( plot.margin = margin(0, 0, 0, 0, "cm"))+
#   theme_bw()
# b
# 
# ab <- ggarrange(a,b,
#                 labels = c("By-trial Average", "Cumulative Average"),
#                 ncol = 2, nrow = 1)
# 


# bar chart

theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}
# 
# phase_avg = d_no_fillers %>%
#   group_by(phase2, exposure_condition) %>%
#   mutate(exposure_condition = fct_recode(exposure_condition,"subject island"="SUBJ", "whether island"="WH")) %>% 
#   mutate(phase2 = fct_recode(phase2,"first 6 trials"="pre-exposure", "exposure"="exposure","test"="test")) %>% 
#   summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
#   ungroup() %>%
#   mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
# 
# 
# phase_avg = subset(phase_avg, phase2 != "exposure")
# phase_graph<- ggplot(phase_avg, aes(x=phase2,y=Mean, fill=exposure_condition)) +
#   geom_bar(data=phase_avg, stat="identity", position="dodge") +
#   geom_errorbar(aes(ymin=CILow,ymax=CIHigh), position=position_dodge2(width=0.2, padding=0.5)) +
#   scale_fill_manual(name="exposure condition", values=cbPalette) +
#   #scale_alpha_manual(values=c(0.6,1)) +
#   xlab("phase") +
#   ylab("average acceptability") +
#   theme_bw() 
# 
# phase_graph
# ggsave("../graphs/pilot2_phase_bars.pdf",width=10,height=5)
# ggsave("../graphs/pilot2_phase_bars.png",width=10,height=5)


phase_avg = d_no_fillers %>%
  group_by(exposure_condition, test_condition, phase) %>%
  mutate(exposure_condition = fct_recode(exposure_condition,"subject island"="SUBJ", "whether island"="WH", "polar question"="POLAR")) %>% 
  mutate(test_condition = fct_recode(test_condition,"subject island"="SUBJ", "whether island"="WH")) %>% 
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)


phase_avg = subset(phase_avg, phase == "test")
phase_graph<- ggplot(phase_avg, aes(x=test_condition,y=Mean, fill=exposure_condition)) +
  geom_bar(data=phase_avg, stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=CILow,ymax=CIHigh), position=position_dodge2(width=0.2, padding=0.5)) +
  scale_fill_manual(name="exposure condition", values=cbPalette) +
  #scale_alpha_manual(values=c(0.6,1)) +
  xlab("test condition") +
  ylab("average acceptability") +
  theme_bw() 

phase_graph
ggsave("../graphs/exp1c_bars.pdf",width=10,height=5)
ggsave("../graphs/exp1c_bars.png",width=10,height=5)




#by-subject Plot
ggplot(d, aes(x=trial_sequence_total, y=response, color = item_type, shape = item_type)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=item_type))+facet_wrap(~workerid)
ggsave("../graphs/subject_variability_pilot1c.pdf", width=20, height = 25)
ggsave("../graphs/subject_variability_pilot1c.png", width=20, height = 25)



# overall plot
nd = d %>%
  mutate(item_type = fct_recode(item_type,"grammatical"="FILL","ungrammatical"="UNGRAM", "subject island"="SUBJ", "whether island"="WH", "polar question"="POLAR")) %>% 
  #, "complex-NP island"="CNPC", "whether island"="WH")) %>%
  mutate(item_type = fct_relevel(item_type,"grammatical","whether island","subject island", "polar question", "ungrammatical")) %>%
  mutate(exposure_condition = fct_recode(exposure_condition,"subject island"="SUBJ", "whether island"="WH", "polar question"="POLAR"))  %>%
mutate(test_condition = fct_recode(test_condition,"subject island"="SUBJ", "whether island"="WH")) %>%
  mutate(group = fct_recode(group, "control"="control", "within-category"="match", "between-category"="mismatch"))

trial_means = nd %>%
  group_by(exposure_condition,item_type, test_condition, phase,group,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()

trial_means$condition <- ""
trial_means$condition[trial_means$test_condition == "whether island"] <- "whether islands" 
trial_means$condition[trial_means$test_condition == "subject island"] <- "subject islands" 

trial_means$condition[trial_means$item_type == "grammatical" | trial_means$item_type=="ungrammatical"] <- "fillers"


trial_means = trial_means %>%
  group_by(test_condition, condition,phase, item_type, group, trial_sequence_total, exposure_condition) %>%
  summarize(response = mean(response)) %>%
  ungroup()


trial_means$condition <- factor(trial_means$condition, levels = c("fillers", "whether islands", "subject islands"))

# trial_means <- subset(trial_means, condition=="subject islands")
# trial_means <- subset(trial_means, condition=="whether islands")


cbPalette = c("#e69d00", "#009e74","#d55e00",  "#cc79a7", "#0071b2")

ggplot(nd, aes(x=trial_sequence_total, y=response, color = condition, fill=condition, linetype=group, shape=exposure_condition)) +
  geom_point(data=trial_means,alpha=.9) +
  xlab("trial sequence") +
  ylab("average acceptability")+
  geom_smooth(data=subset(trial_means, item_type == "grammatical" ),method=lm) +
  geom_smooth(data=subset(trial_means, item_type == "ungrammatical"),method=lm) +
  geom_smooth(data=subset(trial_means, condition != "fillers" & phase == "exposure"), method=lm) +
  geom_smooth(data=subset(trial_means, condition != "fillers" & phase == "test"), method=lm) +
  
  geom_vline(xintercept=24.5, linetype="dashed",
             size=0.5)+
  scale_color_manual(name="test item type", values=cbPalette) +
  scale_fill_manual(name="test item type", values=cbPalette) +
  scale_linetype(name="experiment group") +
  theme_bw()

ggsave("../graphs/exp_1c_overall.pdf",width=10,height=5)
ggsave("../graphs/exp_1c_overall.png",width=10,height=5)

# # bar plot
# d_no_ungram <- subset(d, item_type != "UNGRAM")
# d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
# d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("WH", "SUBJ", "POLAR"))
# #d_no_fillers$exposure_condition <- factor(d_no_fillers$exposure_condition, c("SUBJ", "WH"))
# 
# d_no_fillers <- subset(d_no_fillers, group=="control")
# # d_no_fillers <- subset(d_no_fillers, exposure_condition=="WH")
# 
# cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")
# cbPalette = c("#009e74","#d55e00","#e69d00","#cc79a7", "#0071b2")
# 
# theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
# ci.low <- function(x,na.rm=T) {
#   quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
# ci.high <- function(x,na.rm=T) {
#   quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}
# 
# phase_avg = d_no_fillers %>%
#   group_by(phase,exposure_condition, test_condition, item_type) %>%
#   mutate(exposure_condition = fct_recode(exposure_condition,"subject island"="SUBJ", "whether island"="WH", "polar question"="POLAR")) %>% 
#   mutate(item_type = fct_recode(item_type,"subject island"="SUBJ", "whether island"="WH", "polar question"="POLAR")) %>% 
#   summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
#   ungroup() %>%
#   mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
# 
# # phase_avg = subset(phase_avg, phase != "exposure")
# phase_graph<- ggplot(phase_avg, aes(x=test_condition,y=Mean, fill=test_condition)) +
#   geom_bar(data=phase_avg, stat="identity", position="dodge", aes(alpha=phase)) +
#   geom_errorbar(aes(ymin=CILow,ymax=CIHigh), position=position_dodge2(width=0.2, padding=0.5)) +
#   scale_fill_manual(name="item type", values=cbPalette) +
#   scale_alpha_manual(values=c(0.6,0.8,1)) +
#   xlab("test item type") +
#   ylab("average acceptability") +
#   theme_bw() 
# 
# phase_graph
