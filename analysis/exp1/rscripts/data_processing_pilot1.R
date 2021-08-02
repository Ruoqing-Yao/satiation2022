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
raw_data_path <- "../data/pilot1_raw.csv"
data<-read.csv(raw_data_path)
cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")


# remove subject information from data
data <- data %>% select(-(catch_trials:system.screenW)) 


# average experiment length
mean(data$time_in_minutes)

head(data)

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

non_Eng <- c("119")

data = subset(data, workerid %notin% non_Eng)


# how many excluded participants?
n_participants <- 80 
exclusion_rate <- (n_participants - length(unique(data$workerid)))/n_participants


#############################
# save cleaned data
#############################

#Clean practice trials and control trials.
data = subset(data, block_sequence != "practice")
data$item_type <- factor(d$item_type, levels = c("FILL", "UNGRAM","SUBJ","WH"))
#data = subset(data, condition != "UNGRAM")
#data = subset(data, condition != "FILL")
#data = subset(data, condition != "CNPC")
#data = subset(data, condition != "SUBJ")
d=transform(data, block_sequence = as.numeric(block_sequence))
write.csv(d,"../data/pilot1_cleaned.csv", row.names = FALSE)





#############################
# Step 6: Statistics
#############################

#model_block <- lmer(response~block_sequence*condition + (1+block_sequence*condition|workerid)+(1+condition|item_number), data = d)
#summary(model_block)

# model_global2 <- lmer(response~trial_sequence_total*item_type +
#                         (1+trial_sequence_total*item_type|workerid)+(1|item_number), data = d, verbose = 100)
#   summary(model_global2)
#   anova(model_global2)

d <- read.csv("../data/pilot1_cleaned.csv")
d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ","UNGRAM"))
d$phase <- factor(d$phase, levels = c("pre-exposure", "exposure", "test"))

d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_ungram$item_type <- factor(d_no_ungram$item_type, levels = c("FILL", "SUBJ", "WH") )

d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("SUBJ", "WH"))

d_no_fillers$exposure_condition <- factor(d_no_fillers$exposure_condition, c("SUBJ", "WH"))
# d_no_fillers$exposure_condition <- relevel(d_no_fillers$exposure_condition, ref="SUBJ")

# phase model
d_pre_post <- subset(d_no_fillers, phase != "exposure")
d_pre_post$phase <- factor(d_pre_post$phase, levels = c("pre-exposure", "test"))
phase_model <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_pre_post,
  # verbose = 100
)

summary(phase_model)
# save output to a txt file
sink(file="generalization_model.txt")
summary(phase_model)
sink(file=NULL)

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

#power analysis
#model_ext_class <- extend(model_global2, along="workerid", n=150)

#model_ext_class
#p_curve_treat <- powerCurve(model_ext_class, nsim=10, test = fcompare(response~trial_sequence_total*condition), along="workerid", breaks=c(50,100,150))
#plot(p_curve_treat)
#powerSim(model_global2, test=fcompare(response~trial_sequence_total*condition))


#############################
# plots and graphs
#############################

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")


d <- read.csv("../data/pilot1_cleaned.csv")
d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ","UNGRAM"))
d$phase <- factor(d$phase, levels = c("pre-exposure", "exposure", "test"))

d = subset(d, block_sequence != "practice")
#d <- subset(d, group == "mismatch")

d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_ungram$item_type <- factor(d_no_ungram$item_type, levels = c("FILL", "SUBJ", "WH") )
d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("WH", "SUBJ"))

d_no_fillers_mismatch <- subset(d_no_fillers, group == "mismatch")
d_no_fillers_control <- subset(d_no_fillers, group == "match")


#calculate and plot trial/cumulative average with all sentence types
# not a useful chart
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
# not a useful chart
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


theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}

phase_avg = d_no_fillers %>%
  group_by(phase,exposure_condition) %>%
  mutate(exposure_condition = fct_recode(exposure_condition,"subject island"="SUBJ", "whether island"="WH")) %>% 
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

phase_avg = subset(phase_avg, phase != "exposure")
phase_graph<- ggplot(phase_avg, aes(x=exposure_condition,y=Mean, fill=exposure_condition)) +
  geom_bar(data=phase_avg, stat="identity", position="dodge", aes(alpha=phase)) +
  geom_errorbar(aes(ymin=CILow,ymax=CIHigh), position=position_dodge2(width=0.2, padding=0.5)) +
  scale_fill_manual(values=cbPalette, guide=FALSE) +
  scale_alpha_manual(values=c(0.6,1)) +
  xlab("exposure type") +
  ylab("average acceptability of whether islands") +
  theme_bw() 

phase_graph
ggsave("../graphs/pilot1_phase_bars.pdf",plot=phase_graph,width=10,height=5)
ggsave("../graphs/pilot1_phase_bars.png",plot=phase_graph,width=10,height=5)

# satiation curves

#calculate and plot trial/cumulative average for non-filler items
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

new_d = d_no_fillers
trial_means = new_d %>%
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
  geom_smooth(data=subset(trial_means, phase=="pre-exposure"),method=lm, aes(fill=exposure_condition))+
  geom_smooth(data=subset(trial_means, phase=="test"),method=lm, aes(fill=exposure_condition))+
  geom_vline(xintercept=12, linetype="dashed", size=1) +
  geom_vline(xintercept=52, linetype="dashed",
             size=1)+
  theme_bw()

ggsave("../graphs/pilot1_phase_curves.pdf",width=10,height=5)
ggsave("../graphs/pilot1_phase_curves.png",width=10,height=5)

curve_grouped <- ggplot(new_d, aes(x=trial_sequence_total, y=response, color = exposure_condition)) +
  geom_point(data=trial_means,alpha=.9) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  xlab("Trial Sequence") +
  ylab("Acceptability rating")+
  geom_smooth(method=lm, aes(fill=exposure_condition))+
  geom_vline(xintercept=12, linetype="dashed", size=1) +
  geom_vline(xintercept=52, linetype="dashed",
             size=1)+
  theme_bw()

ggsave("../graphs/pilot1_curves.pdf",width=10,height=5)
ggsave("../graphs/pilot1_curves.png",width=10,height=5)

curve_split
curve_grouped

# chart with fillers curves
nd = d %>%
  mutate(item_type = fct_recode(item_type,"grammatical"="FILL","ungrammatical"="UNGRAM", "subject island"="SUBJ", "whether island"="WH")) %>% 
  #, "complex-NP island"="CNPC", "whether island"="WH")) %>%
  mutate(item_type = fct_relevel(item_type,"grammatical","whether island","subject island", "ungrammatical")) %>%
  mutate(exposure_condition = fct_recode(exposure_condition,"subject island"="SUBJ", "whether island"="WH")) 
#complex-NP island","subject island", "ungrammatical"))

trial_means = nd %>%
  group_by(exposure_condition,item_type,phase,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()

ggplot(nd, aes(x=trial_sequence_total, y=response, color = item_type, shape=exposure_condition)) +
  geom_point(data=trial_means,alpha=.9) +
  xlab("Trial Sequence") +
  ylab("Acceptability rating")+

  # fillers
  geom_smooth(data=subset(trial_means, item_type == "grammatical" ),method=lm, aes(fill=exposure_condition)) +
  geom_smooth(data=subset(trial_means, item_type == "ungrammatical"),method=lm, aes(fill=exposure_condition)) +
  # wh items
  geom_smooth(
    data=subset(trial_means, !(item_type == "ungrammatical" | item_type == "grammatical") & exposure_condition=="whether island"),
    method=lm, aes(fill=exposure_condition)
    ) +
  # pre-exposure
  geom_smooth(
    data=subset(trial_means, !(item_type == "ungrammatical" | item_type == "grammatical" | exposure_condition=="whether island") & phase=="pre-exposure"),
    method=lm, aes(fill=exposure_condition)
    ) +
  # exposure
  geom_smooth(
    data=subset(trial_means, !(item_type == "ungrammatical" | item_type == "grammatical" | exposure_condition=="whether island") & phase=="exposure"),
    method=lm, aes(fill=exposure_condition)
    ) +
  # test
  geom_smooth(
    data=subset(trial_means, !(item_type == "ungrammatical" | item_type == "grammatical" | exposure_condition=="whether island") & phase=="test"),
    method=lm, aes(fill=exposure_condition)
    ) +
  
  scale_color_manual(name="item type", values=cbPalette) +
  scale_fill_manual(name="exposure condition", values=cbPalette, guide=FALSE) +
  scale_shape(name="exposure condition") +
  theme_bw()


ggsave("../graphs/satiation_pilot1_plot.pdf",width=10,height=5)
ggsave("../graphs/satiation_pilot1_plot.png",width=10,height=5)

#by-subject Plot
ggplot(d, aes(x=trial_sequence_total, y=response, color = item_type, shape = item_type)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=item_type))+facet_wrap(~workerid)
ggsave("../graphs/subject_variability_pilot1.pdf", width=20, height = 25)
ggsave("../graphs/subject_variability_pilot1.png", width=20, height = 25)