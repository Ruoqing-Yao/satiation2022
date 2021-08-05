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
raw_data_path <- "../data/exp1_raw.csv"
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

non_Eng <- c("235")

data = subset(data, workerid %notin% non_Eng)


# how many excluded participants?
n_participants <- 240 
exclusion_rate <- (n_participants - length(unique(data$workerid)))/n_participants



# average experiment length
mean(data$time_in_minutes)


ggplot(data, aes(x=time_in_minutes)) +
  geom_histogram(binwidth=1)
theme_bw()




#############################
# save cleaned data
#############################

#Clean practice trials and control trials.
data = subset(data, block_sequence != "practice")
data$item_type <- factor(data$item_type, levels = c("FILL", "UNGRAM","SUBJ","WH"))
#data = subset(data, condition != "UNGRAM")
#data = subset(data, condition != "FILL")
#data = subset(data, condition != "CNPC")
#data = subset(data, condition != "SUBJ")
d=transform(data, block_sequence = as.numeric(block_sequence))
write.csv(d,"../data/exp1_cleaned.csv", row.names = FALSE)


########################################
# OVERALL PLOT
#######################################

d <- read.csv("../data/exp1_cleaned.csv")
d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ","UNGRAM"))
d$phase <- factor(d$phase, levels = c("pre-exposure", "exposure", "test"))

d = subset(d, block_sequence != "practice")
#d <- subset(d, group == "mismatch")

#d <- subset(d, test_condition == "WH")

d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("SUBJ", "WH"))
d_no_fillers$exposure_condition <- factor(d_no_fillers$exposure_condition, c("SUBJ", "WH"))


d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_ungram$item_type <- factor(d_no_ungram$item_type, levels = c("FILL", "SUBJ", "WH") )
d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("WH", "SUBJ"))

d_no_fillers_mismatch <- subset(d_no_fillers, group == "mismatch")
d_no_fillers_control <- subset(d_no_fillers, group == "match")


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
  # # wh items
  # geom_smooth(
  #   data=subset(trial_means, !(item_type == "ungrammatical" | item_type == "grammatical") & exposure_condition=="whether island"),
  #   method=lm, aes(fill=exposure_condition)
  # ) +
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
  
  # pre-exposure
  geom_smooth(
    data=subset(trial_means, !(item_type == "ungrammatical" | item_type == "grammatical" | exposure_condition=="subject island") & phase=="pre-exposure"),
    method=lm, aes(fill=exposure_condition)
  ) +
  # exposure
  geom_smooth(
    data=subset(trial_means, !(item_type == "ungrammatical" | item_type == "grammatical" | exposure_condition=="subject island") & phase=="exposure"),
    method=lm, aes(fill=exposure_condition)
  ) +
  # test
  geom_smooth(
    data=subset(trial_means, !(item_type == "ungrammatical" | item_type == "grammatical" | exposure_condition=="subject island") & phase=="test"),
    method=lm, aes(fill=exposure_condition)
  ) +
  
  scale_color_manual(name="item type", values=cbPalette) +
  scale_fill_manual(name="exposure condition", values=cbPalette) +
  scale_shape(name="exposure condition ") +
  theme_bw()


###################################################
# CUMULATIVE AVERAGE
###################################################


# get trial/cummulative averages for each condition
# I'm sure there's a much better way to do this

# whole experiment
trial_avg <- aggregate(d[,"response"],list(d$trial_sequence_total), mean)
names(trial_avg)[names(trial_avg) == "Group.1"] <- "trial"
names(trial_avg)[names(trial_avg) == "x"] <- "avg"
trial_avg <- trial_avg[order(trial_avg$trial),]
cum <- cumsum(trial_avg$avg) / seq_along(trial_avg$avg)
trial_avg$cum <- cum
trial_avg$grp <- "all"

# no fillers
trial_avg_NOFILL <- aggregate(d_no_fillers[,"response"],list(d_no_fillers$trial_sequence_total), mean)
names(trial_avg_NOFILL)[names(trial_avg_NOFILL) == "Group.1"] <- "trial"
names(trial_avg_NOFILL)[names(trial_avg_NOFILL) == "x"] <- "avg"
trial_avg_NOFILL <- trial_avg_NOFILL[order(trial_avg_NOFILL$trial),]
cum <- cumsum(trial_avg_NOFILL$avg) / seq_along(trial_avg_NOFILL$avg)
trial_avg_NOFILL$cum <- cum
trial_avg_NOFILL$grp <- "all conditions"

trial_avg <- trial_avg_NOFILL#rbind(trial_avg, trial_avg_NOFILL)

# whether within-condition
whwh <- subset(d_no_fillers, exposure_condition=="WH" & group=="match")
trial_avg2 <- aggregate(whwh[,"response"],list(whwh$trial_sequence_total), mean)
names(trial_avg2)[names(trial_avg2) == "Group.1"] <- "trial"
names(trial_avg2)[names(trial_avg2) == "x"] <- "avg"
trial_avg2 <- trial_avg2[order(trial_avg2$trial),]
cum <- cumsum(trial_avg2$avg) / seq_along(trial_avg2$avg)
trial_avg2$cum <- cum
trial_avg2$grp <- "whether islands"

trial_avg <- rbind(trial_avg, trial_avg2)

# subject within-condition
subjsubj <- subset(d_no_fillers, exposure_condition=="SUBJ" & group=="match")
trial_avg2 <- aggregate(subjsubj[,"response"],list(subjsubj$trial_sequence_total), mean)
names(trial_avg2)[names(trial_avg2) == "Group.1"] <- "trial"
names(trial_avg2)[names(trial_avg2) == "x"] <- "avg"
trial_avg2 <- trial_avg2[order(trial_avg2$trial),]
cum <- cumsum(trial_avg2$avg) / seq_along(trial_avg2$avg)
trial_avg2$cum <- cum
trial_avg2$grp <- "subject islands"

trial_avg <- rbind(trial_avg, trial_avg2)

# whether between-condition
whsubj <- subset(d_no_fillers, exposure_condition=="WH" & group=="mismatch")
trial_avg2 <- aggregate(whsubj[,"response"],list(whsubj$trial_sequence_total), mean)
names(trial_avg2)[names(trial_avg2) == "Group.1"] <- "trial"
names(trial_avg2)[names(trial_avg2) == "x"] <- "avg"
trial_avg2 <- trial_avg2[order(trial_avg2$trial),]
cum <- cumsum(trial_avg2$avg) / seq_along(trial_avg2$avg)
trial_avg2$cum <- cum
trial_avg2$grp <- "whether to subject"

trial_avg <- rbind(trial_avg, trial_avg2)

# subject between-condition
subjwh <- subset(d_no_fillers, exposure_condition=="SUBJ" & group=="mismatch")
trial_avg2 <- aggregate(subjwh[,"response"],list(subjwh$trial_sequence_total), mean)
names(trial_avg2)[names(trial_avg2) == "Group.1"] <- "trial"
names(trial_avg2)[names(trial_avg2) == "x"] <- "avg"
trial_avg2 <- trial_avg2[order(trial_avg2$trial),]
cum <- cumsum(trial_avg2$avg) / seq_along(trial_avg2$avg)
trial_avg2$cum <- cum
trial_avg2$grp <- "subject to whether"

trial_avg <- rbind(trial_avg, trial_avg2)

my_colors <- cbPalette
names(my_colors) <- levels(trial_avg$grp)
#cum_average plot
cum_avg_plt=ggplot(trial_avg, aes(x=trial, y=cum, color=grp)) +
  
  
  # geom_smooth (data=subset(trial_avg, grp="all"),se = F) + 
  # geom_point(data=subset(trial_avg, grp="all"))+
  geom_smooth (se = F) + 
  geom_point()+
  # geom_smooth (data=subset(trial_avg, grp="whether islands"),se = F) + 
  # geom_point(data=subset(trial_avg, grp="whether islands"))+
  # geom_smooth (data=subset(trial_avg, grp="subject islands"),se = F) + 
  # geom_point(data=subset(trial_avg, grp="subject islands"))+
  # geom_smooth (data=subset(trial_avg, grp="whether to subject"),se = F) + 
  # geom_point(data=subset(trial_avg, grp="whether to subject"))+
  # geom_smooth (data=subset(trial_avg, grp="subject to whether"),se = F) + 
  # geom_point(data=subset(trial_avg, grp="subject to whether"))+
  # 
  xlab("Trial number") +
  ylab("Cumulative average acceptability rating")+
  geom_hline(yintercept=0.5, linetype="dashed",
             size=1)+
  theme( plot.margin = margin(0, 0, 0, 0, "cm"))+
  scale_color_manual(name="Condition", values=my_colors) +
  theme_bw()
cum_avg_plt

ggsave("../graphs/exp1_cumavg5.png",width=10,height=5)
