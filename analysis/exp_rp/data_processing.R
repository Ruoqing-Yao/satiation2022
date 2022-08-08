this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
this.dir
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

`%notin%` <- Negate(`%in%`)
raw_data_path <- "data_raw.csv"
data <- read.csv(raw_data_path)

d2 <- data


#############################
# Step 1: Filter out the participants who responded incorrectly more than once to the practice questions:
#############################

excluded_subjects <- c()
practice_data=subset(data,block_sequence == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- c(excluded_subjects, subset(data, !is.element(workerid, practice_good_data$workerid))$workerid)
data=subset(data, is.element(workerid, practice_good_data$workerid))

length(unique(data$workerid))


#############################
# Step 2: filter: no overlap of 95%CI of FILL and UNGRAM
#############################

filler_data = subset(data, item_condition == "FILL")
ungram_data = subset(data, item_condition == "UG")

theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}

filler_by_subject = aggregate(filler_data[,"rating_response"],list(filler_data$workerid), ci.low)
ungram_by_subject = aggregate(ungram_data[,"rating_response"],list(ungram_data$workerid), ci.high)

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
# save cleaned data
#############################

#Clean practice trials and control trials.
data = subset(data, block_sequence != "practice")
data$item_condition <- factor(data$item_condition, levels = c("FILL", "UG","CNPC","WH"))
data$item_group <- factor(data$item_group, levels = c("RP","GAP"))
d=transform(data, block_sequence = as.numeric(block_sequence))
write.csv(d,"data_clean.csv", row.names = FALSE)


#############################
# Statistics
#############################

d <- read.csv("data_clean.csv")
d$item_condition <- factor(d$item_condition, levels = c("FILL", "UG","CNPC","WH"))


d_no_ungram <- subset(d, item_condition != "UG")
d_no_ungram$item_condition <- factor(d_no_ungram$item_condition, levels = c("FILL", "UG","CNPC","WH") )
d_no_fillers <- subset(d_no_ungram, item_condition != "FILL")
d_no_fillers$item_condition <- factor(d_no_fillers$item_condition, levels = c("CNPC","WH") )



#############################
# plots and graphs
#############################


d_no_ungram <- subset(d, item_condition != "UG")
d_no_ungram$item_condition <- factor(d_no_ungram$item_condition, levels = c("FILL", "CNPC", "WH") )
d_no_fillers <- subset(d_no_ungram, item_condition != "FILL")
d_no_fillers$item_condition <- factor(d_no_fillers$item_condition, c("WH", "CNPC"))


theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}



# average acceptability rating plot

exp_avg = d %>%
  group_by(item_condition, item_group) %>%
  mutate(item_condition = fct_recode(item_condition,"whether island"="WH", "Complex NP"="CNPC", "Filler" = "FILL", "Word Salad" = "UG")) %>% 
  summarize(Mean = mean(rating_response), CILow = ci.low(rating_response), CIHigh = ci.high(rating_response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)



graph<- ggplot(exp_avg, aes(x=item_condition,y=Mean, fill=item_group)) +
  geom_bar(data=exp_avg, stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=CILow,ymax=CIHigh), position=position_dodge2(width=0.2, padding=0.5)) +
  scale_fill_manual(name="Resumptive v.s. GAP", values=cbPalette) +
  #scale_alpha_manual(values=c(0.6,1)) +
  xlab("Sentence Type") +
  ylab("average acceptability") +
  theme_bw() 

graph

trial_means = d_no_fillers %>%
  group_by(item_condition,item_group, trial_sequence_total) %>%
  summarize(rating_response = mean(rating_response)) %>%
  ungroup()

ggplot(d_no_fillers, aes(x=trial_sequence_total, y=rating_response, color = item_condition, linetype=item_group)) +
  geom_point(data=trial_means,alpha=.9) +
  xlab("trial sequence") +
  ylab("rating response")+
  geom_smooth(method = glmer) +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()




# acceptability satiation plots


data_CNPC <- subset(d_no_fillers, experiment_condition == "CNPC") 
data_WH <- subset(d_no_fillers, experiment_condition == "WH") 

data_CNPC$block_sequence <- as.numeric(data_CNPC$block_sequence)
data_WH$block_sequence <- as.numeric(data_WH$block_sequence)

trial_means_CNPC = data_CNPC %>%
  group_by(item_condition,item_group, block_sequence) %>%
  summarize(rating_response = mean(rating_response)) %>%
  ungroup()

trial_means_WH = data_WH %>%
  group_by(item_condition,item_group, block_sequence) %>%
  summarize(rating_response = mean(rating_response)) %>%
  ungroup()


trial_means = trial_means %>%
  group_by(test_condition, condition,phase, item_type, group, block_sequence, exposure_condition) %>%
  summarize(response = mean(response)) %>%
  ungroup()



cbPalette = c("#e69d00", "#009e74","#d55e00",  "#cc79a7", "#0071b2")

# CNPC acceptability satiation
ggplot(data_CNPC, aes(x=block_sequence, y=rating_response, color = item_group, fill=item_group, linetype=item_condition)) +
  geom_point(data=trial_means_CNPC,alpha=.9) +
  xlab("trial sequence") +
  ylab("acceptability")+
  geom_smooth(method = loess, aes(fill=item_group)) +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()


# WH acceptability satiation
ggplot(data_WH, aes(x=block_sequence, y=rating_response, color = item_group, fill=item_group, linetype=item_condition)) +
  geom_point(data=trial_means_WH,alpha=.9) +
  xlab("trial sequence") +
  ylab("average acceptability")+
  geom_smooth(method = loess, aes(fill=item_group)) +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()



# Analaysis for Comprehension Questions

d2 <- data %>%
  mutate(comp  = if_else(response_correctness == "Correct", 1, 0))

d_no_ungram <- subset(d2, item_condition != "UG")
d_no_ungram$item_condition <- factor(d_no_ungram$item_condition, levels = c("FILL", "CNPC", "WH") )
d_no_fillers <- subset(d_no_ungram, item_condition != "FILL")
d_no_fillers$item_condition <- factor(d_no_fillers$item_condition, c("WH", "CNPC"))


# Average comprehension plots
comp_avg = d_no_fillers %>%
  group_by(item_condition, item_group) %>%
  mutate(item_condition = fct_recode(item_condition,"whether island"="WH", "Complex NP"="CNPC")) %>% 
  summarize(Mean = mean(comp), CILow = ci.low(comp), CIHigh = ci.high(comp)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)



graph<- ggplot(comp_avg, aes(x=item_condition,y=Mean, fill=item_group)) +
  geom_bar(data=comp_avg, stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=CILow,ymax=CIHigh), position=position_dodge2(width=0.2, padding=0.5)) +
  scale_fill_manual(name="Resumptive v.s. GAP", values=cbPalette) +
  #scale_alpha_manual(values=c(0.6,1)) +
  xlab("Sentence Type") +
  ylab("average comprehension") +
  theme_bw() 

graph

# Comprehension Satiation

data_CNPC <- subset(d_no_fillers, experiment_condition == "CNPC") 
data_WH <- subset(d_no_fillers, experiment_condition == "WH") 

trial_means_CNPC = data_CNPC %>%
  group_by(item_condition,item_group, trial_sequence_total) %>%
  summarize(comp = mean(comp)) %>%
  ungroup()

trial_means_WH = data_WH %>%
  group_by(item_condition,item_group, trial_sequence_total) %>%
  summarize(comp = mean(comp)) %>%
  ungroup()

# WH comprehension satiation
ggplot(data_WH, aes(x=trial_sequence_total, y=comp, color = item_group, fill=item_group, linetype=item_condition)) +
  geom_point(data=trial_means_WH,alpha=.9) +
  xlab("trial sequence") +
  ylab("average comprehension")+
  geom_smooth(method = loess, aes(fill=item_group)) +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()

# CNPC comprehension satiation
ggplot(data_CNPC, aes(x=trial_sequence_total, y=comp, color = item_group, fill=item_group, linetype=item_condition)) +
  geom_point(data=trial_means_CNPC,alpha=.9) +
  xlab("trial sequence") +
  ylab("average comprehension")+
  geom_smooth(method = loess, aes(fill=item_group)) +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()



# modeling

#overall modeling
#Getting data ready for stats
d <- data %>%
  mutate(comp  = if_else(response_correctness == "Correct", 1, 0))

d_no_ungram <- subset(d2, item_condition != "UG")
d_no_ungram$item_condition <- factor(d_no_ungram$item_condition, levels = c("FILL", "CNPC", "WH") )
d_no_fillers <- subset(d_no_ungram, item_condition != "FILL")
d_no_fillers$item_condition <- factor(d_no_fillers$item_condition, c("WH", "CNPC"))

data_CNPC <- subset(d_no_fillers, experiment_condition == "CNPC") 
data_WH <- subset(d_no_fillers, experiment_condition == "WH") 

scale(data_CNPC$trial_sequence_total, center = TRUE, scale = FALSE)
scale(data_CNPC$rating_response, center = TRUE, scale = FALSE)

scale(data_WH$trial_sequence_total, center = TRUE, scale = FALSE)
scale(data_WH$rating_response, center = TRUE, scale = FALSE)



model_all = lmer(rating_response ~ trial_sequence_total * comp + 
                   (1 + trial_sequence_total * comp|workerid) +
                   (1 + trial_sequence_total * comp|item_number),
                 data = data_CNPC, verbose = 100)

summary(model_all)


#sum-code item_group: 
data_CNPC$item_group <- factor(data_CNPC$item_group, c("RP", "GAP"))
contrasts(data_CNPC$item_group) = contr.sum(2)
model_CNPC = lmer(rating_response ~ item_group * trial_sequence_total 
                  + (1 + trial_sequence_total|workerid) + (1 + item_group|item_number),
                  data = data_CNPC)
summary(model_CNPC)


data_WH$item_group <- factor(data_WH$item_group, c("RP", "GAP"))
contrasts(data_WH$item_group) = contr.sum(2)
model_WH = lmer(rating_response ~ item_group * trial_sequence_total 
                  + (1 + trial_sequence_total|workerid) + (1 + item_group|item_number),
                  data = data_WH)
summary(model_WH)

if (!require("devtools")) {
  install.packages("devtools", dependencies = TRUE)}
devtools::install_github("DejanDraschkow/mixedpower") # mixedpower is hosted on GitHub
library(mixedpower)

#CNPC power

data_CNPC$workerid <- as.numeric(data_CNPC$workerid)

power_CNPC <- mixedpower(model = model_CNPC, data = data_CNPC,
                         fixed_effects = c("trial_sequence_total", "item_group"),
                         simvar = "workerid", steps = c(20, 40, 60),
                         critical_value = 2, n_sim = 100)
power_CNPC

#WH power

data_WH$workerid <- as.numeric(data_WH$workerid)

power_WH <- mixedpower(model = model_WH, data = data_WH,
                         fixed_effects = c("trial_sequence_total", "item_group"),
                         simvar = "workerid", steps = c(100, 200, 300),
                         critical_value = 2, n_sim = 100)
power_WH

#CNPC

d_no_fillers$item_group <- factor(d_no_fillers$item_group, c("RP", "GAP"))
d_no_fillers$item_condition <- factor(d_no_fillers$item_condition, c("CNPC", "WH"))
contrasts(d_no_fillers$item_group) = contr.sum(2)
contrasts(d_no_fillers$item_condition) = contr.sum(2)

d_no_fillers$comp <- as.factor(d_no_fillers$comp)

model_comp_CNPC = glmer(comp ~ item_group
                   + (1|workerid) + (1+item_group|item_number), 
                   data = data_CNPC, family = "binomial")
summary(model_comp_CNPC)

model_comp_WH = glmer(comp ~ item_group
                   + (1|workerid) + (1+item_group|item_number), 
                   data = data_WH, family = "binomial")
summary(model_comp_WH)



#d_no_fillers$workerid <- as.numeric(d_no_fillers$workerid)

power_comp_CNPC <- mixedpower(model = model_comp_CNPC, data = data_CNPC,
                         fixed_effects = c("item_group"),
                         simvar = "workerid", steps = c(100, 200, 300),
                         critical_value = 2, n_sim = 100)
power_comp_CNPC

power_comp_WH <- mixedpower(model = model_comp_WH, data = data_WH,
                              fixed_effects = c("item_group"),
                              simvar = "workerid", steps = c(100, 200, 300),
                              critical_value = 2, n_sim = 100)
power_comp_WH



#prediction: positive 
item_group*trial_sequence_total
(experiment condition = CNPC, include fillers, word salad)







