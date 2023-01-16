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

non_practice_data=subset(data, block_sequence != "practice")
length(unique(non_practice_data$workerid))
d2 <- non_practice_data %>%
  mutate(check = if_else(attention_correctness=="incorrect", 1, 0))
wrong <- d2 %>%
  group_by(workerid) %>%
  summarise(total_wrong_attempts = sum(check))
  
length(unique(d2$workerid))
length(unique(wrong$workerid))
non_practice_good_data = subset(wrong, total_wrong_attempts <= 1)
length(unique(non_practice_good_data$workerid))
non_practice_bad_data = subset(wrong, total_wrong_attempts > 1)
length(unique(non_practice_bad_data$workerid))
excluded_subjects <- c(excluded_subjects, !is.element(workerid, non_practice_good_data$workerid))$workerid
length(unique(non_practice_good_data$workerid))
data = subset(data, is.element(workerid, non_practice_good_data$workerid))
length(unique(data$workerid))

#############################
# Step 2: filter: no overlap of 95%CI of FILL and UNGRAM
#############################

filler_data = subset(data, item_sentence_type == "FILL")
ungram_data = subset(data, item_sentence_type == "UG")

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
data$item_sentence_type <- factor(data$item_sentence_type, levels = c("FILL", "UG","CNPC","WH"))
data$item_context_condition <- factor(data$item_context_condition, levels = c("SC","NC"))
d=transform(data, block_sequence = as.numeric(block_sequence))
write.csv(d,"data_clean.csv", row.names = FALSE)


#############################
# Statistics
#############################

d <- read.csv("data_clean.csv")
d$item_sentence_type <- factor(d$item_sentence_type, levels = c("FILL", "UG","CNPC","WH"))


d_no_ungram <- subset(d, item_sentence_type != "UG")
d_no_ungram$item_sentence_type <- factor(d_no_ungram$item_sentence_type, levels = c("FILL", "UG","CNPC","WH") )
d_no_fillers <- subset(d_no_ungram, item_sentence_type != "FILL")
d_no_fillers$item_sentence_type <- factor(d_no_fillers$item_sentence_type, levels = c("CNPC","WH") )



#############################
# plots and graphs
#############################


d_no_ungram <- subset(d, item_sentence_type != "UG")
d_no_ungram$item_sentence_type <- factor(d_no_ungram$item_sentence_type, levels = c("FILL", "CNPC", "WH") )
d_no_fillers <- subset(d_no_ungram, item_sentence_type != "FILL")
d_no_fillers$item_sentence_type <- factor(d_no_fillers$item_sentence_type, c("WH", "CNPC"))


theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}

cbPalette = c("#e69d00", "#009e74","#d55e00",  "#cc79a7", "#0071b2")

# average acceptability rating plot

exp_avg = d %>%
  group_by(item_sentence_type, item_context_condition) %>%
  mutate(item_sentence_type = fct_recode(item_sentence_type,"whether island"="WH", "Complex NP"="CNPC", "Filler" = "FILL", "Word Salad" = "UG")) %>% 
  summarize(Mean = mean(rating_response), CILow = ci.low(rating_response), CIHigh = ci.high(rating_response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)



graph<- ggplot(exp_avg, aes(x=item_sentence_type, y=Mean, fill=item_context_condition)) +
  geom_bar(data=exp_avg, stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=CILow,ymax=CIHigh), position=position_dodge2(width=0.2, padding=0.5)) +
  scale_fill_manual(name="SC vs NC", values=cbPalette) +
  #scale_alpha_manual(values=c(0.6,1)) +
  xlab("Sentence Type") +
  ylab("average acceptability") +
  theme_bw() 

graph

trial_means = d_no_fillers %>%
  group_by(item_sentence_type,item_context_condition, trial_sequence_total) %>%
  summarize(rating_response = mean(rating_response)) %>%
  ungroup()

#Acceptability Satiation All
cbPalette = c("#009e74", "#e69d00", "#d55e00",  "#cc79a7", "#0071b2")
ggplot(d_no_fillers, aes(x=trial_sequence_total, y=rating_response, color = item_sentence_type, fill = item_sentence_type, linetype=item_context_condition)) +
  geom_point(data=trial_means,alpha=.9) +
  xlab("trial sequence") +
  ylab("rating response")+
  geom_smooth(method = lm) +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"))




# acceptability satiation plots


data_CNPC <- subset(d_no_fillers, item_sentence_type == "CNPC") 
data_WH <- subset(d_no_fillers, item_sentence_type == "WH") 

data_CNPC$trial_sequence_total <- as.numeric(data_CNPC$trial_sequence_total)
data_WH$trial_sequence_total <- as.numeric(data_WH$trial_sequence_total)

trial_means_CNPC = data_CNPC %>%
  group_by(item_sentence_type, item_context_condition, trial_sequence_total) %>%
  summarize(rating_response = mean(rating_response)) %>%
  ungroup()

trial_means_WH = data_WH %>%
  group_by(item_sentence_type, item_context_condition, trial_sequence_total) %>%
  summarize(rating_response = mean(rating_response)) %>%
  ungroup()


trial_means = trial_means %>%
  group_by( item_sentence_type, item_context_condition, trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()

# group by SC and NC
data_SC <- subset(d_no_fillers, item_context_condition == "SC") 
data_NC <- subset(d_no_fillers, item_context_condition == "NC") 

data_SC$trial_sequence_total <- as.numeric(data_SC$trial_sequence_total)
data_NC$trial_sequence_total <- as.numeric(data_NC$trial_sequence_total)

trial_means_SC = data_SC %>%
  group_by(item_sentence_type, item_context_condition, trial_sequence_total) %>%
  summarize(rating_response = mean(rating_response)) %>%
  ungroup()

trial_means_NC = data_NC %>%
  group_by(item_sentence_type, item_context_condition, trial_sequence_total) %>%
  summarize(rating_response = mean(rating_response)) %>%
  ungroup()


trial_means = trial_means %>%
  group_by( item_sentence_type, item_context_condition, trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()





# CNPC acceptability satiation
ggplot(data_CNPC, aes(x=trial_sequence_total, y=rating_response, color = item_sentence_type, fill=item_sentence_type, linetype=item_context_condition)) +
  geom_point(data=trial_means_CNPC,alpha=.9) +
  xlab("trial sequence") +
  ylab("acceptability")+
  geom_smooth(method = lm, aes(fill=item_sentence_type)) +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()


# WH acceptability satiation
cbPalette = c("#009e74", "#e69d00", "#d55e00",  "#cc79a7", "#0071b2")
ggplot(data_WH, aes(x=trial_sequence_total, y=rating_response, color = item_sentence_type, fill=item_sentence_type, linetype=item_context_condition)) +
  geom_point(data=trial_means_WH,alpha=.9) +
  xlab("trial sequence") +
  ylab("acceptability")+
  geom_smooth(method = lm) +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()

# SC acceptability satiation
cbPalette = c("#e69d00", "#009e74", "#d55e00",  "#cc79a7", "#0071b2")
ggplot(data_SC, aes(x=trial_sequence_total, y=rating_response, color = item_sentence_type, fill=item_sentence_type, linetype = item_context_condition)) +
  geom_point(data=trial_means_SC,alpha=.9) +
  xlab("trial sequence") +
  ylab("acceptability")+
  geom_smooth(method = lm, aes(fill=item_sentence_type), linetype = "dotdash") +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()+
  theme(axis.text=element_text(size=15),
          axis.title=element_text(size=15,face="bold"))
  


# NC acceptability satiation
cbPalette = c("#009e74", "#e69d00", "#d55e00",  "#cc79a7", "#0071b2")
ggplot(data_NC, aes(x=trial_sequence_total, y=rating_response, color = item_sentence_type, fill=item_sentence_type)) +
  geom_point(data=trial_means_NC,alpha=.9) +
  xlab("trial sequence") +
  ylab("acceptability")+
  geom_smooth(method = lm) +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()+
  theme(axis.text=element_text(size=17),
        axis.title=element_text(size=15,face="bold"))


# Analysis for Comprehension Questions

d2 <- data %>%
  mutate(comp  = if_else(response_correctness == "Correct", 1, 0))

d_no_ungram <- subset(d2, item_sentence_type != "UG")
d_no_ungram$item_sentence_type <- factor(d_no_ungram$item_sentence_type, levels = c("FILL", "CNPC", "WH") )
d_no_fillers <- subset(d_no_ungram, item_sentence_type != "FILL")
d_no_fillers$item_sentence_type <- factor(d_no_fillers$item_sentence_type, c("WH", "CNPC"))

cbPalette = c("#e69d00", "#009e74","#d55e00",  "#cc79a7", "#0071b2")

# Average comprehension plots
comp_avg = d_no_fillers %>%
  group_by(item_sentence_type, item_context_condition) %>%
  mutate(item_sentence_type = fct_recode(item_sentence_type,"whether island"="WH", "Complex NP"="CNPC")) %>% 
  summarize(Mean = mean(comp), CILow = ci.low(comp), CIHigh = ci.high(comp)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)



graph<- ggplot(comp_avg, aes(x=item_sentence_type,y=Mean, fill=item_context_condition)) +
  geom_bar(data=comp_avg, stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=CILow,ymax=CIHigh), position=position_dodge2(width=0.2, padding=0.5)) +
  scale_fill_manual(name="Context condition", values=cbPalette) +
  #scale_alpha_manual(values=c(0.6,1)) +
  xlab("Sentence Type") +
  ylab("Average comprehension") +
  theme_bw() 

graph

# Comprehension Satiation

data_CNPC <- subset(d_no_fillers, item_sentence_type == "CNPC") 
data_WH <- subset(d_no_fillers, item_sentence_type == "WH") 

trial_means_CNPC = data_CNPC %>%
  group_by(item_sentence_type,item_context_condition, trial_sequence_total) %>%
  summarize(comp = mean(comp)) %>%
  ungroup()

trial_means_WH = data_WH %>%
  group_by(item_sentence_type,item_context_condition, trial_sequence_total) %>%
  summarize(comp = mean(comp)) %>%
  ungroup()

# CNPC comprehension satiation
ggplot(data_CNPC, aes(x=trial_sequence_total, y=comp, color = item_context_condition, fill=item_context_condition, linetype=item_sentence_type)) +
  geom_point(data=trial_means_CNPC,alpha=.9) +
  xlab("trial sequence") +
  ylab("average comprehension")+
  geom_smooth(method = lm, aes(fill=item_context_condition)) +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()

# WH comprehension satiation
cbPalette = c("#009e74", "#e69d00", "#d55e00",  "#cc79a7", "#0071b2")
ggplot(data_WH, aes(x=trial_sequence_total, y=comp, color = item_context_condition, fill=item_context_condition, linetype=item_sentence_type)) +
  geom_point(data=trial_means_WH,alpha=.9) +
  xlab("trial sequence") +
  ylab("average comprehension")+
  geom_smooth(method = lm, aes(fill=item_context_condition)) +
  scale_color_manual (values=cbPalette) +
  scale_fill_manual (values=cbPalette) +
  theme_bw()





# modeling

#overall modeling
#Getting data ready for stats
d <- data %>%
  mutate(comp  = if_else(response_correctness == "Correct", 1, 0))

d_no_ungram <- subset(d2, item_sentence_type != "UG")
d_no_ungram$item_sentence_type <- factor(d_no_ungram$item_sentence_type, levels = c("FILL", "CNPC", "WH") )
d_no_fillers <- subset(d_no_ungram, item_sentence_type != "FILL")
d_no_fillers$item_sentence_type <- factor(d_no_fillers$item_sentence_type, c("WH", "CNPC"))

data_CNPC <- subset(d_no_fillers, item_sentence_type == "CNPC") 
data_WH <- subset(d_no_fillers, item_sentence_type == "WH") 

data_CNPC$trial_sequence_total <- scale(data_CNPC$trial_sequence_total, center = TRUE, scale = FALSE)
data_CNPC$rating_response <- scale(data_CNPC$rating_response, center = TRUE, scale = FALSE)

data_WH$trial_sequence_total <-scale(data_WH$trial_sequence_total, center = TRUE, scale = FALSE)
data_WH$rating_response <-scale(data_WH$rating_response, center = TRUE, scale = FALSE)



model_all = lmer(rating_response ~ trial_sequence_total * comp + 
                   (1 + trial_sequence_total * comp|workerid) +
                   (1 + trial_sequence_total * comp|item_number),
                 data = d_no_fillers, verbose = 100)

summary(model_all)


#sum-code item_group: 
data_CNPC$item_context_condition <- factor(data_CNPC$item_context_condition, c("SC", "NC"))
contrasts(data_CNPC$item_context_condition) = contr.sum(2)
model_CNPC = lmer(rating_response ~ item_context_condition * trial_sequence_total 
                  + (1 + trial_sequence_total|workerid) + (1 + item_context_condition * trial_sequence_total|item_number),
                  data = data_CNPC)
summary(model_CNPC)


data_WH$item_context_condition <- factor(data_CNPC$item_context_condition, c("SC", "NC"))
contrasts(data_WH$item_context_condition) = contr.sum(2)
model_WH = lmer(rating_response ~ item_context_condition * trial_sequence_total 
                  + (1 + trial_sequence_total|workerid) + (1 + item_context_condition*trial_sequence_total|item_number),
                  data = data_WH)
summary(model_WH)

data$item_context_condition <- factor(data$item_context_condition, c("SC", "NC"))
contrasts(data$item_context_condition) = contr.sum(2)
model = lmer(rating_response ~ item_sentence_type*item_context_condition * trial_sequence_total 
                  + (1 + trial_sequence_total|workerid) + (1 + item_sentence_type*item_context_condition*trial_sequence_total|item_number),
                  data = data)
summary(model)

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

d_no_fillers$item_context_condition <- factor(d_no_fillers$item_context_condition, c("SC", "NC"))
d_no_fillers$item_sentence_type <- factor(d_no_fillers$item_sentence_type, c("CNPC", "WH"))
contrasts(d_no_fillers$item_context_condition) = contr.sum(2)
contrasts(d_no_fillers$item_sentence_type) = contr.sum(2)

d_no_fillers$comp <- as.factor(d_no_fillers$comp)

# main model
model_comp = glmer(comp ~ item_context_condition * item_sentence_type
                        + (1|workerid) + (1+item_context_condition * item_sentence_type|item_number), 
                        data = d_no_fillers, family = "binomial")
summary(model_comp)

model_comp_CNPC = glmer(comp ~ item_context_condition
                   + (1|workerid) + (1+item_context_condition|item_number), 
                   data = data_CNPC, family = "binomial")
summary(model_comp_CNPC)

# WH

model_comp_WH = glmer(comp ~ item_context_condition
                   + (1|workerid) + (1+item_context_condition|item_number), 
                   data = data_WH, family = "binomial")
summary(model_comp_WH)


# second part

model = glmer(comp ~ trial_sequence_total * item_context_condition
              +(1 + trial_sequence_total|workerid),
              data = d_no_fillers, family = "binomial")
summary(model)


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


# CNPC subset
model_CNPC_subset$trial_sequence_total <- scale(model_CNPC_subset$trial_sequence_total, center = TRUE, scale = FALSE)
model_CNPC_subset = lmer(rating_response ~ trial_sequence_total * item_context_condition
              + (1 + trial_sequence_total|workerid)
              + (1 + trial_sequence_total * item_context_condition|item_number),
              data = data_CNPC)
summary(model_CNPC_subset)

# WH subset
model_WH_subset = lmer(rating_response ~ trial_sequence_total * item_context_condition
                          + (1 + trial_sequence_total|workerid) 
                       + (1 + trial_sequence_total * item_context_condition|item_number),,
                          data = data_WH)
summary(model_WH_subset)



# Three centered model
data_for_three_center <- d_no_fillers

data_for_three_center$trial_sequence_total <- scale(data_for_three_center$trial_sequence_total, center = TRUE, scale = FALSE)
contrasts(data_for_three_center$item_sentence_type) = contr.sum(2)
contrasts(data_for_three_center$item_context_condition) = contr.sum(2)

model_three_center = lmer(rating_response ~ trial_sequence_total * item_context_condition * item_sentence_type
                       + (1|workerid),
                       data = data_for_three_center)
summary(model_three_center)

#Dummy coding
data_for_dummy <- d_no_fillers
data_for_dummy$trial_sequence_total <- scale(data_for_dummy$trial_sequence_total, center = TRUE, scale = FALSE)
contrasts(data_for_dummy$item_context_condition) = contr.sum(2)

# Reference CNPC
data_for_dummy <- data_for_dummy %>%
  mutate(dummy_CNPC = if_else(item_sentence_type == "CNPC", 0, 1))

model_dummy_CNPC = lmer(rating_response ~ trial_sequence_total * item_context_condition * dummy_CNPC
                          + (1 + trial_sequence_total|workerid)
                        + (1 + trial_sequence_total + item_context_condition * item_sentence_type|item_number),
                          data = data_for_dummy)
summary(model_dummy_CNPC)



# Reference WH
data_for_dummy <- data_for_dummy %>%
  mutate(dummy_WH = if_else(item_sentence_type == "WH", 0, 1))

model_dummy_WH = lmer(rating_response ~ trial_sequence_total * item_context_condition * dummy_WH
                        + (1 |workerid),
                        data = data_for_dummy)
summary(model_dummy_WH)

#No centering
data_for_no_center <- d_no_fillers


model_no_center = lmer(rating_response ~ trial_sequence_total * item_context_condition * item_sentence_type
                      + (1 |workerid),
                      data = data_for_no_center)
summary(model_no_center)

#dummy CNPC
contrasts(data_for_three_center$item_sentence_type) = contr.sum(2)
data_for_no_center <- data_for_no_center %>%
  mutate(dummy_CNPC = if_else(item_sentence_type == "CNPC", 0, 1))

model_no_center_dummy_CNPC = lmer(rating_response ~ trial_sequence_total * item_context_condition * dummy_CNPC
                                + (1 |workerid),
                                data = data_for_no_center)
summary(model_no_center_dummy_CNPC)

#dummy WH
data_for_no_center <- data_for_no_center %>%
  mutate(dummy_WH = if_else(item_sentence_type == "WH", 0, 1))

model_no_center_dummy_WH = lmer(rating_response ~ trial_sequence_total * item_context_condition * dummy_WH
                      + (1 + trial_sequence_total|workerid),
                      data = data_for_no_center)
summary(model_no_center_dummy_WH)
