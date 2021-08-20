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