cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")

d <- read.csv("../data/exp1_cleaned.csv")
d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ","UNGRAM"))
d$phase <- factor(d$phase, levels = c("pre-exposure", "exposure", "test"))

d = subset(d, block_sequence != "practice")
#d <- subset(d, group == "mismatch")

#d <- subset(d, test_condition == "WH")



####################################
# overall satiation plot
####################################

nd = d %>%
  mutate(item_type = fct_recode(item_type,"grammatical"="FILL","ungrammatical"="UNGRAM", "subject island"="SUBJ", "whether island"="WH")) %>% 
  #, "complex-NP island"="CNPC", "whether island"="WH")) %>%
  mutate(item_type = fct_relevel(item_type,"grammatical","whether island","subject island", "ungrammatical")) %>%
  mutate(exposure_condition = fct_recode(exposure_condition,"subject island"="SUBJ", "whether island"="WH")) 
#complex-NP island","subject island", "ungrammatical"))

trial_means = nd %>%
  group_by(exposure_condition,item_type,phase,group,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()

#data$phase2[as.integer(data$trial_sequence_total) <= 6 & data$block_sequence != "practice"] <- "pre-exposure"
trial_means$condition <- ""
trial_means$condition[trial_means$exposure_condition == "whether island" & trial_means$group=="match"] <- "whether islands" 
trial_means$condition[trial_means$exposure_condition == "subject island" & trial_means$group=="match"] <- "subject islands" 
trial_means$condition[trial_means$exposure_condition == "whether island" & trial_means$group=="mismatch"] <- "subject islands" 
trial_means$condition[trial_means$exposure_condition == "subject island" & trial_means$group=="mismatch"] <- "whether islands" 
trial_means$condition[trial_means$item_type == "grammatical" | trial_means$item_type=="ungrammatical"] <- "fillers"


trial_means = trial_means %>%
  group_by(condition, phase, item_type, group, trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()

#trial_means <- subset(trial_means, condition=="whether islands")

# trial_means <- subset(trial_means, condition=="fillers")
# trial_means <- subset(trial_means, condition=="fillers" | (condition == "whether islands" & group == "match"))
# trial_means <- subset(trial_means, condition=="fillers" | (condition == "whether islands" & group == "match") | (group=="mismatch" & phase=="pre-exposure"))
# trial_means <- subset(trial_means, condition=="fillers" | (condition == "whether islands" & group == "match") | (group=="mismatch" & phase!="test"))
# trial_means <- subset(trial_means, condition=="fillers" | condition == "whether islands")
# trial_means <- subset(trial_means, !(condition == "subject islands" & group == "mismatch"))
# trial_means <- subset(trial_means, !(condition == "subject islands" & group == "mismatch") | (group=="mismatch" & phase=="pre-exposure"))
# trial_means <- subset(trial_means, !(condition == "subject islands" & group == "mismatch") | (group=="mismatch" & phase!="test"))
# trial_means <- subset(trial_means, !(condition == "subject islands" & group == "mismatch")| (group=="mismatch"))


trial_means$condition <- factor(trial_means$condition, levels = c("fillers", "whether islands", "subject islands"))

cbPalette = c("#e69d00", "#009e74","#d55e00",  "#cc79a7", "#0071b2")

ggplot(nd, aes(x=trial_sequence_total, y=response, color = condition, fill=condition, linetype=group)) +
  geom_point(data=trial_means,alpha=.9) +
  xlab("trial sequence") +
  ylab("average acceptability")+
  
  
  # fillers
  geom_smooth(data=subset(trial_means, item_type == "grammatical" ),method=lm) +
  geom_smooth(data=subset(trial_means, item_type == "ungrammatical"),method=lm) +
  
  geom_smooth(data=subset(trial_means, condition != "fillers" & phase == "pre-exposure"), method=lm) +
  geom_smooth(data=subset(trial_means, condition != "fillers" & phase == "exposure"), method=lm) +
  geom_smooth(data=subset(trial_means, condition != "fillers" & phase == "test"), method=lm) +
  
  geom_vline(xintercept=12.5, linetype="dashed",
             size=0.5)+
  geom_vline(xintercept=52.5, linetype="dashed",
             size=0.5)+
  scale_color_manual(name="test item type", values=cbPalette) +
  scale_fill_manual(name="test item type", values=cbPalette) +
  scale_linetype(name="experiment group", labels=c("within-category", "between-category")) +
  theme_bw()

# ggsave("~/csli/presentation/exp1_overall.png",width=10,height=5)

######################
# bar plot
######################

d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("WH", "SUBJ"))
#d_no_fillers$exposure_condition <- factor(d_no_fillers$exposure_condition, c("SUBJ", "WH"))

d_no_fillers <- subset(d_no_fillers, group=="match")
# d_no_fillers <- subset(d_no_fillers, exposure_condition=="WH")

cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")
cbPalette = c("#009e74","#d55e00","#e69d00","#cc79a7", "#0071b2")

theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}

phase_avg = d_no_fillers %>%
  group_by(phase,exposure_condition, item_type) %>%
  mutate(exposure_condition = fct_recode(exposure_condition,"subject island"="SUBJ", "whether island"="WH")) %>% 
  mutate(item_type = fct_recode(item_type,"subject island"="SUBJ", "whether island"="WH")) %>% 
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

phase_avg = subset(phase_avg, phase != "exposure")
phase_graph<- ggplot(phase_avg, aes(x=item_type,y=Mean, fill=item_type)) +
  geom_bar(data=phase_avg, stat="identity", position="dodge", aes(alpha=phase)) +
  geom_errorbar(aes(ymin=CILow,ymax=CIHigh), position=position_dodge2(width=0.2, padding=0.5)) +
  scale_fill_manual(name="item type", values=cbPalette) +
  scale_alpha_manual(values=c(0.6,1), labels=c("pre-exposure", "post-exposure")) +
  xlab("test item type") +
  ylab("average acceptability") +
  theme_bw() 

phase_graph
ggsave("~/csli/presentation/exp1_bar_within_2.png",width=10,height=5)

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


trial_avg$grp <- factor(trial_avg$grp, levels = c("all conditions", "whether islands", "subject islands", "subject to whether", "whether to subject"))

#cum_average plot
cum_avg_plt=ggplot(trial_avg, aes(x=trial, y=cum, color=grp)) +
  
  geom_smooth (se = F) + 
  geom_point()+
  
  xlab("Trial number") +
  ylab("Cumulative average acceptability rating")+
  geom_hline(yintercept=0.5, linetype="dashed",
             size=1)+
  theme( plot.margin = margin(0, 0, 0, 0, "cm"))+
  scale_color_manual(name="Condition", values=cbPalette) +
  theme_bw()
# cum_avg_plt
