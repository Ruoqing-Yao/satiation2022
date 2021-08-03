d <- read.csv("../data/exp1_cleaned.csv")
d$item_type <- factor(d$item_type, levels = c("FILL", "WH","SUBJ","UNGRAM"))
d$phase <- factor(d$phase, levels = c("pre-exposure", "exposure", "test"))

d2 <- read.csv("../../pilot/data/pilot1_cleaned.csv")
d2$item_type <- factor(d2$item_type, levels = c("FILL", "WH","SUBJ","UNGRAM"))
d2$phase <- factor(d2$phase, levels = c("pre-exposure", "exposure", "test"))

d <- rbind(d, d2)

# split data by test condition
d <- subset(d, test_condition == "WH")

d_no_ungram <- subset(d, item_type != "UNGRAM")
d_no_fillers <- subset(d_no_ungram, item_type != "FILL")
d_no_fillers$item_type <- factor(d_no_fillers$item_type, c("SUBJ", "WH"))
d_no_fillers$exposure_condition <- factor(d_no_fillers$exposure_condition, c("SUBJ", "WH"))


d_pre_post <- subset(d_no_fillers, phase != "exposure")
# set reference levels
d_pre_post$phase <- factor(d_pre_post$phase, levels = c("pre-exposure", "test"))
d_pre_post$exposure_condition <- factor(d_pre_post$exposure_condition, c("SUBJ", "WH"))
phase_model <- lmer(
  response ~ phase * exposure_condition +
    (1 + phase | workerid) +
    (1 + exposure_condition*phase | item_number),
  data = d_pre_post,
  # verbose = 100
)

summary(phase_model)

cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")

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
  scale_fill_manual(values=cbPalette, guide="none") +
  scale_alpha_manual(values=c(0.6,1)) +
  xlab("exposure type") +
  ylab("average acceptability of whether islands") +
  theme_bw() 

phase_graph

trial_means = d_no_fillers %>%
  group_by(exposure_condition, phase, item_type,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()
trial_means$exposure_condition <- factor(trial_means$exposure_condition, levels=c("WH", "SUBJ"))
trial_means$item_type <- factor(trial_means$item_type, levels=c("WH", "SUBJ"))
curve_split <- ggplot(d_no_fillers, aes(x=trial_sequence_total, y=response, color = item_type)) +
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


curve_split

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
  scale_fill_manual(name="exposure condition", values=cbPalette, guide="none") +
  scale_shape(name="exposure condition") +
  theme_bw()