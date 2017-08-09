# rm(list=ls())
library(ggplot2)
library(dplyr)

resultsRH.perTrial.all$subject <- as.factor(resultsRH.perTrial.all$subject)

# explore main effects of confidence for different trialTypes
if (1 %in% unique(resultsRH.perTrial.all$trialType)){
    resultsRH.perTrial.all$trialType <- rowSums(cbind(resultsRH.perTrial.all$trialType,0.5 *resultsRH.perTrial.all$RU.Rchosen), na.rm=TRUE)  
  }

subj_meanConfs = aggregate(choiceConf ~ trialType + condition + subject, resultsRH.perTrial.all, mean, drop=TRUE)
# subj_meanConfs <- subset(subj_meanConfs, trialType %in% c("1.5", "2"))


resultsRH.perTrial.all.tosummarize            <- resultsRH.perTrial.all[,c("choiceConf", "trialType", "condition")]
resultsRH.perTrial.all.tosummarize$trialType  <- as.factor(resultsRH.perTrial.all.tosummarize$trialType)
resultsRH.perTrial.all.tosummarize$condition  <- as.factor(resultsRH.perTrial.all.tosummarize$condition)
resultsRH.perTrial.all.tosummarize$choiceConf <- as.numeric(resultsRH.perTrial.all.tosummarize$choiceConf)

# Get densities by hand for the split violin plots 
# you need library(dplyr for this)
group_confDensity <- subj_meanConfs %>%                                              #resultsRH.perTrial.all.tosummarize %>% 
          group_by(trialType, condition) %>%
          do(data.frame(loc  = density(.$choiceConf)$x,                              #bw="nrd0", adjust = .2
                        dens = density(.$choiceConf)$y))

group_confStats <- resultsRH.perTrial.all.tosummarize %>% 
          group_by(trialType, condition) %>%
            summarize(conf_mean = mean(choiceConf),
                conf_se = sqrt(var(choiceConf)/length(choiceConf)))

temp_0 = NULL
for (trial in unique(group_confDensity$trialType)) {
  for (cond in unique(group_confDensity$condition) ) {
    temp = subset(group_confDensity, trialType == trial & condition == cond)
    temp$conf_mean = subset( group_confStats, condition == cond & trialType == trial)$conf_mean
    temp$conf_se = subset( group_confStats, condition == cond & trialType == trial)$conf_se
    temp_0 = rbind(temp_0,temp)
  } 
}

group_confDensity = temp_0

group_confDensity$dens <- ifelse(group_confDensity$condition == "1", group_confDensity$dens * -1, group_confDensity$dens)

group_confDensity$dens[group_confDensity$trialType=="0"]   <- 10*group_confDensity$dens[group_confDensity$trialType=="0"] 
group_confDensity$dens[group_confDensity$trialType=="0.5"] <- 10*group_confDensity$dens[group_confDensity$trialType=="0.5"] + 1
group_confDensity$dens[group_confDensity$trialType=="1.5"] <- 10*group_confDensity$dens[group_confDensity$trialType=="1.5"] + 2
group_confDensity$dens[group_confDensity$trialType=="2"]   <- 10*group_confDensity$dens[group_confDensity$trialType=="2"]   + 3

group_confDensity <- as.data.frame(group_confDensity)

subj_meanConfs$x[subj_meanConfs$trialType=="0"]     <- 0
subj_meanConfs$x[subj_meanConfs$trialType=="0.5"]   <- 1
subj_meanConfs$x[subj_meanConfs$trialType=="1.5"]   <- 2
subj_meanConfs$x[subj_meanConfs$trialType=="2"]     <- 3


subj_meanConfs$x         <- as.numeric(subj_meanConfs$x)
subj_meanConfs$trialType <- as.factor(subj_meanConfs$trialType)
subj_meanConfs$condition <- as.factor(subj_meanConfs$condition)


save(group_confDensity, file = paste(fullPath,'/data/toPlot_group_confDensity.Rda', sep=""))
save(subj_meanConfs,    file = paste(fullPath,'/data/toPlot_subj_meanConfs.Rda', sep=""))


#' ## Plot split violin plots + scatter
p_splitViolins <- ggplot(group_confDensity, aes(dens, loc, fill = condition, group = interaction(condition, trialType))) + 
  geom_polygon() +
  scale_x_continuous(breaks = 0:3, labels = c('UU trials', 'RU(U) trials', 'RU(R) trials', 'RR trials')) +
  # scale_x_continuous(breaks = 2:3, labels = c('RU+ trials', 'RR trials')) +
  #scale_fill_brewer(palette = "Greys", guide=FALSE) +
  scale_fill_manual(values = c("#909090", "#A0A0A0"), guide=FALSE) +
  ylab('density') +
  theme(axis.title.x = element_blank()) +
  geom_jitter(data = subj_meanConfs,
        aes(x, y = choiceConf, shape = condition, group = interaction(condition, trialType)),
        alpha = 0.5, size = 1, #shape = 21, #colour = "black", 
        position = position_jitterdodge(), #width = 0.25, 
        show.legend = FALSE) + 
  scale_shape_manual(name = "Condition", values=c(19,1), labels = c("Population", "Distance")) +  
  # scale_fill_discrete(name="Condition",
  #                  labels=c("Population", "Distance")) +
  xlab("Trial type") +
  ylab("Mean Individual Confidence")  +
  annotate("segment", x = 1.75, y = 100, xend  = 2.25,  yend = 100) +
  annotate("text",    x = 2,    y = 101, label = "*",   size = 7)  +
  annotate("segment", x = 2,    y = 110, xend  = 3,     yend = 110) +
  annotate("text",    x = 2.5,  y = 111, label = "*",   size = 7)  +
  ggtitle("D.                                                           ")



