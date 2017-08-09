#' ### Get and plot mean confidence vs α in each condition 

library(ggplot2)
library(BayesFactor)
library(gridExtra)

source(paste(fullPath, "/functions/multiplot.R", sep=""))

load(paste(fullPath,'/data/modelresults.Rda', sep=""))
load(paste(fullPath,'/data/modelresults_long.Rda', sep=""))
load(paste(fullPath,'/data/resultsRH_perTrial.Rda', sep=""))


resultsRH.perTrial.all$trialType <- rowSums(cbind(resultsRH.perTrial.all$trialType,0.5 *resultsRH.perTrial.all$RU.Rchosen), na.rm=TRUE)


meanConfPerSubj <- ddply(resultsRH.perTrial.all, c("trialType", "condition", "subject"), summarise,
                          meanConf = mean(choiceConf)
                        )
meanConfPerSubj.RUplus      <- subset(meanConfPerSubj, trialType %in% c(1.5))
meanConfPerSubj.RUplus.pop  <- subset(meanConfPerSubj.RUplus, condition %in% c(1)) 
meanConfPerSubj.RUplus.dist <- subset(meanConfPerSubj.RUplus, condition %in% c(2)) 

RU.Delta <- data.frame(meanConf_RH = meanConfPerSubj.RUplus.pop$meanConf - meanConfPerSubj.RUplus.dist$meanConf,
                       alpha       = modelResults$alpha.1                - modelResults$alpha.2,
                       ID          = modelResults$ID)

RU <- data.frame(alpha = modelResults.long$alpha,
                 condition = modelResults.long$condition,
                 meanConfRH = c(meanConfPerSubj.RUplus.pop$meanConf, meanConfPerSubj.RUplus.dist$meanConf)
                 )

p_meanConf_Alpha <- ggplot(
          RU, aes(x = alpha, y = meanConfRH)) +
          geom_point(aes(shape=factor(condition)), alpha = 1) + 
#          scale_shape_manual(name = "Condition", values=c(19,1), labels = c("Population", "Distance")) + 
          scale_shape_manual(name = "Condition", values=c(19,1), guide=FALSE) + 
          xlim(0, 1) +
          labs(x = expression(alpha), y = "Mean individual confidence") +
          # theme(legend.position=c(.85,.85),
          #   legend.key = element_rect(fill = NA, colour = NA)) +
          ggtitle("A.                                                      ")


#' ### Plot confidence difference vs α difference in each condition 
p_meanConfDiff_AlphaDiff <- ggplot(RU.Delta, aes(x = alpha, y = meanConf_RH)) +
          geom_point(shape = 17) +  
          #xlim(0,.75) +
          labs(x = expression(Delta*alpha), y = expression(Delta*" Mean individual confidence")) +
          ggtitle("B.                                                      ") 



load(file = paste(fullPath,'/data/toPlot_group_confDensity.Rda', sep=""))
load(file = paste(fullPath,'/data/toPlot_subj_meanConfs.Rda', sep=""))


source(paste(fullPath,'/functions/summarySE.R', sep=""))

descriptives.MeanConfs <- summarySE(subj_meanConfs, measurevar="choiceConf", groupvars=c("condition", "trialType"))
is.factor(descriptives.MeanConfs$trialType)
descriptives.MeanConfs$trialType <- as.numeric(as.character(descriptives.MeanConfs$trialType))
descriptives.MeanConfs$trialType[descriptives.MeanConfs$trialType == 0]   <- 0
descriptives.MeanConfs$trialType[descriptives.MeanConfs$trialType == 0.5] <- 1
descriptives.MeanConfs$trialType[descriptives.MeanConfs$trialType == 2]   <- 3
descriptives.MeanConfs$trialType[descriptives.MeanConfs$trialType == 1.5] <- 2
mean.pop.UU   <- descriptives.MeanConfs$choiceConf[descriptives.MeanConfs$condition == 1 & descriptives.MeanConfs$trialType == 0]
mean.dist.UU  <- descriptives.MeanConfs$choiceConf[descriptives.MeanConfs$condition == 2 & descriptives.MeanConfs$trialType == 0]
mean.pop.RUm  <- descriptives.MeanConfs$choiceConf[descriptives.MeanConfs$condition == 1 & descriptives.MeanConfs$trialType == 1]
mean.dist.RUm <- descriptives.MeanConfs$choiceConf[descriptives.MeanConfs$condition == 2 & descriptives.MeanConfs$trialType == 1]
mean.pop.RUp  <- descriptives.MeanConfs$choiceConf[descriptives.MeanConfs$condition == 1 & descriptives.MeanConfs$trialType == 2]
mean.dist.RUp <- descriptives.MeanConfs$choiceConf[descriptives.MeanConfs$condition == 2 & descriptives.MeanConfs$trialType == 2]
mean.pop.RR   <- descriptives.MeanConfs$choiceConf[descriptives.MeanConfs$condition == 1 & descriptives.MeanConfs$trialType == 3]
mean.dist.RR  <- descriptives.MeanConfs$choiceConf[descriptives.MeanConfs$condition == 2 & descriptives.MeanConfs$trialType == 3]

group_confDensity$condition <- as.factor(group_confDensity$condition)

#' ## Plot split violin plots + scatter
p_splitViolins <- ggplot(group_confDensity, aes(dens, loc, fill = condition, group = interaction(condition, trialType))) + 
  geom_polygon() +
  scale_x_continuous(breaks = 0:3, labels = c('UU trials', 'RU- trials', 'RU+ trials', 'RR trials')) +
  #scale_fill_brewer(palette = "Greys", guide=FALSE) +
  scale_fill_manual(values = c("#708090", "#C0C0C0"), name = "Condition", labels = c("Population", "Distance")) +
  ylab('density') +
  theme(axis.title.x = element_blank()) +
  geom_jitter(data = subj_meanConfs,
        aes(x, y = choiceConf, shape = condition, group = interaction(condition, trialType)),
        alpha = 0.5, size = 1, #shape = 21, #colour = "black", 
        position = position_jitterdodge(), #width = 0.25, 
        show.legend = TRUE) +
  scale_shape_manual(name = "Condition", values=c(19,1), labels = c("Population", "Distance")) +  
  xlab("Trial type") +
  ylab("Mean Individual Confidence")  +
  annotate("segment", x = 1.75, y = 100, xend  = 2.25,  yend = 100) +
  annotate("text",    x = 2,    y = 101, label = "*",   size = 7)  +
  # annotate("segment", x = 2,    y = 110, xend  = 3,     yend = 110) +
  # annotate("text",    x = 2.5,  y = 111, label = "*",   size = 7)  +
  ggtitle("C.                                                                                                 ") +
  annotate("segment", x = -0.4, y = mean.pop.UU,   xend  = -0.05, yend = mean.pop.UU,   size = 1) +
  annotate("segment", x = 0.05, y = mean.dist.UU,  xend  =  0.4,  yend = mean.dist.UU,  size = 1) +
  annotate("segment", x = 0.6,  y = mean.pop.RUm,  xend  = 0.95,  yend = mean.pop.RUm,  size = 1) +
  annotate("segment", x = 1.05, y = mean.dist.RUm, xend  = 1.4,   yend = mean.dist.RUm, size = 1) +
  annotate("segment", x = 1.6,  y = mean.pop.RUp,  xend  = 1.95,  yend = mean.pop.RUp,  size = 1) +
  annotate("segment", x = 2.05, y = mean.dist.RUp, xend  = 2.4,   yend = mean.dist.RUp, size = 1) +
  annotate("segment", x = 2.6,  y = mean.pop.RR,   xend  = 2.95,  yend = mean.pop.RR,   size = 1) +
  annotate("segment", x = 3.05, y = mean.dist.RR,  xend  = 3.4,   yend = mean.dist.RR,  size = 1)

# The right way to do it is something like this, but I just can't get it right. Added means by hand, aware that it's a bad strategy!
  # geom_point(data = descriptives.MeanConfs, aes(colour=condition, y=choiceConf, x=trialType), position=position_jitterdodge(0.5), size = 5) + 
  # geom_errorbar(data = descriptives.MeanConfs, aes(ymax = choiceConf + se, ymin = choiceConf - se, group = interaction(condition, trialType)), width=0.2)



grid.arrange(p_meanConf_Alpha, p_meanConfDiff_AlphaDiff, p_splitViolins, layout_matrix = rbind(c(1,2),c(3,3)))
