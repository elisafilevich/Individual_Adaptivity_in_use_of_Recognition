
# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


library(lsr)        # compute effect sizes (cohensD) in ttests
library(R.matlab)
library(plyr)       # to summarize data and required before dplyr
library(dplyr)      # for renaming columns 
library(reshape)    # melt and cast functions
source(paste(fullPath,"/functions/as.data.frame.list.R", sep=""))
library(BayesFactor)

# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Criterion knowledge

# Melt for ggplot2
criterionKnowledge_toPlot <- resultsRH.perSubj[,c("rankingRhoRecogOnly_pop", "rankingRhoRecogOnly_dist",
												                          "rankingRho_pop", 		     "rankingRho_dist")]
criterionKnowledge_toPlot <- melt(criterionKnowledge_toPlot)

# Just some renaming for convenience 
# assign condition ID
criterionKnowledge_toPlot$condition[criterionKnowledge_toPlot$variable == "rankingRhoRecogOnly_pop"] 	<- "pop"
criterionKnowledge_toPlot$condition[criterionKnowledge_toPlot$variable == "rankingRho_pop"] 			    <- "pop"
criterionKnowledge_toPlot$condition[criterionKnowledge_toPlot$variable == "rankingRhoRecogOnly_dist"] <- "dist"
criterionKnowledge_toPlot$condition[criterionKnowledge_toPlot$variable == "rankingRho_dist"] 			    <- "dist"
criterionKnowledge_toPlot$condition <- as.factor(criterionKnowledge_toPlot$condition)
# assign "Recognized only" ID
criterionKnowledge_toPlot$Ronly[criterionKnowledge_toPlot$variable == "rankingRhoRecogOnly_pop"] 	    <- "Ronly"
criterionKnowledge_toPlot$Ronly[criterionKnowledge_toPlot$variable == "rankingRho_pop"] 			        <- "allcities"
criterionKnowledge_toPlot$Ronly[criterionKnowledge_toPlot$variable == "rankingRhoRecogOnly_dist"] 	  <- "Ronly"
criterionKnowledge_toPlot$Ronly[criterionKnowledge_toPlot$variable == "rankingRho_dist"] 			        <- "allcities"
criterionKnowledge_toPlot$Ronly <- as.factor(criterionKnowledge_toPlot$Ronly)

library(ggplot2)
ggplot(criterionKnowledge_toPlot, aes(x = condition, y = value, color = condition)) +
		geom_jitter() +
		facet_grid(. ~ Ronly) +
		ylab("criterionKnowledge_toPlot")


rhoCritKnow.mean <- aggregate(value ~ condition + Ronly, data = criterionKnowledge_toPlot, mean)
rhoCritKnow.mean
rhoCritKnow.sd   <- meanVals <- aggregate(value ~ condition + Ronly, data = criterionKnowledge_toPlot, sd)
rhoCritKnow.sd

#' ## Stats on criterion Knowledge 
#' ### Permutation test for the recognized-only cities
library(exactRankTests)
perm.test(resultsRH.perSubj$rankingRhoRecogOnly_pop, resultsRH.perSubj$rankingRhoRecogOnly_dist, paired=TRUE, alternative="two.sided")

#' ### BF
rhoCritKnow.bf = ttestBF(x = resultsRH.perSubj$rankingRhoRecogOnly_pop,
			 			 y = resultsRH.perSubj$rankingRhoRecogOnly_dist, paired=TRUE)
rhoCritKnow.bf

#' ### Permutation test for the all cities
perm.test(resultsRH.perSubj$rankingRho_pop, resultsRH.perSubj$rankingRho_dist, paired=TRUE, alternative="two.sided")

#' ### BF
rhoCritKnow.bf = ttestBF(x = resultsRH.perSubj$rankingRho_pop,
             y = resultsRH.perSubj$rankingRho_dist, paired=TRUE)
rhoCritKnow.bf


# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Recognition Rate and Applicability of the RH

recognized.percent.mean <- mean(resultsRH.perSubj$totalRecognized/15*100)
recognized.percent.mean
recognized.percent.sd   <- sd(resultsRH.perSubj$totalRecognized/15*100)
recognized.percent.sd

#' ## Proportion of RU trials in the Comparative Judgment Task
resultsRH.perTrial.all$subject <- as.factor(resultsRH.perTrial.all$subject)
trialProportion <- resultsRH.perTrial.all[resultsRH.perTrial.all$condition==1,] %>% #only one condition is necessary. It's the same for both
    group_by(subject) %>% 
    summarise(
      UUtrials  = length(which(trialType==0)),
      RUtrials  = length(which(trialType==1)),
      RRtrials  = length(which(trialType==2))
      )
trialProportion <- as.data.frame(trialProportion)

# Check output: should all be 70 (sum all columns except subject)
all(rowSums(trialProportion[,!names(trialProportion) %in% c("subject")]) == 70)

# Check output: should all be 100 (sum all columns except subject)
trialProportion.percentage <- trialProportion[,!names(trialProportion) %in% c("subject")]/70*100
all(round(rowSums(trialProportion.percentage[,!names(trialProportion.percentage) %in% c("subject")])) == 100)

mean(trialProportion.percentage$RUtrials)
sd(trialProportion.percentage$RUtrials)

#' ### Plot all subjs' trial proportions
trialProportion.sorted <- trialProportion[order(trialProportion$UUtrials),]
trialProportion.sorted$subj.sorted <- c(1:dim(trialProportion.sorted)[1])
trialProportion.sorted <- reshape2:::melt.data.frame(trialProportion.sorted,  id.vars=c("subj.sorted"),    #explicitly call melt from reshape2 otherwise coumns might not get properly named
                                                        measure.vars = c("UUtrials", "RUtrials", "RRtrials"),
                                                        variable.name = "trialType",
                                                        value.name = "trialCount")
ggplot(trialProportion.sorted, aes(x = subj.sorted, y = trialCount, fill=trialType)) +
    geom_bar(stat='identity')


# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' # (Performance in the) Comparative Judgment Task 


#' ## Population vs distance
# Group by subject
proportionCorrect.perSubj <- resultsRH.perTrial.all %>% 
    group_by(subject, condition) %>% 
    summarise(
      correct  = mean(correct, na.rm = TRUE)
      )

proportionCorrect.perSubj <- as.data.frame(proportionCorrect.perSubj)
proportionCorrect.perSubj$condition[proportionCorrect.perSubj$condition == 1] <- "pop"
proportionCorrect.perSubj$condition[proportionCorrect.perSubj$condition == 2] <- "dist"

proportionCorrect.perSubj.wide <- reshape(proportionCorrect.perSubj, timevar=c("condition"), 
                  idvar=c("subject"), dir="wide")

#' ### ttest
t.test(proportionCorrect.perSubj.wide$correct.pop, proportionCorrect.perSubj.wide$correct.dist, paired = TRUE)
cohensD(proportionCorrect.perSubj.wide$correct.pop, proportionCorrect.perSubj.wide$correct.dist, method="paired")

#' ### BF
proportionCorrect.bf = ttestBF(x = proportionCorrect.perSubj.wide$correct.pop,
                               y = proportionCorrect.perSubj.wide$correct.dist, paired=TRUE)
proportionCorrect.bf


#' ## Compare population vs. distance
#' ### alphas
t.test(modelResults$alpha.1, modelResults$alpha.2, paired=TRUE, var.equal=TRUE)  #Because of renaming, 1 is population and 2 is distance
cohensD(modelResults$alpha.1, modelResults$alpha.2, method = "paired") 
ttestBF(x = modelResults$alpha.1, y = modelResults$alpha.2, paired=TRUE)

#' ### r parameters 
t.test(modelResults$r_param.1, modelResults$r_param.2, paired=TRUE, var.equal=TRUE)
cohensD(modelResults$r_param.1, modelResults$r_param.2, method = "paired") 
ttestBF(x = modelResults$r_param.1, y = modelResults$r_param.2, paired=TRUE)

#' ### b parameters
t.test(modelResults$b_param.1, modelResults$b_param.2, paired=TRUE, var.equal=TRUE)  #Because of renaming, 1 is population and 2 is distance
cohensD(modelResults$b_param.1, modelResults$b_param.2, method = "paired") 


# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#' # Formal Modeling: Adaptive Use of the RH 

#' This section is not included here

# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Metacognitive Sensitivity 

#' Do a glm approach, as described my Siedlecka et al (2016, Front Psychol)
library(lme4)
library(lmerTest)

# Prepare the dataset
# Separate between RU+ and RU-
resultsRH.perTrial.all$trialType <- rowSums(cbind(resultsRH.perTrial.all$trialType,0.5 *resultsRH.perTrial.all$RU.Rchosen), na.rm=TRUE)
# Check output
unique(resultsRH.perTrial.all$trialType)

# Set factors
resultsRH.perTrial.all$trialType <- as.factor(resultsRH.perTrial.all$trialType)
resultsRH.perTrial.all$condition <- as.factor(resultsRH.perTrial.all$condition)

# Add the sorted cityPair
# But be careful with this, because city pair will be (partially) confounded with condition
twoCities <- resultsRH.perTrial.all[,c("city1", "city2")]
twoCities <- as.data.frame(t(apply(twoCities, 1, sort))) #Sort the two cities in alphabetical order (so no matter how they were shown on the screen)
resultsRH.perTrial.all$cityPair <- as.factor(paste(twoCities$V1,twoCities$V2,sep=""))


# Get only RU+ trials 
myDataForglm.RU                   <- subset(resultsRH.perTrial.all, trialType %in% c("1.5")) 
myDataForglm.RU                   <- as.data.frame(myDataForglm.RU)
myDataForglm.RU$trialType         <- factor(myDataForglm.RU$trialType)         
myDataForglm.RU$choiceConf.scaled <- scale(myDataForglm.RU$choiceConf)

#' ## Do the models on RU+ trials only
glmres.RU.conf.full = glmer(correct ~ choiceConf.scaled * condition + (1 + choiceConf.scaled|subject)  + (1|cityPair), data = myDataForglm.RU, family = binomial)
glmres.RU.conf.sum  = glmer(correct ~ choiceConf.scaled + condition + (1 + choiceConf.scaled|subject)  + (1|cityPair), data = myDataForglm.RU, family = binomial)

# The full model fails to converge but the subsequent tests show that the obtained result is actually reasonable
source(system.file("utils", "allFit.R", package="lme4"))
fm1.all <- allFit(glmres.RU.conf.full)
ss <- summary(fm1.all)
ss$fixef

#' ### χ^2 
interactionModelComp <- anova(glmres.RU.conf.full,glmres.RU.conf.sum) 
interactionModelComp
#' ### BF10 
interactionModelComp.BF10 = 1/exp((interactionModelComp[3][[1]][2] - interactionModelComp[3][[1]][1])/2) #BF10
interactionModelComp.BF10

# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Individual confidence ratings 

meanConfPerSubj <- ddply(resultsRH.perTrial.all, c("trialType", "condition", "subject"), summarise,
                          meanConf = mean(choiceConf)
                        )
meanConfPerSubj.RUplus      <- subset(meanConfPerSubj, trialType %in% c(1.5))
meanConfPerSubj.RUplus.pop  <- subset(meanConfPerSubj.RUplus, condition %in% c(1)) 
meanConfPerSubj.RUplus.dist <- subset(meanConfPerSubj.RUplus, condition %in% c(2)) 


#' ## α and mean confidence in RU+ 
forBF.alphaVsConf <- data.frame(alpha.pop            = modelResults$alpha.1,
                                alpha.dist           = modelResults$alpha.2,
                                meanConf.RUplus.pop  = meanConfPerSubj.RUplus.pop$meanConf,
                                meanConf.RUplus.dist = meanConfPerSubj.RUplus.dist$meanConf,
                                ID                   = modelResults$ID)

# ### population
cor.test(forBF.alphaVsConf$alpha.pop, forBF.alphaVsConf$meanConf.RUplus.pop)
lmBF(meanConf.RUplus.pop ~ alpha.pop + ID, data = forBF.alphaVsConf)

# ### distance
cor.test(forBF.alphaVsConf$alpha.dist, forBF.alphaVsConf$meanConf.RUplus.dist)
lmBF(meanConf.RUplus.dist ~ alpha.dist + ID, data = forBF.alphaVsConf)

#' ## Correlation between Δconfidence and Δα: (∆pop-dist)
RU.Delta <- data.frame(meanConf_RH = meanConfPerSubj.RUplus.pop$meanConf - meanConfPerSubj.RUplus.dist$meanConf,
                       alpha       = modelResults$alpha.1                - modelResults$alpha.2,
                       ID          = modelResults$ID)

cor.test(RU.Delta$alpha, RU.Delta$meanConf_RH)
# plot(RU.Delta$alpha, RU.Delta$meanConf_RH)
lmBF(meanConf_RH ~ alpha + ID, data = RU.Delta)


# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Mean confidence ratings

#' ## ANOVA with all trial types: RU+, RU-  (4x2)
#' 3x2 anova on mean confidence with all trial types (RR, RUplus, RUminus, UU) * condition
#  Get means
myDataForglm.RUp.RUm.RR.UU                   <- subset(resultsRH.perTrial.all, trialType %in% c("0", "0.5", "1.5", "2")) 
myDataForglm.RUp.RUm.RR.UU                   <- as.data.frame(myDataForglm.RUp.RUm.RR.UU)
myDataForglm.RUp.RUm.RR.UU$trialType         <- factor(myDataForglm.RUp.RUm.RR.UU$trialType)        
myDataForglm.RUp.RUm.RR.UU$choiceConf.scaled <- scale(myDataForglm.RUp.RUm.RR.UU$choiceConf)
myDataForglm.RUp.RUm.RR.UU.mean              <- aggregate(choiceConf ~ subject + condition + trialType, myDataForglm.RUp.RUm.RR.UU, mean)


#Now look for subjects that did not have any RU- or no UU trials and remove them
yesUU.pop   <- unique(myDataForglm.RUp.RUm.RR.UU.mean$subject[myDataForglm.RUp.RUm.RR.UU.mean$trialType == 0   & myDataForglm.RUp.RUm.RR.UU.mean$condition == 1])
yesUU.dist  <- unique(myDataForglm.RUp.RUm.RR.UU.mean$subject[myDataForglm.RUp.RUm.RR.UU.mean$trialType == 0   & myDataForglm.RUp.RUm.RR.UU.mean$condition == 2])
yesRUm.pop  <- unique(myDataForglm.RUp.RUm.RR.UU.mean$subject[myDataForglm.RUp.RUm.RR.UU.mean$trialType == 0.5 & myDataForglm.RUp.RUm.RR.UU.mean$condition == 1])
yesRUm.dist <- unique(myDataForglm.RUp.RUm.RR.UU.mean$subject[myDataForglm.RUp.RUm.RR.UU.mean$trialType == 0.5 & myDataForglm.RUp.RUm.RR.UU.mean$condition == 2])
yesRUp.pop  <- unique(myDataForglm.RUp.RUm.RR.UU.mean$subject[myDataForglm.RUp.RUm.RR.UU.mean$trialType == 1.5 & myDataForglm.RUp.RUm.RR.UU.mean$condition == 1])
yesRUp.dist <- unique(myDataForglm.RUp.RUm.RR.UU.mean$subject[myDataForglm.RUp.RUm.RR.UU.mean$trialType == 1.5 & myDataForglm.RUp.RUm.RR.UU.mean$condition == 2])
yesRR       <- unique(myDataForglm.RUp.RUm.RR.UU.mean$subject[myDataForglm.RUp.RUm.RR.UU.mean$trialType == 2])
noUU.pop    <- as.numeric(setdiff(yesRR,yesUU.pop))
noUU.dist   <- as.numeric(setdiff(yesRR,yesUU.dist))
noRUm.pop   <- as.numeric(setdiff(yesRR,yesRUm.pop))
noRUm.dist  <- as.numeric(setdiff(yesRR,yesRUm.dist))
noRUp.pop   <- as.numeric(setdiff(yesRR,yesRUp.pop))
noRUp.dist  <- as.numeric(setdiff(yesRR,yesRUp.dist))
missings <- unique(c(noUU.pop, noUU.dist, noRUm.pop, noRUm.dist, noRUp.pop, noRUp.dist))
myDataForglm.RUp.RUm.RR.UU.mean$subject[myDataForglm.RUp.RUm.RR.UU.mean$subject %in% missings]  <- NA
myDataForglm.RUp.RUm.RR.UU.mean <- myDataForglm.RUp.RUm.RR.UU.mean[complete.cases(myDataForglm.RUp.RUm.RR.UU.mean),]

factornames <- c("subject", "condition", "trialType")
myDataForglm.RUp.RUm.RR.UU.mean[factornames] <- lapply(myDataForglm.RUp.RUm.RR.UU.mean[factornames], as.factor) 

fit.meanConfs.RUp.RUm.RR.UU <- aov(
            choiceConf ~ (condition * trialType) + 
            Error(subject/(condition * trialType)),
            data = myDataForglm.RUp.RUm.RR.UU.mean)
summary(fit.meanConfs.RUp.RUm.RR.UU)

#' ### ANOVA effect sizes
library(DescTools)
EtaSq(fit.meanConfs.RUp.RUm.RR.UU, type=1, anova=TRUE)



#' ## ANOVA with all trial types: RU regardless of +/- (3x2)
#' 3x2 anova on mean confidence with all trial types (RR, RU, UU) * condition
#  Get means
myDataForglm.RU.RR.UU                   <- subset(resultsRH.perTrial.all, trialType %in% c("0", "1.5", "2")) 
myDataForglm.RU.RR.UU                   <- as.data.frame(myDataForglm.RU.RR.UU)
myDataForglm.RU.RR.UU$trialType         <- factor(myDataForglm.RU.RR.UU$trialType)        
myDataForglm.RU.RR.UU$choiceConf.scaled <- scale(myDataForglm.RU.RR.UU$choiceConf)

#' ## ANOVA only with RR, RU+ 
myDataForglm.RU.RR.UU.mean <- aggregate(choiceConf ~ subject + condition + trialType, myDataForglm.RU.RR.UU, mean)

#controlcount <- aggregate(choiceConf ~ subject + condition + trialType, myDataForglm.RU.RR.UU, length)
#Subject 45 did not have any UU trials in one of the conditions. So I remove it 

myDataForglm.RU.RR.UU.mean$choiceConf[myDataForglm.RU.RR.UU.mean$subject == 45] <- NA 
myDataForglm.RU.RR.UU.mean <- myDataForglm.RU.RR.UU.mean[complete.cases(myDataForglm.RU.RR.UU.mean),]
factornames <- c("subject", "condition", "trialType")
myDataForglm.RU.RR.UU.mean[factornames] <- lapply(myDataForglm.RU.RR.UU.mean[factornames], as.factor) 

fit.meanConfs.RU.RR.UU <- aov(
            choiceConf ~ (condition * trialType) + 
            Error(subject/(condition * trialType)),
            data = myDataForglm.RU.RR.UU.mean)
summary(fit.meanConfs.RU.RR.UU)

#' ### ANOVA effect sizes
library(DescTools)
EtaSq(fit.meanConfs.RU.RR.UU, type=1, anova=TRUE)


#' 2x2 anova on mean confidence with trial type (RU+ vs RR) * condition
#  Get means
# myDataForglm.RU.RR                   <- subset(resultsRH.perTrial.all, trialType %in% c("1.5", "2")) 
# myDataForglm.RU.RR                   <- as.data.frame(myDataForglm.RU.RR)
# myDataForglm.RU.RR$trialType         <- factor(myDataForglm.RU.RR$trialType)        
# myDataForglm.RU.RR$choiceConf.scaled <- scale(myDataForglm.RU.RR$choiceConf)

# #' ## Anova
# myDataForglm.RU.RR.mean <- aggregate(choiceConf ~ subject + condition + trialType, myDataForglm.RU.RR, mean)
# factornames <- c("subject", "condition", "trialType")
# myDataForglm.RU.RR.mean[factornames] <- lapply(myDataForglm.RU.RR.mean[factornames], as.factor) 

# fit.meanConfs <- aov(
#             choiceConf ~ (condition * trialType) + 
#             Error(subject/(condition * trialType)),
#             data = myDataForglm.RU.RR.mean)
# summary(fit.meanConfs)

# #' ### ANOVA effect sizes
# library(DescTools)
# EtaSq(fit.meanConfs, type=1, anova=TRUE)

# # bayesian anova
# # anovaBF(choiceConf ~ condition * trialType + subject, data = myDataForglm.RU.RR.mean,  whichRandom="subject")

# #' ### BF for interaction
# choiceConf.sum.bf  = lmBF(choiceConf ~ condition + trialType + subject,                       data=myDataForglm.RU.RR.mean,  whichRandom="subject")
# choiceConf.full.bf = lmBF(choiceConf ~ condition + trialType + condition:trialType + subject, data=myDataForglm.RU.RR.mean,  whichRandom="subject")

# # Compare the two models
# bf.condXtrialType = choiceConf.full.bf / choiceConf.sum.bf
# bf.condXtrialType


# #' ## Follow-up t-tests
# # Get each condition (probably could have been done with aggregate)
# RU.pop.means  <- myDataForglm.RU.RR.mean[myDataForglm.RU.RR.mean$trialType=="1.5" & myDataForglm.RU.RR.mean$condition=="1",]
# RR.pop.means  <- myDataForglm.RU.RR.mean[myDataForglm.RU.RR.mean$trialType=="2"   & myDataForglm.RU.RR.mean$condition=="1",]
# RU.dist.means <- myDataForglm.RU.RR.mean[myDataForglm.RU.RR.mean$trialType=="1.5" & myDataForglm.RU.RR.mean$condition=="2",]
# RR.dist.means <- myDataForglm.RU.RR.mean[myDataForglm.RU.RR.mean$trialType=="2"   & myDataForglm.RU.RR.mean$condition=="2",]




RR.pop.means     <- myDataForglm.RUp.RUm.RR.UU.mean[myDataForglm.RUp.RUm.RR.UU.mean$trialType=="2"   & myDataForglm.RUp.RUm.RR.UU.mean$condition=="1",]
RR.dist.means    <- myDataForglm.RUp.RUm.RR.UU.mean[myDataForglm.RUp.RUm.RR.UU.mean$trialType=="2"   & myDataForglm.RUp.RUm.RR.UU.mean$condition=="2",]
RUplu.pop.means  <- myDataForglm.RUp.RUm.RR.UU.mean[myDataForglm.RUp.RUm.RR.UU.mean$trialType=="1.5" & myDataForglm.RUp.RUm.RR.UU.mean$condition=="1",]
RUplu.dist.means <- myDataForglm.RUp.RUm.RR.UU.mean[myDataForglm.RUp.RUm.RR.UU.mean$trialType=="1.5" & myDataForglm.RUp.RUm.RR.UU.mean$condition=="2",]
RUmin.pop.means  <- myDataForglm.RUp.RUm.RR.UU.mean[myDataForglm.RUp.RUm.RR.UU.mean$trialType=="0.5" & myDataForglm.RUp.RUm.RR.UU.mean$condition=="1",]
RUmin.dist.means <- myDataForglm.RUp.RUm.RR.UU.mean[myDataForglm.RUp.RUm.RR.UU.mean$trialType=="0.5" & myDataForglm.RUp.RUm.RR.UU.mean$condition=="2",]
UU.pop.means     <- myDataForglm.RUp.RUm.RR.UU.mean[myDataForglm.RUp.RUm.RR.UU.mean$trialType=="0"   & myDataForglm.RUp.RUm.RR.UU.mean$condition=="1",]
UU.dist.means    <- myDataForglm.RUp.RUm.RR.UU.mean[myDataForglm.RUp.RUm.RR.UU.mean$trialType=="0"   & myDataForglm.RUp.RUm.RR.UU.mean$condition=="2",]

#' ### RR trials
t.test(RR.pop.means$choiceConf, RR.dist.means$choiceConf, paired = TRUE)
cohensD(RR.pop.means$choiceConf, RR.dist.means$choiceConf, method="paired")
ttestBF(x = RR.pop.means$choiceConf, y = RR.dist.means$choiceConf, paired=TRUE)
mean(RR.pop.means$choiceConf - RR.dist.means$choiceConf)
sd(RR.pop.means$choiceConf - RR.dist.means$choiceConf)

#' ### RU+ trials
t.test(RUplu.pop.means$choiceConf, RUplu.dist.means$choiceConf, paired = TRUE)
cohensD(RUplu.pop.means$choiceConf, RUplu.dist.means$choiceConf, method="paired")
ttestBF(x = RUplu.pop.means$choiceConf, y = RUplu.dist.means$choiceConf, paired=TRUE)
mean(RUplu.pop.means$choiceConf - RUplu.dist.means$choiceConf)
sd(RUplu.pop.means$choiceConf - RUplu.dist.means$choiceConf)

#' ### RU- trials
t.test(RUmin.pop.means$choiceConf, RUmin.dist.means$choiceConf, paired = TRUE)
cohensD(RUmin.pop.means$choiceConf, RUmin.dist.means$choiceConf, method="paired")
ttestBF(x = RUmin.pop.means$choiceConf, y = RUmin.dist.means$choiceConf, paired=TRUE)
mean(RUmin.pop.means$choiceConf - RUmin.dist.means$choiceConf)
sd(RUmin.pop.means$choiceConf - RUmin.dist.means$choiceConf)

#' ### UU trials
t.test(UU.pop.means$choiceConf, UU.dist.means$choiceConf, paired = TRUE)
cohensD(UU.pop.means$choiceConf, UU.dist.means$choiceConf, method="paired")
ttestBF(x = UU.pop.means$choiceConf, y = UU.dist.means$choiceConf, paired=TRUE)
mean(UU.pop.means$choiceConf - UU.dist.means$choiceConf)
sd(UU.pop.means$choiceConf - UU.dist.means$choiceConf)



#' ### Difference (between trial types) of differences (between conditions) 
# t.test((RU.pop.means$choiceConf - RU.dist.means$choiceConf), (RR.pop.means$choiceConf - RR.dist.means$choiceConf), paired = TRUE)
# cohensD((RU.pop.means$choiceConf - RU.dist.means$choiceConf), (RR.pop.means$choiceConf - RR.dist.means$choiceConf), method="paired")
# ttestBF(x = (RU.pop.means$choiceConf - RU.dist.means$choiceConf), y = (RR.pop.means$choiceConf - RR.dist.means$choiceConf), paired=TRUE)
# mean((RU.pop.means$choiceConf - RU.dist.means$choiceConf) - (RR.pop.means$choiceConf - RR.dist.means$choiceConf))
# sd((RU.pop.means$choiceConf - RU.dist.means$choiceConf) - (RR.pop.means$choiceConf - RR.dist.means$choiceConf))



# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Table 2

#' ### Proportion correct
mean(proportionCorrect.perSubj.wide$correct.pop)
mean(proportionCorrect.perSubj.wide$correct.dist)
sd(proportionCorrect.perSubj.wide$correct.pop)
sd(proportionCorrect.perSubj.wide$correct.dist)

#' ### RH accordance rate
RHaccordance <- ddply(resultsRH.perTrial.all, c("condition", "subject"), summarise,
                          RUplus = length(which(resultsRH.perTrial.all$trialType == "1.5"))
                        )


RHaccordance <- resultsRH.perTrial.all %>% 
    group_by(subject, condition) %>%
    summarise(
      RUplus  = sum(trialType=="1.5"),
      RUminus = sum(trialType=="0.5"),
      RUrate = sum(trialType=="1.5")/(sum(trialType=="1.5") + sum(trialType=="0.5"))
      )
RHaccordance <- as.data.frame(RHaccordance)

mean(RHaccordance$RUrate[RHaccordance$condition == 1])
mean(RHaccordance$RUrate[RHaccordance$condition == 2])
sd(RHaccordance$RUrate[RHaccordance$condition == 1])
sd(RHaccordance$RUrate[RHaccordance$condition == 2])

#' ### α
mean(modelResults$alpha.1)
mean(modelResults$alpha.2)
sd(modelResults$alpha.1)
sd(modelResults$alpha.2)

#' ### RTs
# aggregate for each individual (1st level)
meanRTsindividual <- resultsRH.perTrial.all %>% 
    group_by(subject, condition, trialType) %>%
    summarise(
      RTs  = mean(RT)
      )
meanRTsindividual <- as.data.frame(meanRTsindividual)

# aggregate for each condition (2nd level)
meanRTs <- meanRTsindividual %>% 
    group_by(condition, trialType) %>%
    summarise(
      RTs  = mean(RTs)
      )
meanRTs <- as.data.frame(meanRTs)
meanRTs

stdRTs <- meanRTsindividual %>% 
    group_by(condition, trialType) %>%
    summarise(
      SDs  = sd(RTs)
      )
stdRTs <- as.data.frame(stdRTs)
stdRTs

#' ### meanConfs
# 1st level
meanCONFsindividual <- resultsRH.perTrial.all %>% 
    group_by(subject, condition, trialType) %>%
    summarise(
      CONF  = mean(choiceConf)
      )
meanCONFsindividual <- as.data.frame(meanCONFsindividual)

# 2nd level
meanCONFs <- meanCONFsindividual %>% 
    group_by(condition, trialType) %>%
    summarise(
      meanCONFs  = mean(CONF)
      )
meanCONFs <- as.data.frame(meanCONFs)
meanCONFs

stdCONFs <- meanCONFsindividual %>% 
    group_by(condition, trialType) %>%
    summarise(
      stdCONFs  = sd(CONF)
      )
stdCONFs <- as.data.frame(stdCONFs)
stdCONFs


