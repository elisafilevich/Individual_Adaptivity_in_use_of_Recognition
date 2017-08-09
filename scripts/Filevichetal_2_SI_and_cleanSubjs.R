#' ---
#' title: Get all results and figures for Filevich, Kühn and Horn 
#' author: Elisa Filevich
#' output: 
#'   html_document:
#'     code_folding: show       # code is hidden, but can be seen by clicking on "Code" buttons
#'     number_sections: true
#'     toc: true                # yes, a table of contents 
#'     toc_float:               # - floating (i.e. only remaining visible despite scrolling page)
#'       collapsed: false       
#'       smooth_scroll: false
#' 
#' ---

library(lsr) 			# compute effect sizes in ttests
library(R.matlab)
library(plyr)  			# to summarize data and required before dplyr
library(dplyr) 			# for renaming columns 
library(reshape) 		# melt and cast functions
library(BayesFactor)
library(knitr)

source(paste(fullPath, "/functions/as.data.frame.list.R", sep=""))

# --------------------------------------------------------------------------------------------------------------------------------
# Load all "preprocessed" data
load(file = paste(fullPath,'/data/modelresults.Rda', sep=""))
load(file = paste(fullPath,'/data/modelresults_long.Rda', sep=""))
load(file = paste(fullPath,'/data/congruencyIndex.Rda', sep=""))
load(file = paste(fullPath,'/data/resultsRH_perSubj.Rda', sep=""))
load(file = paste(fullPath,'/data/resultsRH_perTrial.Rda', sep=""))

# --------------------------------------------------------------------------------------------------------------------------------

#' # Analysis of Sequence Effects
#' ## r parameter (number of trials in line with RH)


# Do an aov first
modelResults.summary <- aggregate(r_param ~ conditionOrder * taskOrder * condition * ID, modelResults.long, mean)
factornames 		 <- c("ID", "taskOrder", "conditionOrder", "condition")
modelResults.summary[factornames] <- lapply(modelResults.summary[factornames], as.factor) 

fit.orderEffects <- aov(r_param ~ (conditionOrder * taskOrder * condition) + 
            Error(ID/condition) + conditionOrder * taskOrder,
            data = modelResults.summary)
summary(fit.orderEffects)


#' #### BF: Confirm no main effect of condition order on r_parameter
rParam.mainEffectCondAlone.bf 		= lmBF(r_param ~ condition + ID, 				  data=modelResults.long,  whichRandom="ID")
rParam.mainEffectsTaskOrder.bf 		= lmBF(r_param ~ condition + taskOrder + ID,	  data=modelResults.long,  whichRandom="ID")
rParam.mainEffectsEnvironOrder.bf 	= lmBF(r_param ~ condition + conditionOrder + ID, data=modelResults.long,  whichRandom="ID")

# Compare the two models
bf.condOrder = rParam.mainEffectsEnvironOrder.bf / rParam.mainEffectCondAlone.bf
bf.condOrder
bf.taskOrder = rParam.mainEffectsTaskOrder.bf / rParam.mainEffectCondAlone.bf
bf.taskOrder

#' ### Confirm no interaction of condition and conditionOrder on r_parameter
rParam.mainEffects.bf 			= lmBF(r_param ~ condition + taskOrder + conditionOrder + ID, 							 data=modelResults.long,  whichRandom="ID")
rParam.InteractionTaskOrder.bf 	= lmBF(r_param ~ condition + taskOrder + conditionOrder + condition:taskOrder + ID, 	 data=modelResults.long,  whichRandom="ID")
rParam.InteractionCondOrder.bf 	= lmBF(r_param ~ condition + taskOrder + conditionOrder + condition:conditionOrder + ID, data=modelResults.long,  whichRandom="ID")

# Compare the two models
bf.condXtaskOrder = rParam.InteractionTaskOrder.bf / rParam.mainEffects.bf
bf.condXtaskOrder

bf.condXCondOrder = rParam.InteractionCondOrder.bf / rParam.mainEffects.bf
bf.condXCondOrder


# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Congruency index 
#' Compare indeces between conditions, partially to show that the manipulation worked 

#' ### Population
sd(congruencyIndex$population)
t.test(congruencyIndex$population, mu=0.5, var.equal=TRUE)
cohensD(congruencyIndex$population, mu=0.5)  			# no method = "paired" in this case: one sample tests!

#' ### Distance
sd(congruencyIndex$distance)
t.test(congruencyIndex$distance	, mu=0.5, var.equal=TRUE)
cohensD(congruencyIndex$distance, mu=0.5) 


# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Lost Focus 

#' ## Identify distracted subjects during recognition task
distractedSubjects_Recognition <- sum(resultsRH.perSubj$totalLostFocusRecognized)
distractedSubjects_Recognition


#' ## Identify distracted subjects during comparative judgement task
distractedSubjects <- aggregate(lostFocus ~ subject, resultsRH.perTrial.all, sum)
taskAccuracy 	   <- aggregate(correct ~ subject, resultsRH.perTrial.all, mean)

#' ## Check overlap between distracted subjects in recognition and comparative judgement task
distractedSubjects$lostFocus[(resultsRH.perSubj$totalLostFocusRecognized>0)]


#' ### Check whether mean task accuracy was higher for more distracted subjects 
cor.test(distractedSubjects$lostFocus, taskAccuracy$correct) 
t.test(taskAccuracy$correct[distractedSubjects$lostFocus >= 1], taskAccuracy$correct[distractedSubjects$lostFocus < 1])
ttestBF(x = taskAccuracy$correct[distractedSubjects$lostFocus >= 1], y = taskAccuracy$correct[distractedSubjects$lostFocus < 1], paired=FALSE)

distracted <- data.frame(lostFocusTimes = distractedSubjects$lostFocus,
						 meanAccuracy = taskAccuracy$correct)
lmBF(meanAccuracy ~ lostFocusTimes, data = distracted)

#' ## So we decide to remove all subjects that left the browser window at least once
toRemove_index 	   <- which(distractedSubjects$lostFocus >= 1)
toRemove_subject   <- distractedSubjects$subject[distractedSubjects$lostFocus >= 1]
length(toRemove_subject)


# Clean up data
congruencyIndex 		<- congruencyIndex[-toRemove_index, ] 
modelResults 			<- modelResults[-toRemove_index, ]
modelResults.long 		<- modelResults.long[-c(modelResults.long$ID %in% toRemove_index), ]
resultsRH.perSubj 		<- resultsRH.perSubj[-toRemove_index, ]
resultsRH.perTrial.all  <- resultsRH.perTrial.all[-c(which(resultsRH.perTrial.all$subject %in% toRemove_subject)), ] 

# Run all the analyses again
{{spin_child(paste(fullPath,'/scripts/FilevichEtAl_allMainResults.R', sep=""))}}



# Then run this from outside
# rmarkdown::render('/scripts/Filevichetal_2_SI_and_cleanSubjs.R')
