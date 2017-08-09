#' ---
#' title: Check task accuracy with Lost focus events 
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

# rmarkdown::render('~/scripts/checkAccuracyLostFocus.R')

# Run code Filevichetal_2_SI_and_cleanSubjs.R up until:

#' ## Identify distracted subjects

distractedSubjects <- aggregate(lostFocus ~ subject, resultsRH.perTrial.all, sum)
taskAccuracy 	   <- aggregate(correct ~ subject, resultsRH.perTrial.all, mean)

#' ## Check whether mean task accuracy was higher for more distracted subjects 

#' ### For all trials pooled together 

#' #### Raw Data 

cor.test(distractedSubjects$lostFocus, taskAccuracy$correct) 

plot(distractedSubjects$lostFocus, taskAccuracy$correct)
reg1 <- lm(taskAccuracy$correct~distractedSubjects$lostFocus)
abline(reg1)

#' #### sqrt-transformed data

cor.test(sqrt(distractedSubjects$lostFocus), taskAccuracy$correct) 

plot(sqrt(distractedSubjects$lostFocus), taskAccuracy$correct)
reg2 <- lm(taskAccuracy$correct~sqrt(distractedSubjects$lostFocus+1))
abline(reg2)


#' ### For all trials pooled but split for each condition

distractedSubjects.condition 	<- aggregate(lostFocus ~ subject*condition, resultsRH.perTrial.all, sum)
taskAccuracy.condition 	   		<- aggregate(correct ~ subject*condition, resultsRH.perTrial.all, mean)
perCondition 					<- merge(distractedSubjects.condition, taskAccuracy.condition, by=c("subject", "condition"))
perCondition$condition 			<- as.factor(perCondition$condition)

#' #### Raw Data 

cor.test(perCondition$lostFocus[perCondition$condition==1], perCondition$correct[perCondition$condition==1]) 
cor.test(perCondition$lostFocus[perCondition$condition==2], perCondition$correct[perCondition$condition==2]) 

theme_set(theme_bw())
ggplot(perCondition, aes(x = lostFocus, y = correct, color = condition)) + 
    geom_point() + 
    stat_smooth(method = 'lm', se = FALSE)

#' #### sqrt-transformed data

cor.test(sqrt(perCondition$lostFocus[perCondition$condition==1]), perCondition$correct[perCondition$condition==1]) 
cor.test(sqrt(perCondition$lostFocus[perCondition$condition==2]), perCondition$correct[perCondition$condition==2]) 

#' ### For RU trials only, split for each condition

distractedSubjects.condition.RU <- aggregate(lostFocus ~ subject*condition, resultsRH.perTrial.all[!is.na(resultsRH.perTrial.all$RU.Rchosen),], sum)
taskAccuracy.condition.RU 	   	<- aggregate(correct ~ subject*condition, resultsRH.perTrial.all[!is.na(resultsRH.perTrial.all$RU.Rchosen),], mean)
perCondition.RU 				<- merge(distractedSubjects.condition.RU, taskAccuracy.condition.RU, by=c("subject", "condition"))
perCondition.RU$condition 		<- as.factor(perCondition.RU$condition)

#' #### Raw Data 

cor.test(perCondition.RU$lostFocus[perCondition$condition==1], perCondition.RU$correct[perCondition$condition==1]) 
cor.test(perCondition.RU$lostFocus[perCondition$condition==2], perCondition.RU$correct[perCondition$condition==2]) 

ggplot(perCondition.RU, aes(x = lostFocus, y = correct, color = condition)) + 
    geom_point() + 
    stat_smooth(method = 'lm', se = FALSE) +
    ggtitle("only RU trials")


#' #### sqrt-transformed data

cor.test(sqrt(perCondition.RU$lostFocus[perCondition$condition==1]), perCondition.RU$correct[perCondition$condition==1]) 
cor.test(sqrt(perCondition.RU$lostFocus[perCondition$condition==2]), perCondition.RU$correct[perCondition$condition==2]) 





