
library(ggplot2)
library(BayesFactor)
library(gridExtra)

source(paste(fullPath, "/functions/multiplot.R", sep=""))

load(paste(fullPath,'/data/modelresults.Rda', sep=""))
load(paste(fullPath,'/data/modelresults_long.Rda', sep=""))

#' **REMEMBER: The stats are all done at the modelling level. Here we just plot r and alpha** 

# Get Δr
modelResults$Delta.r <- modelResults$r_param.1 - modelResults$r_param.2 

modelResults$Delta.r.sorted <- sort(modelResults$Delta.r)
modelResults$sortedOrder 	<- c(1:length(modelResults$ID))
modelResults$positiveDelta.r[modelResults$Delta.r.sorted > 0] <- 1
modelResults$positiveDelta.r[modelResults$Delta.r.sorted < 0] <- 0
modelResults$positiveDelta.r <- as.factor(modelResults$positiveDelta.r)

#p1 <- ggplot(data = modelResults, aes(x = sortedOrder, y = Delta.r.sorted, fill = positiveDelta.r)) +
p_deltaR <- ggplot(data = modelResults, aes(x = sortedOrder, y = Delta.r.sorted)) +
		geom_bar(stat="identity") +
		coord_flip() + 
	    #scale_fill_manual(values = c("blue", "red")) +
	    theme(legend.position="none") +
	    labs(x = "Participant index (sorted)", y = expression(Delta*r)) +
		ggtitle("A.                          ")


p_alphaVsR <- ggplot(modelResults.long, aes(x = alpha, y = r_param)) +
					geom_point(aes(shape=factor(condition)), alpha = 1) +
					scale_shape_manual(
						name = "Condition",
						values = c(19,1),
						labels = c("Population", "Distance")) + 
					scale_alpha_manual(values = c(0.4, 1)) +
					xlim(0, 1) +
					labs(x = expression(alpha), y = "r parameter") +
					#theme(legend.position="none") +
					# coord_fixed() + 																	#force square plot
					# theme(legend.position=c(.85,.85),
					ggtitle("B.                                                         ")



#' Maybe we'd like to plot ΔmeanConf vs Δalpha here.  


# Plot both with custom arrangement
grid.arrange(p_deltaR, p_alphaVsR, widths=c(0.3, 0.7), ncol=2)



