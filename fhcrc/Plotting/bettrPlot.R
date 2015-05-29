bettrPlot  <- function(model, toplot="points", which=c(1:3, 5), mfrow=c(1,1), smooth="auto", rView=T, stderr=TRUE,
											 returnwhat= "plots", printPlot=T, grobOut=T, ...) {
	
	# bettrPlot rewrites plot() to produce ggplot plots similar to plot().
	# Define a var for bettrPlot, and you can ggsave() it.
	
	
	## PARAMETER LIST ##
	# 1. `model' is of class lm, input.
	# 2. `toplot' is either "points" or "labels". 
	# a. The latter can turn datapoints for scatters into labels.
	# 3. `which' is num or listnum of which plots to output.
	# 4. `mfrow' is x/y list of dimensions of grid to print out.
	# 5. `smooth' is method of applying geom_smooth.
	# 6. `stderr' is a logical, TRUE to have standard error geom_smooth appear.
	# 7. `printPlot' is logical, TRUE to show plot(s) in viewer.
	# 8. `grobOut' is logical, TRUE to return grob so you can ggsave() variable.
	
	
	# Needed libraries
	library(ggplot2); library(grid); library(gridExtra)
	
	# New environment needed for arrangeGrob to work.  
	plotspace <- new.env()
	
	# fortify() makes class lm ggplotable data frame.
	df <- fortify(model)
	df <- cbind(df, rows = 1:nrow(df))
	
	# Logic for pars (2) and (8)
	# TODO: Allow some quick theme changes from parameters.
	if(toplot == "points") {
		toplot <- geom_point(size=1,alpha=1/2)
	} else if(toplot == "labels") {
		toplot <- geom_text(aes(label=rows))
	}
	
	### Plotting ###
	# TODO: automatically note influential points 
	# TODO: implement spread-level plot
	## G1: residuals vs fitted
	assign("g1",
				 ggplot(df, aes(.fitted, .resid)) +
				 	toplot +
				 	geom_smooth(se=stderr, method=smooth) +
				 	geom_hline(linetype=2, size=.2, aes(yintercept=0)) +
				 	scale_x_continuous("Fitted Values") +
				 	scale_y_continuous("Residual") +
				 	ggtitle("Residuals vs Fitted")
				 ,envir=plotspace)
	
	## G2: normal qq	
	a <- quantile(df$.stdresid, c(0.25, 0.75))
	b <- qnorm(c(0.25, 0.75))
	slope <- diff(a)/diff(b)
	int <- a[1] - slope * b[1]
	
	# plotting
	assign("g2",
				 ggplot(df, aes(sample=.stdresid)) +
				 	geom_abline(slope=slope, intercept=int,colour="red",linetype="longdash", size=.25) +
				 	stat_qq(size=0.75, alpha=1/2) +
				 	scale_x_continuous("Theoretical Quantiles") +
				 	scale_y_continuous("Standardized Residuals") +
				 	ggtitle("Normal Q-Q")
				 ,envir=plotspace)
	
	## G3: histogram of response variable
	df2 <- df
	names(df2)[1] <- "responsevar"
	
	# plotting
	assign("g3",
				 ggplot(df2, aes(x=responsevar)) +
				 	geom_histogram(aes(y=..density..),
				 								 colour="black", fill="white") +
				 	geom_density(alpha=.2,fill="#1491CC") +
				 	scale_y_continuous("Frequency") +
				 	scale_x_continuous("Response Variable") +
				 	ggtitle("Response Variable Histogram")
				 ,envir=plotspace)
	
	# G4: histogram of residuals
	assign("g4",
				 ggplot(df, aes(.resid)) +
				 	geom_histogram(aes(y=..density..),
				 								 colour="black", fill="white") +
				 	geom_density(alpha=.2,fill="#1491CC") +
				 	scale_y_continuous("Residuals") +
				 	ggtitle("Residuals Histogram")
				 ,envir=plotspace)
	
	## G5: cook's distance
	horizline <- 4/length(model$residuals) # Create base of graph. 
	
	# plotting
	assign("g5",
				 ggplot(df, aes(rows, .cooksd, ymin=0, ymax=.cooksd)) + 
				 	toplot + geom_point(aes(color=.cooksd),size=2.2,alpha=1) +
				 	scale_colour_gradient(limits=c(0.05,1), low="red", high="red") +
				 	geom_linerange(position="jitter") +
				 	geom_hline(y=horizline, linetype=2) + 
				 	scale_x_continuous("Observation Number") +
				 	scale_y_continuous("Cook's Distance") +
				 	ggtitle("Cook's Distance") +
				 	theme(legend.position="none")
				 ,envir=plotspace)
	
	
	## G6: residuals vs leverage
	assign("g6",
				 ggplot(df, aes(.hat, .stdresid)) +
				 	toplot +
				 	geom_smooth(se=stderr, method=smooth) +
				 	geom_hline(linetype=2, size=.2) +
				 	scale_x_continuous("Leverage") +
				 	scale_y_continuous("Standardized Residuals") +
				 	ggtitle("Residuals vs Leverage")
				 ,envir=plotspace)		
	
	## G7: cooks distance vs leverage
	assign("g7",
				 ggplot(df, aes(.hat, .cooksd)) +
				 	toplot +
				 	geom_abline(slope = seq(0, 3, by = 0.5), colour = "grey",size=2,alpha=1/4) +
				 	geom_smooth(se=stderr, method=smooth) +
				 	scale_x_continuous("Leverage") +
				 	scale_y_continuous("Cook's Distance") +
				 	ggtitle("Cook's Distance vs Leverage")
				 ,envir=plotspace)
	
	
	# Store the plots in plotspace$ for grobOut.
	plots <- list(plotspace$g1, plotspace$g2, plotspace$g3, 
								plotspace$g4, plotspace$g5, plotspace$g6, 
								plotspace$g7)
	
	# Make the plots
	if(printPlot==T){
		
		grid.newpage()
		
		# Throw the plots on seperate pages, grid::stuff()
		if (prod(mfrow)>1) {
			mypos <- expand.grid(1:mfrow[1], 1:mfrow[2])
			mypos <- mypos[with(mypos, order(Var1)), ]
			pushViewport(viewport(layout = grid.layout(mfrow[1], mfrow[2])))
			formatter <- function(.){}
		} else {
			mypos <- data.frame(matrix(1, length(which), 2))
			pushViewport(viewport(layout = grid.layout(1, 1)))
			
			# formatter spits plots onto new pages, following prompt.
			formatter <- function(.) {
				.dontcare <- readline("Hit <Return> to see next plot: ")
				grid.newpage()
			}
		}
		
		# Print layout to viewer
		j <- 1
		for (i in which){
			formatter()
			print(plots[[i]], vp=viewport(layout.pos.row=mypos[j,][1], layout.pos.col=mypos[j,][2]))
			j <- j+1
		}
	}
	
	# returns graphical object 
	if(grobOut==T){
		plotNames <- c("g1","g2","g3","g4","g5","g6","g7") # All possible plots
		usedPlots <- plotNames[which]                      # Those `which' were called for
		return(do.call("arrangeGrob",                      # use arrangeGrob()
									 lapply(usedPlots,                   # on the variables specified by `which'
									 			 get,envir=plotspace))         # grab em from the plotspace
		) 
	}
	
	rm(plotspace) # remove plotspace environment
}