# val_GenResultsGlmm.R
# 
# Summary:
# Do a chisquared analysis to determine if search difficulty and the frequency
# of different search outcomes are related. Do an ANOVA type analysis with 
# GLMMs to determine how search difficulty affects search performance
# and behavior. Do ICC and correlation analysis to determine how consistently 
# subjects performed within and across difficulty treatments respectively.
#
# Requires: Data files, listed libraries
#
# Outputs: Results of Chi-square, Glmms, R squared, ICCs, and Spearman's
# correlation analysis. Also output figures detailing Glmm and correlation
# results. 
# 
# Author: Olivia Guayasamin
# Date: 7/12/2017
# ----- Initializing steps -----

# read and work with data files
library(readxl)  
library(reshape2)   

# glmm analysis
library(lme4)
library(lmerTest)
library(MuMIn)
library(pscl)

# additional stats functions
library(psych)
library(car)  
library(MASS)

# format figures and output
library(ggplot2)
library(grid)
library(gridExtra)  
library(knitr) 

# quick function for getting legend from a ggplot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#set working directory
getwd()
setwd("~/R/val_GenResultsGlmm")

# Save all console output to file
sink("val_GenResultsGlmm.doc", append = FALSE)  # initialize output sink


# ----- Import and format data -----

# import data files
val.GenData <- read_excel("val_GenResultsBlockedData.xlsx")

# get rid of excess data columns
val.Data <- as.data.frame(val.GenData[, c(1, 2, 3, 4, 5, 6, 9, 10,
                                          11, 12, 17, 18, 19, 20)])
head(val.Data, 3)  # check first 3 rows 


# ----- Chi Square test of independence on search outcomes -----

# Determine if there is a relationship between frequency of search outcomes 
# (TP, TN, FP, FN) is independent from search difficulty 

# subset data depending on experimental treatment
val.Most.Cont <- subset(val.Data, val.Data[, 2] == 'most')
val.Least.Cont <- subset(val.Data, val.Data[, 2] == 'least')

# get data for contingency table 
cont.Counts <- as.data.frame(rbind(colSums(val.Most.Cont[, c(7, 8, 9, 10)], 
                                           na.rm = TRUE), 
                                   colSums(val.Least.Cont[, c(7, 8, 9, 10)], 
                                           na.rm = TRUE)), 
                             row.names = c("Easy", "Hard"), 
                             col.names = colnames(val.Most.Cont[, 
                                                                c(7, 8, 9, 10)]))
print(cont.Counts)

# plot data in contingency table for visualization
# format data for plotting 
dat <- melt(t(cont.Counts))
colnames(dat) <- c("SearchType", "Treatment", "Count")

# make and save barplot
jpeg(filename = "SearchCountBarPlot.jpg", units = "in", height = 5, width = 9, 
     res  = 300)  
ggplot(data = dat, aes(x = SearchType, y = Count, fill = Treatment)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  ylim(0, 800) +
  geom_text(aes(label = Count), vjust = -0.3, position = position_dodge(0.9), 
            size = 3.5) + 
  ggtitle("Counts of Search Types by Difficulty Treatment") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
dev.off()

# plot a contingency table in mkd
# print(kable(cont.Counts,  results = 'asis', 
#            caption = "Contingency Table of Search Outcome by Treatment", 
#            digits = 2))

# conduct chi square test of independence
cont.Results <- chisq.test(cont.Counts)

# format and print test results
print("Chi-squared results")
print(cont.Results)


# ----- Check data distribution prior to analysis -----

# due to large amounts of missing data for FN and FP searches, only conduct
# remaining analysis on TP and TN searches, search distance, and pupil size

# visualize data using hists and normal qq plots, draw multiple plots per figure
# put into list to allow iteration of both data sets
# list to keep track of different groups of variables we want to visualize
varTracker <- list(c(5, 6), c(7, 10), c(11, 12))

# iterate thorught different groups of variables
for (j in 1:length(varTracker)){
  cur.Data <-  data.frame(rbind(val.Most.Cont[, varTracker[[j]]],
                            val.Least.Cont[, varTracker[[j]]]))
  # create and save figure for number searches
  fig.Title <- sprintf("genResultsDistCheck.%d.jpg", j)
  jpeg(filename <- fig.Title, units = "in", height = 6, width = 6, res = 300)
  par(mfcol = c(2, ncol(cur.Data)))  # format figure window
  # for each variable plot a histogram and qqplot 
  for (k in 1:ncol(cur.Data)){
    thisPlot.Title <- sprintf("%s", names(cur.Data[k]))  # format and add title
    hist(cur.Data[, k], main = thisPlot.Title)  # histogram
    qqnorm(cur.Data[, k]); qqline(cur.Data[, k])  # qqplot with normal line
  }
  dev.off()  # close and save figures
}


# ----- ANOVA type analysis with Glmms-----

#TO DO give summary of this chunk here

# reformat data for applying and plotting glmms in a loop
dat.4Analysis <- as.data.frame(val.Data[, c(1, 2, 3, 4, 5, 6, 7, 10, 11, 12)])
dat.4Poisson<- dat.4Analysis[, c(1, 2, 3, 4, 7, 8)]
dat.4Gauss <- dat.4Analysis[, c(1, 2, 3, 4, 5, 6, 9, 10)]

# for our continuous variables
# loop through data 
for (i in 5:ncol(dat.4Gauss)){
  # current dependent variables
  cur.Data <- dat.4Gauss[, c(1, 2, 3, 4, i)]
  cur.Val.Most <- subset(cur.Data, cur.Data[, 2] == 'most')
  cur.Val.Least <- subset(cur.Data, cur.Data[, 2] == 'least')
  cur.Name <- names(cur.Data[5])
  
  # rename data col i to "y" for iteration
  colnames(cur.Data)[5] = "Y"
  # create model
  cur.Glmm <- glmer(Y ~ ExpVer + TreatmentOrder + (1|BlockOrder) + (1|PartID),
                    data = cur.Data, family = gaussian(link = identity))
  
  # kable output for rmkd file
  print(sprintf("Results for %s", cur.Name))
  print(summary(cur.Glmm))  # full summary, with p values for fixed effects
  print(confint(cur.Glmm, method = 'boot', oldNames = FALSE))  # confidence intervals for everything
  print(lsmeansLT(cur.Glmm))  # get estimates for all levels of fixed effect
  print(rand(cur.Glmm))  # p values for random effects and slopes
  
  # print conditional r squared and marginal r squared
  cur.Glmm.Rsquareds <- r.squaredGLMM(cur.Glmm)
  print(cur.Glmm.Rsquareds)
  
  # for each variable plot residuals
  resid.Title <- sprintf("residuals.%s", cur.Name) # title
  qqnorm(resid(cur.Glmm), main = resid.Title) ; qqline(resid(cur.Glmm))
}


# for our count variables
# loop through data 
for (i in 5:ncol(dat.4Poisson)){
  # current dependent variables
  cur.Data <- dat.4Poisson[, c(1, 2, 3, 4, i)]
  cur.Val.Most <- subset(cur.Data, cur.Data[, 2] == 'most')
  cur.Val.Least <- subset(cur.Data, cur.Data[, 2] == 'least')
  cur.Name <- names(cur.Data[5])
  
  # rename data col i to "y" for iteration
  colnames(cur.Data)[5] = "Y"
  # create model
  cur.Glmm <- glmer(Y ~ ExpVer + TreatmentOrder + (1|BlockOrder) + (1|PartID),
                    data = cur.Data, family = poisson(link = log))
  
  # kable output for rmkd file
  print(sprintf("Results for %s", cur.Name))
  print(summary(cur.Glmm))  # full summary, with p values for fixed effects
  print(confint(cur.Glmm, method = 'boot', oldNames = FALSE))  # confidence intervals for everything
  print(lsmeansLT(cur.Glmm))  # get estimates for all levels of fixed effect
  print(rand(cur.Glmm))  # p values for random effects and slopes
  
  # print conditional r squared and marginal r squared
  cur.Glmm.Rsquareds <- r.squaredGLMM(cur.Glmm)
  print(cur.Glmm.Rsquareds)
  
  # for each variable plot residuals
  resid.Title <- sprintf("residuals.%s", cur.Name) # title
  qqnorm(resid(cur.Glmm), main = resid.Title) ; qqline(resid(cur.Glmm))
}



# ----- Within Treatment ICC esitmates -----

# determine subject behavioral consistency within treatments

# select data for ICC analysis
dat.4ICC <- as.data.frame(dat.4Analysis[, c(1, 2, 4, 5, 6, 7, 8, 9, 10)])
# create empty list for holding data
results.ICC <- list()

# iterate throught ICC data frame
for (i in 4:length(dat.4ICC)) {
  
  # subset data by treatment and reformat for ICC
  cur.Most.ICC <- na.omit(dcast(subset(dat.4ICC[, c(1, 3, i)], 
                                       dat.4ICC[, 2] == 'most'), PartID ~ BlockOrder))
  cur.Least.ICC <- na.omit(dcast(subset(dat.4ICC[, c(1, 3, i)], 
                                        dat.4ICC[, 2] == 'least'), PartID ~ BlockOrder))
  
  # use icc from the psych package
  most.ICC <- ICC(cur.Most.ICC[, 2:5], alpha = 0.25)
  least.ICC <- ICC(cur.Least.ICC[, 2:5], alpha = 0.25)
  
  # temporary data frames for outputting results to file
  t.Easy = t(as.data.frame(c(Treatment = "Easy",
                             rbind(subset(most.ICC$results, 
                                          most.ICC$results$type == 'ICC2'),
                                   subset(most.ICC$results,
                                          most.ICC$results$type == 'ICC2k')))))
  t.Hard = t(as.data.frame(c(Treatment = "Hard",
                             rbind(subset(least.ICC$results, 
                                          least.ICC$results$type == 'ICC2'),
                                   subset(least.ICC$results,
                                          least.ICC$results$type == 'ICC2k')))))
  
  # combine into one data frame
  cur.ICC.Output <- as.data.frame(cbind(t.Easy, t.Hard))
  # put into result list
  results.ICC[[i-3]] <- cur.ICC.Output
  
  # filename, TODO kable this output later
  sprintf("%s ICC Results", names(dat.4ICC[i]))
  # print
  print(cur.ICC.Output)
}
# change names of list items
names(results.ICC) <- names(dat.4ICC[4:9])
# try to kable this list


# ----- Correlation Analysis across treatments -----

# correlation analysis of subject behaviors across difficulty treatments

# make empty list for holding result
dat.4Corr <- list()

# iterate through data columns in list to get average of participant behavior
# across blocks
for (i in 4:length(dat.4ICC)) {
  # current variable name
  cur.Names <- c(sprintf("Easy_%s", names(dat.4ICC[i])), 
                 sprintf("Hard_%s", names(dat.4ICC[i])),
                 "SubjectID")
  # current variable data
  cur.Most.Corr <- dcast(subset(dat.4ICC[, c(1, 3, i)], 
                                dat.4ICC[, 2] == 'most'), PartID ~ BlockOrder)
  cur.Least.Corr <- dcast(subset(dat.4ICC[, c(1, 3, i)], 
                                 dat.4ICC[, 2] == 'least'), PartID ~ BlockOrder)
  # take average of column values and convert to data frame
  cur.Data.Sum <- as.data.frame(cbind(rowMeans(cur.Most.Corr[2:5]), 
                                      rowMeans(cur.Least.Corr[2:5]),
                                      cur.Most.Corr[1]))
  # change column names 
  colnames(cur.Data.Sum) <- cur.Names  
  # add to list
  dat.4Corr[[i-3]] <- cur.Data.Sum
}

# change names of list slots to variable names
names(dat.4Corr) <- names(dat.4ICC[4:9]) # get list names


# make empty list to hold correlation results
results.Corr <- list()
# calculate correlations for data from each experimental group
for (i in 1:length(dat.4Corr)){
  # run spearmans correlation
  corr.res = cor.test(dat.4Corr[[i]][, 1], dat.4Corr[[i]][, 2],
                      method = "spearman")  
  # print with nice format
  sprintf("Spearman's correlation results for %s", names(dat.4Corr[i]))
  corr.4Print = cbind(names(dat.4Corr[[i]][1]), 
                      names(dat.4Corr[[i]][2]),
                      corr.res$estimate, 
                      corr.res$p.value)
  # change names
  colnames(corr.4Print) = cbind("Var1", "Var2", "Corr-Value", "P-Value")
  # put results into list
  results.Corr[[i]] <- corr.4Print
  # print for immediate output
  print(corr.4Print)
}

# change names of list slots to variable names
# TODO try to print this entire list
names(results.Corr) <- names(dat.4Corr) # get list names


# ----- Boxplots to compare treatments -----

# use boxplots and histograms to show subject change in behavior as a result
# of difficulty treatment

# new var tracker for remaining analysis
varTracker <- list(c(1, 2), c(3, 5), c(4, 6))
# rename so shit isn't confusing
dat.4Plots <- dat.4Corr
# create and print plots for data from each experimental group
axes.names = c("Diameter (mm)", "Total Distance (px)",  # var names
               "Average Number", "Time (ms)",
               "Average Number", "Time (ms)") 
titles.names = c("Average Pupil Size", "Total Search Distance", 
                 "Average # Targets Found", "Average Search Duration - Targets", 
                 "Average # Distractors Ignored", 
                 "Average Search Duration - Distractors")
counter = 0  # establish counter for keeping track of names

# go through variables 
for (i in 1:length(varTracker)){
  
  # prep data frame for boxplot 1
  plot1.dat <- dat.4Plots[varTracker[[i]][1]]
  names(plot1.dat[[1]]) <- cbind("Easy", "Hard", "SubjectID")
  plot1.dat.m = melt(plot1.dat, id = "SubjectID", value.name = "VarName",
                     variable.name = "Treatment")
  # prep data frame for histogram 1
  hist1.dat <- plot1.dat[[1]][2] - plot1.dat[[1]][1]
  colnames(hist1.dat) <- "Change"
  counter = counter + 1
  
  # create plot object for variable 1
  plot1 = ggplot(plot1.dat.m, aes(x = Treatment, 
                                  y = VarName)) + 
    geom_line(aes(group = SubjectID, colour = SubjectID), size = 1) +
    geom_point(aes(group = SubjectID, colour = SubjectID), size = 1.5) + 
    geom_boxplot(aes(fill = Treatment), width = 0.6, alpha = 0.6, lwd = 1,
                 notch = TRUE, outlier.shape = NA) +
    labs(title = "The Effect of Difficulty Treatment on",  # change labels and titles
         subtitle = titles.names[counter],  
         y = axes.names[counter]) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size = 12), 
          plot.subtitle = element_text(hjust = 0.5, size = 10)) +
    scale_fill_manual(values = c("#072bab", "#0ab2f4")) +  # boxplot fill
    guides(colour = FALSE, size = FALSE)  # remove legend for subjects & size
  
  # save search type histogram as jpeg image
  hist1 = ggplot(data = hist1.dat, aes(x = Change)) + 
    # histogram
    geom_histogram(alpha = 0.7, position = "identity", bins = 10, 
                   fill = "#0072BB") +
    # with normal dist on top
    geom_vline(xintercept = 0, linetype = "dashed", color = 'blue') +
    labs(title = expression(paste("Subject ", Delta, " Across Treatments")), 
         subtitle = titles.names[counter],
         x = expression(paste(Delta ["H - E"])), 
         y = "Count") + # axes labels and plot titles
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12), 
          plot.subtitle = element_text(hjust = 0.5, size = 10))
  
  # prep data frame for plot 2
  plot2.dat <- dat.4Plots[varTracker[[i]][2]]
  names(plot2.dat[[1]]) <- cbind("Easy", "Hard", "SubjectID")
  plot2.dat.m = melt(plot2.dat, id = "SubjectID", value.name = "VarName",
                     variable.name = "Treatment")
  # prep data frame for histogram 1
  hist2.dat <- plot2.dat[[1]][2] - plot2.dat[[1]][1]
  colnames(hist2.dat) <- "Change"
  counter = counter + 1
  
  # create plot object for variable 2
  plot2 = ggplot(plot2.dat.m, aes(x = Treatment, 
                                  y = VarName)) + 
    geom_line(aes(group = SubjectID, colour = SubjectID), size = 1) +
    geom_point(aes(group = SubjectID, colour = SubjectID), size = 1.5) + 
    geom_boxplot(aes(fill = Treatment), width = 0.6, alpha = 0.6, lwd = 1,
                 notch = TRUE, outlier.shape = NA) +
    labs(title = "The Effect of Difficulty Treatment on",  # change labels and titles
         subtitle = titles.names[counter],  
         y = axes.names[counter]) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size = 12), 
          plot.subtitle = element_text(hjust = 0.5, size = 10)) +
    scale_fill_manual(values = c("#072bab", "#0ab2f4")) +  # boxplot fill
    guides(colour = FALSE, size = FALSE)  # remove legend for subjects & size
  
  # save search type histogram as jpeg image
  hist2 = ggplot(data = hist2.dat, aes(x = Change)) + 
    # histogram
    geom_histogram(alpha = 0.7, position = "identity", bins = 10, 
                   fill = "#0072BB") +  
    # with normal dist on top
    geom_vline(xintercept = 0, linetype = 'dashed', color = 'blue') +
    labs(title = expression(paste("Subject ", Delta, " Across Treatments")), 
         subtitle = titles.names[counter],
         x = expression(paste(Delta ["H - E"])), 
         y = "Count") + # axes labels and plot titles
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12), 
          plot.subtitle = element_text(hjust = 0.5, size = 10))
  
  # get legend for boxplot figure
  cur.Plot.Legend = get_legend(plot1)
  
  # remove remaining legends
  plot1.new <- plot1 + theme(legend.position = "none")
  plot2.new <- plot2 + theme(legend.position = "none")
  hist1.new <- hist1 + theme(legend.position = "none")
  hist2.new <- hist2 + theme(legend.position = "none")
  
  # make a blank image to fill space
  blank <- rectGrob(gp=gpar(col="white")) # make a white spacer grob
  
  # put r1 and val plots into a list
  plotting.list <- list(cur.Plot.Legend, plot1.new, plot2.new,
                        blank, blank, blank,
                        blank, hist1.new, hist2.new)
  
  # use grid arrange to print plots and save as image
  fig.Title <- sprintf("genResultsBarPlots.%d.jpg", i) # title
  jpeg(filename <- fig.Title, units = "in", height = 8, width = 9, res = 300)
  grid.arrange(grobs = plotting.list, nrow = 3, ncol = 3, 
               heights = c(0.45, 0.05, 0.45), widths = c(0.2, 0.4, 0.4))  
  dev.off()  # close and save figures
}

# ----- Scatterplots to view relationships -----

# scatter plots to show relationship between subject behaviors across 
# difficulty treatments

# re initialize counter
counter = 0

# plot scatterplots to visualize relationships 
for (i in 1:length(varTracker)){
  
  # format data for plotting
  corr1Plot <- dat.4Plots[varTracker[[i]][1]]
  names(corr1Plot[[1]]) <- cbind("Easy", "Hard", "SubjectID")
  corr1Plot.dat <- data.frame(Easy = corr1Plot[[1]][1], 
                              Hard = corr1Plot[[1]][2])
  counter = counter + 1  # advance counter
  
  # scatterplot
  corr1.plot = ggplot(corr1Plot.dat, aes(x = Easy, y = Hard)) +
    geom_point(shape = 1, size = 2, na.rm = TRUE, show.legend = TRUE) +
    geom_smooth(method = lm, se = TRUE) +
    labs(title = titles.names[counter], 
         subtitle = "Across Treatments", 
         x = sprintf("Easy - %s", axes.names[counter]),
         y = sprintf("Hard - %s", axes.names[counter])) +  # change text
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          plot.subtitle = element_text(hjust = 0.5, size = 10)) 
  
  
  # format data for plotting second correlation plot
  corr2Plot <- dat.4Plots[varTracker[[i]][2]]
  names(corr2Plot[[1]]) <- cbind("Easy", "Hard", "SubjectID")
  corr2Plot.dat <- data.frame(Easy = corr2Plot[[1]][1], 
                              Hard = corr2Plot[[1]][2])
  counter = counter + 1
  
  # scatterplot
  corr2.plot = ggplot(corr2Plot.dat, aes(x = Easy, y = Hard)) +
    geom_point(shape = 1, size = 2, na.rm = TRUE, show.legend = TRUE) +
    geom_smooth(method = lm, se = TRUE) +
    labs(title = titles.names[counter], 
         subtitle = "Across Treatments", 
         x = sprintf("Easy - %s", axes.names[counter]),
         y = sprintf("Hard - %s", axes.names[counter])) +  # change text
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          plot.subtitle = element_text(hjust = 0.5, size = 10)) 
  
  # put r1 and val plots into a list
  plotting.list <- list(corr1.plot, corr2.plot)
  # use grid arrange to print plots and save as image
  fig.Title <- sprintf("genResultsCorrPlots.%d.jpg", i) # title
  jpeg(filename <- fig.Title, units = "in", height = 4, width = 8, 
       res = 300)
  grid.arrange(grobs = plotting.list, ncol = 2, widths = c(5, 5))  
  dev.off()  # close and save figures
}

# ----- Clean Up -----
# 
# End Sink
sink()
closeAllConnections() # make sure sink saves

# Detach libraries
detach(package:readxl)
detach(package:reshape2)

detach(package:lmerTest)
detach(package:lme4)
detach(package:MuMIn)
detach(package:pscl)
detach(package:car)
detach(package:MASS)

detach(package:ggplot2)
detach(package:gridExtra)
detach(package:knitr)



