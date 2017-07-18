The Effects of Target Crypticity on Search Performance & Behaviors
=======================================================

Olivia Guayasamin
7/18/2017

Introduction
--------
This code demonstrate initial analyses to determine the effects of target crypticity (difficulty) on search performance and behaviors during a visual search task. We asked subjects to complete several rounds of a repeated visual search task where their goal was to find as many target stimuli, randomly placed withing a large "cloud" of similar looking distractors, as possible within a limited amount of time. The number of targets per trial was always 5, but subjects were told that it varied randomly. Each subject completed two blocks of trials, with one block containing only Easy versions of the task and the other containing only Hard versions. The Easy and Hard versions differed only in the [crypticity of targets](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt1), not in their size, placement, or amount. The order in which these versions were presented was counterbalanced across subjects. Subject gaze positions were recorded throughout the study using Tobii eye-trackers.

To get a more complete understanding of how target crypticity affects search performance and behaviors, we decided to analyze the frequency and properties of all searches, not just the successful ones. To do this, we first classified each search based on its outcome: True Positives **TP** were searches where the target was successfully identified, True Negatives **TN** describe searches where a distractor was correctly left alone, False Positives **FP** were searches where a distractor was incorrectly identifed as a target, and False Negatives **FN** describe searches where a target went unidentified (FP and TN searches were extracted from a subset of 5 randomly selected distractors). After classifying searches into different "types" based on outcome, we determined the number and duration of every search type from each trial. In addition, we also extracted the total search distance (length of scan path) and average pupil size from each trial.

The current analysis is inteded to address the following quesitons about target crypticity: *What is the effect of target crypticity on the frequency of search outcomes?*, *search effort as measured by duration of each search type?*, *search expenditure as approximated by total distance travelled during the task?*, and *cognitive load as measured by average pupil size?*. Because of our within-subject experimental design we can also address, *Do subjects behave consistently within and* *across difficulty treatments?* in regards to all measures mentioned above.

The data file is organized by subject, target crypticity treatment, and the order in which the treatment was presented. To control for any possible differences due to effects from the exact stimuli used (which varied across trials), all measures represent averages from sub-blocks of 4 sequential trials with each treatment block. To determine if target crypticity treatment affected the frequency of different search types, we ran a *χ*<sup>2</sup> [Test](http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence) of Independence. The effects of target crypticity on the above mentioned measures was determined with [GLMMs](http://www.theanalysisfactor.com/advantages-of-repeated-measures-anova-as-a-mixed-model/) containing treatment and treatment order as fixed effects, and individual subject and sub-block order as random effects. Subject consistency within and across crypticity treatments was estimated using Intraclass Correlation Coefficients ([ICC](http://www.theanalysisfactor.com/the-intraclass-correlation-coefficient-in-mixed-models/))and correlation analysis with [Spearman's Rho](https://www.r-bloggers.com/spearmans-rho/) respectiviely. Summaries, comparisons, and relationships are visualized with tables, boxplots, and scatterplots.

GLMM were conducted using the [lme4](https://cran.r-project.org/web/packages/lme4/lme4.pdf) package and further analyzed using functions from the [car](https://cran.r-project.org/web/packages/car/car.pdf), [lmerTest](https://cran.r-project.org/web/packages/lmerTest/lmerTest.pdf), [MuMIn](https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf), and [pscl](https://cran.r-project.org/web/packages/pscl/pscl.pdf) packages. ICC values were estimated using the [psych](https://cran.r-project.org/web/packages/psych/psych.pdf) package, and the *χ*<sup>2</sup> and correlation analyses were performed using the [MASS](https://cran.r-project.org/web/packages/MASS/MASS.pdf) package.

**Summary:** Conduct chi-squared test of independence to determine if search difficulty and the frequency of different search outcomes are related. Use GLMMS to determine how search difficulty affects search performance and behavior. Run ICC and correlation analysis to determine how consistently subjects performed within and across difficulty treatments respectively. **Requires:** Data file, mentioned R libraries. **Outputs:** Results of Chi-square, GLMM, ICC, and correlation analysis. Figures visualizing data/analysis as jpg files.

Initializing Steps
==================

Load libraries from CRAN
------------------------

``` r
# read and work with data files
library(readxl)  
library(reshape2)   

# glmm analysis
library(lme4)
library(lmerTest)
library(MuMIn)
library(pscl)

# stats functions
library(psych)
library(car)  
library(MASS)

# format figures and output
library(ggplot2)
library(grid)
library(gridExtra)  
library(knitr) 
library(broom)
```

Create custom functions
-----------------------

``` r
# quick function for getting legend from a ggplot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
```

Import and format data files
----------------------------

Chi-Square Test of Independence on Search Outcomes
==================================================

Determine if the frequency of different search types (TP, TN, FP, FN) is independent from target crypticity treatment (Easy & Hard).

Format data into a contingency table
------------------------------------

``` r
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


# plot a contingency table in mkd
kable(cont.Counts,  results = 'asis', 
      caption = "Contingency Table of Search Outcome by Treatment", 
      digits = 2)
```

|      |   NumTP|   NumFN|  NumFP|  NumTN|
|------|-------:|-------:|------:|------:|
| Easy |  583.00|  157.00|  12.75|    727|
| Hard |  229.75|  506.25|  22.25|    718|

Visualize data using a bar plot
-------------------------------

``` r
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
```

**Figure 1** A barplot showing the frequencies of different search types according to crypticity treatment (bar colors). Numbers are not whole because they represent the sums of the count averages taken from each sub-block of four trials.

![](https://github.com/oguayasa/SearchDifficultyValidation-Pt2/blob/master/imgs/SearchCountBarPlot.jpg)


Based on the contingency table, while crypticity treatment does not seem to affect the number of FP and TN searches, there definitely seems to be an effect on the number of TP and FN searches (which makes sense, since the number of TP and FN searches are related).

Chi-Square Test of Independence
-------------------------------

To formally determine if the count of search outcomes is indeed dependent on crypticity treatment, we will conduct a *χ*<sup>2</sup> Test of Independence.

``` r
cont.Results <- chisq.test(cont.Counts)

# format and print test results
cont.Cap = "Chi-Square Test Results"
kable(tidy(cont.Results), caption = cont.Cap)
```

|  statistic|  p.value|  parameter| method                     |
|----------:|--------:|----------:|:---------------------------|
|   340.0718|        0|          3| Pearson's Chi-squared test |

The results of this test definitely support our intuition that crypticity treatment and the frequency of search outcomes are dependent. Based on the contingency table (Table 1) and barplot representation (Fig. 1), the dependence appears to come from the number of TP (and associated FN) and FP searches. When the crypticity treatment is Hard, there were fewer successful searches and more false-positive outcomes.

While not included in the discussion above, there are large amounts of missing data for FN and FP searches. This is because many subjects were able to find all 5 targets in a single trial (yielding no false-negative searches for that trial), and the fact that false-positives were incredibly rare events. Therefore, latter analysis will include TP and TN searches, but not FP and FN.

Using GLMMs to Determine Treatment Effects
==========================================

To determine the effect of crypticity treatment (Easy vs. Hard) on our search performance and behavior measures, we will essentially be conducting a Repeated Measures ANOVA analysis using Generalized Linear Mixed Models. There are several reasons why GLMMs can be a better alternative to Repeated Measures ANOVAs. We won't spend time discussing that here, but if you are interested in knowing more, the Analysis Factor blog gives a nice overview [here](http://www.theanalysisfactor.com/advantages-of-repeated-measures-anova-as-a-mixed-model/).

Check data distributions prior to analysis
------------------------------------------

This is so that we know how to properly specify our GLMMs in our code. Here, we will visualize data distributions using histograms and compare them to a normal distribution using qq-plots.

**Figure 2** Number of TP searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsDistCheck.2.jpg)


**Figure 3** Number of TN searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsDistCheck.3.jpg)


**Figure 4** Average Pupil Size and Total Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsDistCheck.1.jpg)


Given the fact that NumTP and NumTN are count data (Figure 2), it is not surprising that they do not follow a normal distribution. To model these two measures, we will apply GLMMs with a Possion family(count data) and a log link function. Our other dependent variables (Figures 3 & 4)are continuous, but many of the distributions are not normal (which is pretty normal for psychological and biological behavioral studies). For these measures, our GLMMs will be specified with a Gaussian family (to deal with continuous data) with an identify link function. Since none of our distributions are "normal", we will but estimate our model parameters using Laplace approximation instead of any Maximum Likelihood approaches.

Run GLMMs and review output
---------------------------

To save time and space, only the output from one GLMM will be shown. However, all of the boxplots showing treatment comparisons will be included, and the results from all of the GLMMs will be summarized there. This example will show the GLMM output and subsequent analysis for the average duration of True Positive searches.

| term                       |     estimate|  std.error|   statistic| group      |
|:---------------------------|------------:|----------:|-----------:|:-----------|
| (Intercept)                |   4223.23265|   200.1265|   21.102811| fixed      |
| ExpVermost                 |  -1409.32809|    97.9753|  -14.384524| fixed      |
| TreatmentOrder             |    130.58705|    97.9753|    1.332857| fixed      |
| sd\_(Intercept).PartID     |    684.86343|         NA|          NA| PartID     |
| sd\_(Intercept).BlockOrder |     95.07958|         NA|          NA| BlockOrder |
| sd\_Observation.Residual   |    838.15382|         NA|          NA| Residual   |

| .rownames                  |       X2.5..|     X97.5..|
|:---------------------------|------------:|-----------:|
| sd\_(Intercept)|PartID     |    470.22007|    867.0899|
| sd\_(Intercept)|BlockOrder |      0.00000|    236.7538|
| sigma                      |    763.34405|    916.5541|
| (Intercept)                |   3823.35210|   4592.0703|
| ExpVermost                 |  -1623.04932|  -1209.3974|
| TreatmentOrder             |    -75.04587|    303.0219|

|              | ExpVer |  Estimate|  Standard Error|    DF|  t-value|  Lower CI|  Upper CI|  p-value|
|--------------|:-------|---------:|---------------:|-----:|--------:|---------:|---------:|--------:|
| ExpVer least | least  |  4418.892|        140.4895|  28.9|    31.45|  4131.511|  4706.273|        0|
| ExpVer most  | most   |  3009.564|        140.3584|  28.8|    21.44|  2722.405|  3296.722|        0|

|            |      Chi.sq|  Chi.DF|    p.value|
|------------|-----------:|-------:|----------:|
| BlockOrder |   0.8261084|       1|  0.3634002|
| PartID     |  80.9390309|       1|  0.0000000|

| names |          x|
|:------|----------:|
| R2m   |  0.2952882|
| R2c   |  0.5806626|

**Figure 5** Residuals.

Describe the results from one of them.

Determine the behavioral consistency of subjects within treatments
------------------------------------------------------------------

The intraclass correlation coefficent (ICC) that will be applied here is the ICC 2, because I am considering sub-block order (judge) as a random effect. This ICC measures subject rank consistency across samples, while controlling for the effects of varying group means. It is not an absolute measure of agreement. Search duration for TP searches will once again be used as an example.

|             | V1        | V2           |
|-------------|:----------|:-------------|
| Treatment   | Easy      | Hard         |
| type        | ICC2      | ICC2         |
| ICC         | 0.7133721 | 0.4073689    |
| F           | 11.36059  | 3.784846     |
| df1         | 36        | 35           |
| df2         | 108       | 105          |
| p           | 0         | 7.273653e-08 |
| lower.bound | 0.6416578 | 0.3089165    |
| upper.bound | 0.7817393 | 0.5161467    |

Looking at the values for the "ICC", "p", "lower.bound", and "upper.bound" rows for the Easy and Hard crypticity treatments (Table 2), we can see that subjects are significantly consistent in their behavior during both treatments. Interestingly however, it does appear that subjects were less consistent in during the Hard treatment than during the Easy treatment, suggesting that increasing target crypticity results in greater within-subject behavioral variance.

Boxplots to visualize treatments effects
----------------------------------------

To visualize treatment effects and subject variation, we will use boxplots overlaid on top of individual lines representing subject averages across treatments. Below each boxplot figure, we will include a histogram showing the distribution of within subject differences across treatments. These figures will include data from all measures, so that we can discuss results from the measures that were not used as examples for the GLMM and ICC analyses.

**Figure 6** Number of TP searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsBarPlots.2.jpg)


**Figure 7** Number of TN searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsBarPlots.3.jpg)


**Figure 8** Average Pupil Size and Total Search Distance
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsBarPlots.1.jpg)

Discuss Results

Correlation Analysis Across Treatments
======================================

While the ICC analysis conduction above showed that subjects behave consistently within treatments, it also important to know whether subjects were consistent across treatments as well to see if the treatment affected all subjects equivalently. First, we will take subject averages for all measures collected during the Easy and Hard treatments.

Averages of subject behavior
----------------------------

``` r
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
```

Correlation analysis
--------------------

Then we will run correlate the subject averages across treatments using Spearman's *ρ*, a rank-based correlation statistic for use with non-parametric data distributions.

``` r
# calculate correlations for data from each experimental group
for (i in 1:length(dat.4Corr)){
  # run spearmans correlation
  corr.res = cor.test(dat.4Corr[[i]][, 1], dat.4Corr[[i]][, 2],
                      method = "spearman")  
  # print with nice format
  corr.4Print = cbind(names(dat.4Corr[[i]][1]), 
                      names(dat.4Corr[[i]][2]),
                      corr.res$estimate, 
                      corr.res$p.value)
  # change column names
  colnames(corr.4Print) = cbind("Var1", "Var2", "Corr-Value", "P-Value")
  # make table caption
  tab.Cap = sprintf("Correlation Results for %s", names(dat.4Corr[i]))
  # print for immediate output
  print(kable(corr.4Print, results = 'asis', caption = tab.Cap, digits = 2))
}
```

|     | Var1            | Var2            | Corr-Value        | P-Value |
|-----|:----------------|:----------------|:------------------|:--------|
| rho | Easy\_PupilSize | Hard\_PupilSize | 0.930535798956851 | 0       |

|     | Var1                 | Var2                 | Corr-Value        | P-Value              |
|-----|:---------------------|:---------------------|:------------------|:---------------------|
| rho | Easy\_ScanPathLength | Hard\_ScanPathLength | 0.744902797534376 | 6.45658152246944e-07 |

|     | Var1        | Var2        | Corr-Value        | P-Value           |
|-----|:------------|:------------|:------------------|:------------------|
| rho | Easy\_NumTP | Hard\_NumTP | 0.189292136274213 | 0.261828014241431 |

|     | Var1        | Var2        | Corr-Value        | P-Value             |
|-----|:------------|:------------|:------------------|:--------------------|
| rho | Easy\_NumTN | Hard\_NumTN | 0.449294882518318 | 0.00527709365326942 |

|     | Var1            | Var2            | Corr-Value        | P-Value              |
|-----|:----------------|:----------------|:------------------|:---------------------|
| rho | Easy\_TimePerTP | Hard\_TimePerTP | 0.541218869773786 | 0.000653281399756486 |

|     | Var1            | Var2            | Corr-Value        | P-Value             |
|-----|:----------------|:----------------|:------------------|:--------------------|
| rho | Easy\_TimePerTN | Hard\_TimePerTN | 0.449739212897108 | 0.00564131406661042 |

Interestingly, all of the correlations are moderate to strong, positive, and significant with the exception of NumTP, the number of true-positive searches (Table 5). While all subjects did complete more TP searches in the Easy vs Hard crypticity treatment (Figure 6), subject ranking must have changed significantly across treatments. For example, the individuals who found the most targets during the Easy treatment, were not the same individuals who found the most targets during the Hard treatment.

Scatterplots to view relationships
----------------------------------

Let's visualize these correlations using scatterplots overlaid with a line of best fit.

**Figure 9** Number of TP searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsCorrPlots.2.jpg)


**Figure 10** Number of TN searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsCorrPlots.3.jpg)


**Figure 11** Average Pupil Size and Total Search Distance
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsCorrPlots.1.jpg)

The lack of correlation for the number of true-positive searches (Figure 10), is quite apparent.

Summary
=======

TBC
