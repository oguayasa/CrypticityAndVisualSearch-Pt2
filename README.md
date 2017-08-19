The Effects of Target Crypticity on Search Performance & Behaviors
=======================================================

Olivia Guayasamin
7/18/2017

Introduction
--------
This code demonstrates initial analyses to determine the effects of target crypticity (difficulty) on search performance and behaviors during a visual search task. We asked subjects to complete several rounds of a repeated visual search task where their goal was to find as many target stimuli as possible (randomly dispersed inside a large "cloud" of similar looking distractors) within a limited amount of time. The number of targets per trial was always 5, but subjects were told that it varied randomly to prevent them from being discouraged by occasional poor performance and to prevent them from ceasing searching behaviors before the time was finished. Each subject completed two blocks of trials, with one block containing only Easy versions of the task and the other containing only Hard versions. The Easy and Hard versions differed only in the [crypticity of targets](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt1), not in their size, placement, or amount. The order in which these versions were presented was counterbalanced across subjects. Subject gaze positions were recorded throughout the study using Tobii eye-trackers.

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

``` r
val.GenData <- read_excel("val_GenResultsBlockedData.xlsx")

# get rid of excess data columns
val.Data <- as.data.frame(val.GenData[, c(1, 2, 3, 4, 5, 6, 9, 10,
                                          11, 12, 17, 18, 19, 20)])
```

Chi-Square Test of Independence on Search Outcomes
==================================================

To conduct a simple *χ*<sup>2</sup> test of independence, we need first need to create a contingency table (a.k.a. a count table) that contains the number of times each type of search occured. 

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
**Table 1** Contingency Table of Search Outcome by Treatment

|      |   NumTP|   NumFN|  NumFP|  NumTN|
|------|-------:|-------:|------:|------:|
| Easy |  583.00|  157.00|  12.75|    727|
| Hard |  229.75|  506.25|  22.25|    718|

Visualize data using a bar plot
-------------------------------

While images can make things easier to understand, they can also go a long way towards describing the magnitude of results in a more meaningful way than text. For example, the table above isn't all that hard to read, so the graph below isn't really providing an easy summary of the data so much as a lasting visualization. 

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
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/SearchCountBarPlot.jpg)

Based on the contingency table, while crypticity treatment does not seem to affect the number of TN searches, there definitely seems to be an effect on the number of TP and FN searches (which makes sense, since the number of TP and FN searches are dependent), but also a small effect of the number of FP searches as well.

Chi-Square Test of Independence
-------------------------------

To formally determine if the count of search outcomes is indeed dependent on crypticity treatment, we will conduct a *χ*<sup>2</sup> Test of Independence.

``` r
cont.Results <- chisq.test(cont.Counts)

# format and print test results
cont.Cap = "Chi-Square Test Results"
kable(tidy(cont.Results), caption = cont.Cap)
```
**Table 2** Chi-Square Test Results

|  statistic|  p.value|  parameter| method                     |
|----------:|--------:|----------:|:---------------------------|
|   340.0718|        0|          3| Pearson's Chi-squared test |

The results of this test definitely support our intuition that crypticity treatment and the frequency of search outcomes are dependent. Based on the contingency table (Table 1) and barplot representation (Fig. 1), the dependence appears to come from the number of TP (and associated FN) and FP searches. When the crypticity treatment is Hard, there were fewer successful searches and more false-positive outcomes.

While not included in the discussion above, prior examination of the data showed a large number of missing values for FP searches. This is because subjects made very few false positive identifications. For the sake of brevity, we will also omit FN searches from this tutorial. Therefore, analysis in this example will include data from TP and TN search, but not FP and FN ones.

Using GLMMs to Determine Treatment Effects
==========================================

To determine the effect of crypticity treatment (Easy vs. Hard) on our search performance and behavior measures, we will essentially be conducting a Repeated Measures ANOVA analysis using Generalized Linear Mixed Models. There are several reasons why GLMMs can be a better alternative to Repeated Measures ANOVAs. We won't spend time discussing that here, but if you are interested in knowing more, the Analysis Factor blog gives a nice overview [here](http://www.theanalysisfactor.com/advantages-of-repeated-measures-anova-as-a-mixed-model/). Long story short, you get more interpretive bang for your buck with a GLMM, whose results enable you to answer a wider variety of questions than can be done with the output of an ANOVA.

Check data distributions prior to analysis
------------------------------------------

This is so that we know how to properly specify our GLMMs in our code. Here, we will visualize data distributions using histograms and compare them to a normal distribution using qq-plots. 

**Figure 2** Number of TP searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsDistCheck.2.jpg)


**Figure 3** Number of TN searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsDistCheck.3.jpg)


**Figure 4** Average Pupil Size and Total Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsDistCheck.1.jpg)


Given the fact that NumTP is are count data (Figure 2), it is not surprising that they do not follow a normal distribution. To model these two measures, we will apply GLMMs with a Possion family (count data) and a log link function. Our other dependent variables (Figures 3 & 4) are continuous, but many of the distributions are not normal (which is pretty normal for psychological and biological behavioral studies). For these measures, our GLMMs will be specified with a Gaussian family (to deal with continuous data) with an identify link function. Since none of our distributions are "normal", we will estimate our model parameters using Laplace approximation instead of any Maximum Likelihood approaches.

Run GLMMs and review output
---------------------------

To save time and space, only the output from one GLMM will be shown. However, all of the boxplots showing treatment comparisons will be included, so all general results will eventually be summarized. This example will show the GLMM output and subsequent analysis for the average duration of True-Positive searches.

While most counterbalance designs would include an interaction term for the fixed effects (to see if there is an interaction between a treatment and the order in which it occured), there is not one included here. Previously, we checked for a significant interaction and found none, so it has been removed from the final model shown below. In addition, sub-block order and subject ID have been included as random effects to account for any variation due to time or individual.

``` r
# reformat data for applying and plotting glmm
dat.4Analysis <- as.data.frame(val.Data[, c(1, 2, 3, 4, 5, 6, 7, 10, 11, 12)])
cur.Data <- as.data.frame(dat.4Analysis[, c(1, 2, 3, 4, 9)])
cur.Data$TreatmentOrder <- factor(cur.Data$TreatmentOrder)  # convert to factor
cur.Data$BlockOrder <- factor(cur.Data$BlockOrder)  # convert to factor

# name of measure
cur.Name <- names(cur.Data[5])

# create model
cur.Glmm <- glmer(TimePerTP ~ ExpVer + TreatmentOrder + (1|BlockOrder) + (1 + ExpVer|PartID),
                  data = cur.Data, family = gaussian(link = identity))
```
**Table 3** Estimates of Fixed and Random Effects. There is no std.error or statistic for the random effects because this particular function does not estimate them.

| term                               |       estimate|  std.error|  statistic| group      |
|:-----------------------------------|--------------:|----------:|----------:|:-----------|
| (Intercept)                        |   4348.5868139|   177.6400|   24.47978| fixed      |
| ExpVermost                         |  -1413.3563530|   134.5622|  -10.50337| fixed      |
| TreatmentOrder2                    |    147.7201942|   124.5050|    1.18646| fixed      |
| sd\_(Intercept).PartID             |    896.7876663|         NA|         NA| PartID     |
| sd\_ExpVermost.PartID              |    603.0722333|         NA|         NA| PartID     |
| cor\_(Intercept).ExpVermost.PartID |     -0.7637152|         NA|         NA| PartID     |
| sd\_(Intercept).BlockOrder         |     99.4459871|         NA|         NA| BlockOrder |
| sd\_Observation.Residual           |    775.9416965|         NA|         NA| Residual   |

Here, we see the basic estimates of each effect's contribution to the model. Because we only get limited output from this function, it doesn't tell us very much. To actually learn if each effect made a significant contribution, let's estimate some confidence intervals. 

**Table 4** Bootstrapped confidence intervals for the estimates of Fixed and Random Effects

| .rownames                          |      X0.5..|        X99.5..|
|:-----------------------------------|-----------:|--------------:|
| sd\_(Intercept) PartID             |    568.3818|   1219.6829182|
| sd\_(Intercept) BlockOrder         |      0.0000|    289.2539580|
| sigma                              |    681.8362|    874.4833442|
| (Intercept)                        |   3944.7234|   4800.5154395|
| ExpVermost                         |  -1744.8809|  -1109.9880952|
| TreatmentOrder2                    |   -240.0648|    480.3760089|

**Table 5** Comparing the effects of Easy and Hard crypticity treatments

|      | ExpVer |  Estimate|  Standard Error|    DF|  t-value|  Lower CI|  Upper CI| 
|------|:-------|---------:|---------------:|-----:|--------:|---------:|---------:|
| Hard | least  |  4422.447|        168.3409|  31.7|    26.27|  4079.430|  4765.463|
| Easy | most   |  3009.091|        125.7184|  21.2|    23.94|  2747.816|  3270.365|

While neither table explicity gives a p-value, they give 99.5% confidence interval ranges, from which we can infer significance. We can see that crypticity treatment was a significant effect in our model (Table 4), because it's confidence interval does not include zero, and we can also see that there was a significant difference between True-Positive search durations across the Easy and Hard treatments (Table 5) demonstrated by the fact that confidence intervals for the two treatments do not overlap. 

**Table 6** Comparing the effects of Block Order

|         | TreatmentOrder |  Estimate|  Standard Error|    DF|  t-value|  Lower CI|  Upper CI|
|---------|:---------------|---------:|---------------:|-----:|--------:|---------:|---------:|
| Block 1 | 1              |  3641.909|        146.3069|  33.9|    24.89|  3344.530|  3939.287|
| Block 2 | 2              |  3789.629|        146.4081|  33.9|    25.88|  3492.074|  4087.183|

Treatment order is not a significant predictor of True-Positive search durations (Table 4), shown by a confidence interval that overlaps zero. Additionally,there was not significant difference in True-Positive search durations across the first and second treatment blocks (Table 5), demonstrated by the fact their confidence intervals overlap. 

**Table 7** Check to see if Random Effects contribute to the model fit

|               |     Chi.sq|  Chi.DF|    p.value|
|---------------|----------:|-------:|----------:|
| BlockOrder    |   1.219254|       1|  0.2695071|
| PartID        |  16.899378|       2|  0.0002140|

From the confidence intervals (Table 4), we can see that all of the random effects are siginificant with the exception of sub-block order. In addition, tests of model fit (Table 7) shows that include a random effect for sub-block order did not significantly improve the model (compared to a model with no such random effect), but by including an intercept for each subject we have accounted for a significant amount of variation in the model and yielded a model with a significantly better fit (compared to a model where those random effects had not been included). 

**Table 8** Get approximate measures of variance explained by estimate Marginal and Conditional R<sup>2</sup>

| names |          x|
|:------|----------:|
| R2m   |  0.2959030|
| R2c   |  0.6420232|

This conclusion about the importance of the random effects in our model is bolstered by checking the Marginal and Conditional R<sup>2</sup> values. These non-standard R<sup>2</sup>'s are useful for approximating the amount of variation in our data set explained by the model. Marginal R<sup>2</sup> estimates how much variance is accounted for by just the fixed effects in our model, while Conditional R<sup>2</sup> estimate the variance explained by a model with fixed *and* random effects. We can see that considerably more variance is explained with the inclusion of random effects (Table 8). 

**Figure 5** Checking the distribution of model residuals to make sure they approximate normality (suggesting a correct model specification). Fortunately, it looks like we did an OK job.

![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/modelResid.TimePerTP.jpg)

In brief, crypticity treatment was a significant predictor of the duration of True-Positive searches, with searches in the Hard condition taking significantly more time on average. In contrast, treatment order did not significantly affect True-Positive search durations. While including time within treatment (sub-block order) did not explain any additional variation in our model, accounting for the effects of individual subject certainly did. This indicates that for True-Positive search times, subjects varied amongst each other quite a bit, with some people completing TP searches much more quickly than others. 

Determine the behavioral consistency of subjects within treatments
------------------------------------------------------------------
Now, let's see how consisitent subjects were in TP search time, and if this consistency was affected by difficulty, using the intraclass correlation coefficient (ICC). The ICC that will be applied here is the ICC 2, because I am considering sub-block order (judge) as a random effect. This ICC measures subject rank consistency across samples, while controlling for the effects of varying group means. It is not an absolute measure of agreement. Search duration for TP searches will once again be used as an example.

**Table 9** ICC estimates for the duration of True-Positive searches completed during the Easy and Hard crypticity treatments. 

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

Looking at the values for the "ICC", "p", "lower.bound", and "upper.bound" rows for the Easy and Hard crypticity treatments (Table 9), we can see that subjects are significantly consistent in their behavior during both treatments. Interestingly however, it does suggest that increasing target crypticity causes a reduction in subject behavioral consistency. 

Boxplots to visualize treatments effects
----------------------------------------

To visualize average treatment effects both across and within subjects, we will use boxplots overlaid on top of individual lines representing subject averages across treatments. Below each boxplot figure, we will include a histogram showing the distribution of within subject differences across treatments. While including the histogram is not necessary (technically people could figure out treatment differences by looking at the boxplot), it does make this difference much easier to visualize. 

These figures will include data from all measures so that we can discuss results outside of those in the examples given above.

**Figure 6** Number of TP searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsBarPlots.2.jpg)

There is a big effect of crypticity treatment on number and average duration of True-Positive searches. All subjects found fewer targets during the Hard crypticity treatment, and for almost every subject those searches took longer in the Hard treatment as well. 

**Figure 7** Number of TN searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsBarPlots.3.jpg)

Crypticity treatment does not appear to affect the number of True-Negative searches, but it certainly affected how long it took to correctly dismiss a distractor stimuli, with True-Negative searches almost always lasting longer in the Hard crypticity treatment. 

**Figure 8** Average Pupil Size and Total Search Distance
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsBarPlots.1.jpg)

There is no apparent effects of crypticity treatment on average pupil size or scanpath length, suggesting that the Hard treatment did not cause greater cognitive load or increase movement expenditure during search. However, increasing target crypticity definitely affected subject's ability to find targets, increased the amount of time needed to find targets, and made it more difficult to dismiss distractors. 

Correlation Analysis Across Treatments
======================================

While the ICC analysis conducted above showed that subjects behave consistently within treatments, it also important to know whether subjects were consistent across treatments as well to see if the treatment affected all subjects equivalently. First, we will take subject averages for all measures collected during the Easy and Hard treatments so that the data is properly structured for correlation analysis.

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
**Tables 10-15** Results of correlation analysis using Spearman's *ρ* to determine the relationship between behaviors during Easy and Hard crypticity Treatments. 

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


Scatterplots to view relationships
----------------------------------

Before we get into interpreting our results, let's first visualize these correlations using scatterplots overlaid with a line of best fit.

**Figure 9** Number of TP searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsCorrPlots.2.jpg)


**Figure 10** Number of TN searches and Average Search Duration.
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsCorrPlots.3.jpg)


**Figure 11** Average Pupil Size and Total Search Distance
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2/blob/master/imgs/genResultsCorrPlots.1.jpg)


Interestingly, all of the correlations are moderate to strong, positive, and significant with the exception of NumTP, the number of True-Positive searches (Table 5). While all subjects did complete more TP searches in the Easy vs Hard crypticity treatment (Figure 6), subject ranking must have changed significantly across treatments. The scatterplot showing the number of True-Positive searches (Figure 9) helps explain the low correlation value. For example, in Figure 9, it is easy to see that subjects who found the most targets during the Easy treatment, were not the same individuals who found the most targets during the Hard treatment, 

Conclusions
===========

From this data set, we were able to learn that target crypiticty (search difficulty) affects the ratios of search success and errors, the number of True-Positive searches (targets successfully found), and subject efficiency at recognizing targets and dismissing distractor stimuli. While subjects varied quite a bit from each other, they usually held their relative ranks across difficulty treatments. For example, those who took longer to perform TP searches relative to their peers in the Easy Treatment also did so in the Hard Treatment. In addition, we learned that subject consistency was affected by difficulty, which subjects performing more variably during the difficult search trials. 

We now have a good understanding of how search difficulty affects search performance and efficiency, but which search behaviors (gaze patterns and actions) are most affected by search difficulty?
