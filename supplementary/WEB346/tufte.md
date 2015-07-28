---
title: "WEB346"
author: "Benjamin Weigel"
date: "July 27th, 2015"
output: rmarkdown::tufte_handout
---



\listoffigures

# Question

Do Mg\textsuperscript{2+}, pH, chemical motif (e.g. catecholic, phenolic, 3'-OMe, 4'-OMe) and the choice of enzyme (WT or 3'-variant) influence the observed conversion of flavonoids and phenylpropanoids by the O-methyltransferase PFOMT?

# Introduction

17 different flavonoid and phenyl propanoid substrates were tested for methylation.
These substrates can loosely be categorized into four groups by the chemical motif that is to be methylated (e.g. catecholic, phenolic, 3'-OMe, 4'-OMe).
Three other factors are studied that might also influence the conversion. The addition of Mg\textsuperscript{2+}, high (8.6) or low (7.5) pH and the enzyme variant (WT or 3'-variant) that is used are also varied.

\newthought{A total of 96 experiments} were conducted to cover each group at least with three independent observations. 

\begin{marginfigure}
$$\mathrm{Mg} \times \mathrm{pH} \times \mathrm{variant} \times \mathrm{motif} $$
$$2 \times 2 \times 2 \times 4 = 32$$
\caption{number of groups studied}
\end{marginfigure}

# Results

## Data

\begin{table}[ht]
\centering
\begin{tabular}{rlrllll}
  \toprule
 & substrate & value & pH & Mg & variant & motif \\ 
  \midrule
1 & 1 & 0.00 & low & FALSE & WT & A \\ 
  2 & 1 & 0.00 & high & FALSE & Y51RN202W & A \\ 
  3 & 1 & 0.07 & high & TRUE & WT & A \\ 
  4 & 1 & 0.00 & low & TRUE & Y51RN202W & A \\ 
  5 & 1 & 0.04 & high & FALSE & WT & A \\ 
  6 & 1 & 0.00 & low & FALSE & Y51RN202W & A \\ 
   \bottomrule
\end{tabular}
\caption{First rows of dataframe} 
\end{table}

\begin{margintable}
\centering
\begin{tabular}{rll}
  \toprule
 & labels & motif \\ 
  \midrule
1 & A & phenolic \\ 
  2 & B & catecholic \\ 
  3 & C & 4-O-Me \\ 
  4 & D & 3-O-Me \\ 
   \bottomrule
\end{tabular}
\caption{Labels in the data.frame and their corresponding motif.} 
\end{margintable}

## Main effects plots

The main effects plots give an overview of what happens. The motif clearly has the biggest influence on conversion. This makes sense, since PFOMT acts on catecholic moities.

![Main effetcs plots for the factor variables. Clearly the motif seems to have the biggest impact. Catecholic moieties are converted most effectively.](figure/unnamed-chunk-4-1.pdf) 

\newthought{The interaction plot}\ref{} of Mg and pH displays an interaction. When magnesium is added the activity tends to be higher. This effect is more pronounced at low pH values. 
It is not possible to say wether this effect is significant without the according satistical test.


![Interaction plot for Mg and pH. The lines suggest an interaction between pH and Mg, but this is not enough evidence to say wether that interaction is significant. red -- no magnesium, blue -- magnesium added.](figure/unnamed-chunk-5-1.pdf) 


## Regression Tree

A regression tree can be built from the data. At first glance it shows, that the motif is especially important for conversion.
This is trivial, since PFOMT is a 4'-OMT that acts on catecholic motifs. The substrates with catecholic motifs are thus converted more efficiently.

\newthought{It seems as if the variant} is influenced by pH and metal addition more clearly thatn the wt-enzyme, since the tree splits up more.

![RegressionTree of the data built with the `rpart`-package. The motif effects the conversion most. It seems that only the variant is influenced by pH and Mg.](figure/unnamed-chunk-6-1.pdf) 

## Linear Models

At first it was checked, if pH and Mg have an influence on the conversion.
It does not seem that Mg or pH have any influence on the conversion, as the p-values are much too high.
This could be the result of the fact that the conversion reactions were incubated for 16 hours.
Possible intricacies in the conversion (due to different reaction velocities) can not be distinguished from one another if the reaction time is so long that all substrate is used up.

\begin{table}[ht]
\centering
\begin{tabular}{rrrrr}
  \toprule
 & Estimate & Std. Error & t value & Pr($>$$|$t$|$) \\ 
  \midrule
(Intercept) & 0.2711 & 0.0855 & 3.17 & 0.0021 \\ 
  pHlow & -0.0969 & 0.1209 & -0.80 & 0.4247 \\ 
  MgTRUE & 0.0299 & 0.1209 & 0.25 & 0.8051 \\ 
  pHlow:MgTRUE & 0.0604 & 0.1709 & 0.35 & 0.7247 \\ 
   \bottomrule
\end{tabular}
\caption{A linear model with pH and Mg as factors is not a lot better than random guesses. p-values are very high. Thus this alone does not explain the variance.} 
\end{table}

\newthought{However, when the motif is included} as a variable it becomes clear that Mg and pH DO NOT influence the conversion.



 \marginnote{This is a margin note.  Notice that there isn't a number preceding the note.}


![RegressionTree of the data of only substrates with phenolic, 3'-OMe and 4'-OMe moieties. Catecholic substrates are omitted.](figure/unnamed-chunk-9-1.pdf) 

## Model selection using lasso regression

Lasso regression can shrink model coefficients to zero. This helps in variable selection.
Variables that were shrunken to 0 tend to have little impact on the prediction capabilities of the model.

![Lasso regression on the model.](figure/unnamed-chunk-10-1.pdf) 


## Cross-validation to select best model

After cross-validation only 11 variuables have non-zero coefficients and are thus used during prediction.
All other variabkes were shrunken to zero.


```r
x<-sparse.model.matrix(value~pH*Mg*variant*motif, data = data)
y <- data[,2]
cv.lasso <- cv.glmnet(x,y,alpha=1,nfolds=5)
```


![Cross validation results for lasso regression. The best model only needs around 10 variables to describe the data with low error.](figure/unnamed-chunk-12-1.pdf) ![The variables that have non-zero coefficients.](figure/unnamed-chunk-12-2.pdf) \begin{table}[ht]
\centering
\begin{tabular}{rlr}
  \toprule
 & variable & coefficient \\ 
  \midrule
1 & (Intercept) & 0.0180 \\ 
  2 & pHlow & -0.0160 \\ 
  3 & variantWT & 0.0256 \\ 
  4 & motifB & 0.8658 \\ 
  5 & MgTRUE:motifD & 0.0011 \\ 
  6 & variantWT:motifB & 0.1615 \\ 
  7 & pHlow:MgTRUE:motifB & 0.0026 \\ 
  8 & pHlow:variantWT:motifB & 0.0094 \\ 
  9 & pHlow:variantY51RN202W:motifB & -0.5332 \\ 
  10 & MgTRUE:variantY51RN202W:motifB & 0.1096 \\ 
  11 & MgTRUE:variantWT:motifC & 0.0653 \\ 
  12 & MgTRUE:variantWT:motifD & 0.0490 \\ 
  13 & pHlow:MgTRUE:variantY51RN202W:motifB & 0.3724 \\ 
   \bottomrule
\end{tabular}
\caption{Variables and coefficients that were retained. Non-zero coefficients not shown.} 
\end{table}


