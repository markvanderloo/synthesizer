<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Introduction to synthesizer}
-->

---
title: Introduction to `synthesizer`
author: Mark P.J. van der Loo
css: style.css
---

Package version `packageVersion("synthesizer")`{.R}. Please use `citation('synthesizer')` to cite the package.

## Introduction

`synthetiser` is an R package for quickly and easily synthesizing data.  It
also provides a few basic functions based on pMSE to measure some utility of
the synthesized data.

The package supports numerical, categorical/ordinal, and mixed data, it
synthesizes times series (`ts`) objects and also correctly takes account of
missing values and mixed (or zero-inflated) distributions.  A `rankcor`
parameter lets you gradually shift between realistic data with high utility and
less realistic data with decreased correlations between original and syntesized
data.


## Installation

The latest CRAN release can be installed as follows.
```
install.packages("synthesizer")
```
Next, the package can be loaded. You can use `packageVersion` (from base R) to
check which version you have installed.
```{#load_package .R}
library(synthesizer)
# check the package version
packageVersion("synthesizer")
```

## A first example

We will use the `iris` dataset, that is built into R.

```{#load_iris .R}
data(iris)
head(iris)
```

Creating a synthetic version of this dataset is easy.

```{#synthesize_iris .R}
set.seed(1)
synth_iris <- synthesize(iris)
```

To compare the datasets we can make some side-by-side scatterplots.

```{#plot .R  fun=output_figure name="test" caption="Original and Synthesized Iris" device="png" width=800 height=400}
oldpar <- par(mfrow=c(1,2))
plot(Sepal.Length ~ Petal.Length, data=iris, col=iris$Species, pch=16,las=1,xlim=c(0,7),ylim=c(4,8),main="Original")
legend("topleft",legend=levels(iris$Species),col=1:3,pch=16,bty="n")
plot(Sepal.Length ~ Petal.Length, data=synth_iris, col=iris$Species,pch=16,las=1,xlim=c(0,7),ylim=c(4,8),main="Synthesized")
legend("topleft",legend=levels(iris$Species),col=1:3,pch=16,bty="n")
par(oldpar)
```

By default `synthesize` will return a dataset of the same size as the input
dataset. However, it is possible to ask for any number of records.

```{#synthesize_more .R}
more_synth <- synthesize(iris, n=250)
dim(more_synth)
```

## Controlling the utility-privacy trade-off

Synthetic data can be too realistic, in the sense that it might reveal actual
properties of the original entities used to create the synthetic data. One way
to mitigate this is to decrease the rank correlation between the original and
the synthetic data.

When synthesizing data frames this can be controlled with the `rankcor`
parameter. This parameter varies from 0, representing the lowest utility, to 1,
the default and maximum utility.  The `rankcor` refers to the maximum rank
correlation between original and synthesized variables. If `rankcor` is a
single (unnamed) value, all synthetic variables are rank-decorrelated from the
original data by random permutations until the rank correlation between
synthetic and original data drops below the `rankcor` value. It is also
possible to lower the utility of a selection of variables. Variables for which
`rankcor` is not specified will default to perfect rank correlation
(`rankcor=1`).
```{#decorrelate .R}
# decorrelate rank matching to 0.5
s1 <- synthesize(iris, rankcor=0.5)
# decorrelate only Species
s2 <- synthesize(iris, rankcor=c("Species"=0.5))
```

```{#plot2 .R fun=output_figure name="utility" caption="Two versions of syntetic iris" device="png" width=800 height=400}
par(mfrow=c(1,2))
plot(Sepal.Length~Sepal.Width, data=s1, pch=16, col=s1$Species
  , main="Synthetic Iris - all variables decorrelated", sub="All variables decorrelated")
plot(Sepal.Length~Sepal.Width, data=s2, pch=16, col=s2$Species
  , main="Synthetic Iris - Species decorrelated", sub="Only species decorrelated")
```
In the left figure, we show the three variables of a synthesized `iris`
dataset, where all variables are decorrelated. Both the geometric clustering
and the species are now garbled. In the right figure we only decorrelate the
Species variable. Here, the spatial clustering is retained while the
correlation between color (Species) and location is lost.


## Synthesizing (multivariate) time series

Synthesizing time series is as easy as synthesizing data frames, but there are a few differences.

- Synthesized time series must have the same number of data points as the
  original data. Forecasts or backcasts from the original data are not possible.
- There is no `rankcor` parameter for time series data.

As a demonstration, we create a synthetic version of the `UKDriverDeaths`
dataset that is included with base R.
```{#UKDD .R}
data(UKDriverDeaths)
synth_udd <- synthesize(UKDriverDeaths)
```
Below is a plot of the original and synthetic dataset.
```{#plot2 .R fun=output_figure name="ukdd" caption="Original and synthetic time series" device="png" width=800 height=400}
plot(UKDriverDeaths,las=1,lwd=2,main="Drivers killed or seriously injured in the UK"
    , sub="Data from R package 'datasets'")
scol <- adjustcolor('blue',alpha.f=0.5)
lines(synth_udd, col=scol,lwd=2)
legend("topleft",col=c("black",scol),lwd=2,legend=c("Original","Synthetic"),bty='n')
```



## How it works

Synthetic data is generated in two steps:

1. For numerical variables, use inverse transform sampling based on a linear interpolation of the 
   emperical quantile function; for all other variable types, sample with replacement. This yields
   a set of synthetic variables with univariate distributions close to their originals.
2. Reconstruct (linear or nonlinear) correlations by ensuring that the rank order of each synthetic 
   variable matches that of the original data.

These steps ensure a synthetic dataset that closely resembles the original
data. The rank order matching ensures a certain resiliance to the influence of
outliers. If the `rankcor` argument has a value less than the default 1, a third
step is performed:

3. Randomly choose a block of consecutive values in the synthetic data and permute it
   randomly. Iterate these two steps until the rank correlation between the original
   and synthetic data drops below the specified `rankcor` value.

Except for the case of time series it is possible to sample datasets that are
larger or smaller than their originals. This is done by (if necessary) creating
multiple synthetic datasets and sample records uniformly without replacement
from the combined dataset.








