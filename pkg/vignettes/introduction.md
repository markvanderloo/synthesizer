<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Introduction to synthesizer}
-->

---
title: Introduction to `synthesizer`
author: Mark P.J. van der Loo
css: "style.css"
---

Package version `packageVersion("synthesizer")`{.R}. 

Use `citation('synthesizer')` to cite the package.

## Introduction

`synthetiser` is an R package for quickly and easily synthesizing data.
It also provides a few basic functions based on pMSE to measure some
utility of the synthesized data.

The package supports numerical, categorical/ordinal, and mixed data and also
correctly takes account of missing values.

At the moment the method used seems promising but we are working on
investigating where the method shines and where it fails. So we have no
guarantees yet on utility, privacy, and so on. Having said that, our
preliminary results are promising, and using the package is very easy.


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

```{#load_chickweight .R}
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


By default, `synthesize` will return a dataset of the same size as the input dataset. However it is
possible to ask for any number of records.

```{#synthesize_more .R}
more_synth <- synthesize(iris, n=250)
dim(more_synth)
```

## Checking quality

The pMSE method is a popular way of measuring the quality of a dataset. The idea is to 
train a model to predict whether a record is synthetic or not. The worse a model can
do that, the better a synthic data instance resembles the real data. The value scales
between 0 and 0.25 (if the synthetic and real datasets have the same number of records).
Smaller is better.

```{#pMSE .R}
pmse(synth=synth_iris, real=iris)
```


## Choosing the utility-privacy trade-off

Synthetic data can be too realistic, in the sense that it might reveal actual
properties of the real entities represented by synthetic data. One way to
mitigate this is to decorrelate the variables in the synthetic data. For data
frames, this can be done with the `utility` parameter. Either for all
variables, or for a selectin of parameters. Setting `utility` to 1 (the
default) yields the most realistic data, lowering the utility causes loss
of (linear or nonlinear) correlation between synthetic variables, if there was
any in the real data. 
```{#decorrelate .R}
# decorrelate rank matching to 0.5
s1 <- synthesize(iris, utility=0.5)
# decorrelate only Species
s2 <- synthesize(iris, utility=c("Species"=0.5))
```

```{#plot2 .R fun=output_figure name="test" caption="Two versions of syntetic iris" device="png" width=800 height=400}

par(mfrow=c(1,2))
plot(Sepal.Length~Sepal.Width, data=s1, pch=16, col=s1$Species
  , main="Synthetic Iris", sub="All variables decorrelated")
plot(Sepal.Length~Sepal.Width, data=s2, pch=16, col=s2$Species
  , main="Synthetic Iris", sub="Only species decorrelated")
```
In the left figure, we show the three variables of a synthesized `iris`
dataset, where all variables are decorrelated. Both the geometric clustering
and the species are now garbled. In the right figure we only decorrelate the
Species variable. Here, the spatial clustering is retained while the
correlation between color (Species) and location is lost.





## How it works


Synthetic data is prepared as follows.

Given an original dataset with $n$ records:

1. For each numeric variable in the dataset, determine the empirical inverse
   cumulative density function (ECDF), and use linear interpolation to interpolate
   between the data points. The observed minimum and maximum are also the minimum
   and maximum of the synthetic univariate distribution. Sample $n$ values using
   inverse transform sampling with the linear interpolated inverse ECDF. 
   Missing values are taken into account by sampling them proportional to their
   occurrence.
2. For each categorical or logical variable, sample $n$ values with replacement.
3. Reorder the synthetic dataset such that the rank order combinations of the synthetic
   data match those of the original dataset. If any of the correlations is less than one,
   first randomly permute the rank correlations until correlation between original real and
   synthetic ranks drops below the specified value.

If less than $m<n$ records are needed, sample $m$ records uniformly from the dataset just created.
If $m>n$ records are needed, create $\lceil m/n\rceil$ synthetic datasets of size $m$ and sample
uniformly $m$ records from the combined data sets.

