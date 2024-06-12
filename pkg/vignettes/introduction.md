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

The package supports numerical, categorical/ordinal, and mixed data.

At the moment the method used seems promising but we are working on
investigating where the method shines and where it fails. So we have no
guarantees yet on utility, privacy, and so on. Having said that, our
preliminary results are promesing, and using the package is very easy.


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
par(mfrow=c(1,2))
plot(Sepal.Length ~ Petal.Length, data=iris, col=iris$Species, pch=16,las=1,xlim=c(0,7),ylim=c(4,8),main="Original")
legend("topleft",legend=levels(iris$Species),col=1:3,pch=16,bty="n")
plot(Sepal.Length ~ Petal.Length, data=synth_iris, col=iris$Species,pch=16,las=1,xlim=c(0,7),ylim=c(4,8),main="Synthesized")
legend("topleft",legend=levels(iris$Species),col=1:3,pch=16,bty="n")
```

Although the synthesized dataset shows more variance, it does mimic the
clusture structure of the original dataset.

By default, `synthesize` will return a dataset of the same size as the input dataset. However it is
possible to ask for any number of records.

```{#synthesize_more .R}
more_synth <- synthesize(iris, n=250)
dim(more_synth)
```

## Checking quality

The pMSE method is a popular way of measuring the quality of a dataset. The idea is to 
train a model to predict whether a record is synthetic or not. The mean squared error of prediction
is the measure, so smaller is better.

```{.R}
pmse(synth=synth_iris, real=iris)
```


## How it works


Synthetic data is prepared as follows.

Given an original dataset with $n$ records:

1. For each numeric variable in the dataset, determine the emperical inverse
   cumulative density function (ECDF), and use linear interpolation to interpolate
   between the data points. The observed minimum and maximum are also the minimum
   and maximum of the synthetic univariate distribution. Sample $n$ values using
   inverse transform sampling with the linear interpolated inverse ECDF
2. For each categorical variable, sample $n$ values with replacement.
3. Reorder the synthetic dataset such that the rank order combinations of the synthetic
   data match those of the original dataset.

If less than $m<n$ records are needed, sample $m$ records uniformly from the dataset just created.
If $m>n$ records are needed, create $\lceil m/n\rceil$ synthetic datasets of size $m$ and sample
uniformly $m$ records from the combined data sets.

