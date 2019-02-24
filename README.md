# tspgen: TSP Instance Generation

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/tspgen)](http://cran.r-project.org/web/packages/tspgen)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/tspgen)](http://cran.rstudio.com/web/packages/tspgen/index.html)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/tspgen?color=orange)](http://cran.rstudio.com/web/packages/tspgen/index.html)
[![Build Status](https://travis-ci.org/jakobbossek/tspgen.svg?branch=master)](https://travis-ci.org/jakobbossek/tspgen)
[![Build status](https://ci.appveyor.com/api/projects/status/eu0nns2dsgocwntw/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/tspgen/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/jakobbossek/tspgen/badge.svg?branch=master)](https://coveralls.io/github/jakobbossek/tspgen?branch=master)
[![Research software impact](http://depsy.org/api/package/cran/tspgen/badge.svg)](http://depsy.org/package/r/tspgen)

## What is this all about?

Usually optimization algorithms are compared by running benchmarks on test problems. For the well-known Traveling-Salesperson-Problem (TSP) diverse benchmark sets exist ranging from instances placed purely at random in the Euclidean plane (so-called Random Uniform Euclidean, RUE), placed in cluster structures (see e.g., the related [netgen](https://github.com/jakobbossek/netgen) R package) or stemming from real-world applications (e.g., TSPlib or VLSI instances). 

In the past decade due to advances in machine learning the area of per-instance Algorithm Selection (AS) gained importance significantly. Here, the primary goal is to build a decision-rule that decides for a certain algorithm from a portfolio of well-performing algorithms which will most likely perform best on a given instance. The learning process of the decision rule usually relies on a set of (computationally cheap) characteristics of problem instances, termed *instance features* or simple *features*. Another aspect closely related to AS is the understanding of working principles of algorithms: which features / feature combinations are crucial to determine the performance of a given algorithm or a pair of algorithms? Which features values or ranges of feature values of an instance at hand poses a hard challenge for an algorithm?
Recent research approaches to answer this interesting question adopt evolutionary algorithms which evolve a population of instances which are particularly hard for a given algorithm with respect to approximation quality [1, 2, 3] or maximizing the performance gap between two competitive solvers [4, 5, 6]. However, the latter approaches stick to simple, straight forward mutation operators which results in instances with severe structural similarity and high visual similarity to RUE instances.

This package introduces a set of sophisticated, problem tailored mutation operators which allow for the generation of more diverse sets of instances.

To be continued ...

## Related Software

To be written ...

## How to Contribute

To be written ...

## Installation Instructions

The package will be available at [CRAN](http://cran.r-project.org) soon. Install the release version via:
```r
install.packages("tspgen")
```
If you are interested in trying out and playing around with the current github developer version use the [devtools](https://github.com/hadley/devtools) package and type the following command in R:

```r
devtools::install_github("jakobbossek/tspgen")
```

## Contact

Please address questions and missing features about the **tspgen** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jakobbossek/tspgen/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem quickly.



