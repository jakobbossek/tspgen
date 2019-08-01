# tspgen: TSP Instance Generation

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/tspgen)](http://cran.r-project.org/web/packages/tspgen)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/tspgen)](http://cran.rstudio.com/web/packages/tspgen/index.html)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/tspgen?color=orange)](http://cran.rstudio.com/web/packages/tspgen/index.html)
[![Build Status](https://travis-ci.org/jakobbossek/tspgen.svg?branch=master)](https://travis-ci.org/jakobbossek/tspgen)
[![Build status](https://ci.appveyor.com/api/projects/status/eu0nns2dsgocwntw/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/tspgen/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/jakobbossek/tspgen/badge.svg?branch=master)](https://coveralls.io/github/jakobbossek/tspgen?branch=master)

This software package accompanies the following publication accepted at the [FOGA2019](https://hpi.de/foga2019/) conference:

Bossek, J., Kerschke, P., Neumann, A., Wagner, M., Neumann, F., & Trautmann, H. (2019). Evolving Diverse TSP Instances by Means of Novel and Creative Mutation Operators. In Proceedings of the 15th ACM/SIGEVO Workshop on Foundations of Genetic Algorithms (FOGA XV), Potsdam, Germany. (**Accepted**)

## What is this all about?

Usually optimization algorithms are compared by running benchmarks on test problems. For the well-known Traveling-Salesperson-Problem (TSP) diverse benchmark sets exist ranging from instances placed purely at random in the Euclidean plane (so-called Random Uniform Euclidean, RUE), placed in cluster structures (see e.g., the related [netgen](https://github.com/jakobbossek/netgen) R package) or stemming from real-world applications (e.g., [TSPlib](https://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/) or [VLSI](http://www.math.uwaterloo.ca/tsp/vlsi/) instances). 

In the past decade due to advances in machine learning the area of per-instance [Algorithm Selection](https://en.wikipedia.org/wiki/Algorithm_selection) (AS) gained importance significantly. Here, the primary goal is to build a decision-rule that decides for a certain algorithm from a portfolio of algorithms which will most likely perform best on a given instance. Application of AS to the TSP problem is highly encouraging (see, e.g., [1]). The learning process of the decision rule usually relies on a set of (computationally cheap) characteristics of problem instances, termed *instance features* or simply *features*. Another aspect closely related to AS is the understanding of working principles of algorithms: which features / feature combinations are crucial to determine the performance of a given algorithm or a pair of algorithms? Which feature values or ranges of feature values of an instance at hand pose a hard challenge for an algorithm?
Recent research approaches to answer this interesting question adopt evolutionary algorithms which evolve a population of instances which are particularly hard for a given algorithm with respect to approximation quality [2, 3] or maximizing the performance gap between two competitive solvers [4, 5]. However, the latter approaches stick to simple, straight forward mutation operators which results in instances with severe structural similarity and high visual similarity to RUE instances.

One drawback of the afore-mentioned evolutionary instance generation process is the simplicity of mutation operators. In a nutshell the process is as follows: place n points uniformly at random in the Euclidean plane [0,1]x[0,1] and iteratively move points around by adding Gaussian noise or replacing selected points with new points again placed uniformly at random. Obviously, such basic operations are not able to introduce significant structural diversity and even after thousands of iterations the instance will most likely be similar to the initial solution in particular if n is large.

This package introduces a set of sophisticated, problem tailored mutation operators which allow for the generation of more diverse sets of instances. The proposed mutation operators have a much higher impact on the points than the ones mentioned before. E.g., *explosion mutation* generates an *explosion* within the points cloud leaving a hole where not points are placed at all. In contrast *cluster mutation* selects a random subset of points and generates a circular cluster somewhere in the plane.

[1] Kerschke, P., Kotthoff, L., Bossek, J., Hoos, H. H., & Trautmann, H. (2018). Leveraging TSP Solver Complementarity through Machine Learning. Evolutionary Computation, 26(4), 597–620. https://doi.org/10.1162/evco_a_00215

[2] Mersmann, O., Bischl, B., Bossek, J., Trautmann, H., Wagner, M., & Neumann, F. (2012). Local search and the traveling salesman problem: A feature-based characterization of problem hardness. Lecture Notes in Computer Science (Including Subseries Lecture Notes in Artificial Intelligence and Lecture Notes in Bioinformatics), 7219 LNCS, 115–129. https://doi.org/10.1007/978-3-642-34413-8_9

[3] Mersmann, O., Bischl, B., Trautmann, H., Wagner, M., Bossek, J., & Neumann, F. (2013). A novel feature-based approach to characterize algorithm performance for the traveling salesperson problem. Annals of Mathematics and Artificial Intelligence, 69(2), 151–182. https://doi.org/10.1007/s10472-013-9341-2

[4] Bossek, J., & Trautmann, H. (2016). Evolving Instances for Maximizing Performance Differences of State-of-the-Art Inexact TSP Solvers. In P. Festa, M. Sellmann, & J. Vanschoren (Eds.), Proceedings of the 10th International Conference on Learning and Intelligent Optimization (LION 2016) (Vol. 10079 LNCS, pp. 48–59). Ischia, Italy: Springer International Publishing. https://doi.org/10.1007/978-3-319-50349-3_4

[5] Bossek, J., & Trautmann, H. (2016). Understanding Characteristics of Evolved Instances for State-of-the-Art Inexact TSP Solvers with Maximum Performance Difference. In G. Adorni, S. Cagnoni, M. Gori, & M. Maratea (Eds.), AI*IA 2016 Advances in Artificial Intelligence (Vol. 10037 LNAI, pp. 3–12). Genova, Italy: Springer International Publishing. https://doi.org/10.1007/978-3-319-49130-1_1

## Quickstart

In order to generate an instance we (1) specify a collection of mutation operators and application probabilities and (2) start the generation process.

```r
# add mutation operators one by one
set.seed(1)
collection = init()
collection = addMutator(collection, "doExplosionMutation")
collection = addMutator(collection, "doClusterMutation", pm = 0.2)
collection = addMutator(collection, "doAxisProjectionMutation", pm = 0.3)
collection = addMutator(collection, "doGridMutation", box.min = 0.3, box.max = 0.5)
# define probabilities
collection = setProbabilities(collection, probs = c(0.3, 0.2, 0.3, 0.2))
print(collection)

# generate instance
x = build(n = 1000L, iters = 100L, collection = collection, bound.handling = "uniform")
print(autoplot(x))
```


## Related Software

Another R package with methods for the generation of Random Uniform Euclidean instances (RUE) and heavily clustered instances is [netgen](https://github.com/jakobbossek/netgen). A famous RUE instance generator is provided by the DIMACS challenge and termed [portgen](http://archive.dimacs.rutgers.edu/Challenges/TSP/). Further R software with methods for graph generation (not TSP specific) are [igraph](https://igraph.org/r/) and [grapherator](https://github.com/jakobbossek/grapherator).

## How to Contribute

Bug reports are welcome. Please use the official [issue tracker](https://github.com/jakobbossek/tspgen/issues) to report bugs and ask questions. You may also fix the bug by yourself by (1) forking the project and (2) fixing the bug / adding a feature and (3) generating a pull requests. However, in order to avoid useless work we encourage you to contact the authors and discuss the desired feature request first.

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



