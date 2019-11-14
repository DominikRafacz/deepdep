
# deepdep <img src='images/logo.png' align="right" height="250" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test
coverage](https://codecov.io/gh/DominikRafacz/deepdep/branch/master/graph/badge.svg)](https://codecov.io/gh/DominikRafacz/deepdep?branch=master)
[![Build
Status](https://travis-ci.org/DominikRafacz/deepdep.svg?branch=master)](https://travis-ci.org/DominikRafacz/deepdep)
[![CircleCI build
status](https://circleci.com/gh/DominikRafacz/deepdep.svg?style=svg)](https://circleci.com/gh/DominikRafacz/deepdep)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/DominikRafacz/deepdep?branch=master&svg=true)](https://ci.appveyor.com/project/DominikRafacz/deepdep)

<!-- badges: end -->

Acquire and Visualise Deep Dependiences of R
packages.

[Cheatsheet](https://github.com/DominikRafacz/deepdep/blob/master/images/cheatsheet.pdf)

## Installation

``` r
# Install the development version from GitHub:
devtools::install_github("DominikRafacz/deepdep")
```

## Demo

``` r
library(deepdep)

dd1 <- deepdep("ggplot2", downloads = TRUE)

head(dd1)
```

    ##    origin     name  version    type last_day last_week last_month last_quarter last_half grand_total
    ## 1 ggplot2   digest     <NA> Imports    38127    216843    1037046      2723101   4678559    21981135
    ## 2 ggplot2   gtable >= 0.1.1 Imports    21452    117458     517523      1492998   2678144    14241763
    ## 3 ggplot2 lazyeval     <NA> Imports    25111    139651     629231      1775422   3065808    15025239
    ## 4 ggplot2     MASS     <NA> Imports     6334     33620     132721       348586    694901     5361955
    ## 5 ggplot2     mgcv     <NA> Imports     8635     49032     177350       368702    570441     4405598
    ## 6 ggplot2 reshape2     <NA> Imports    27118    158796     694046      1934162   3249457    17570974

``` r
plot_dependencies(dd1, "tree")
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
dd2 <- deepdep("ggplot2", depth = 2)

plot_dependencies(dd2, "circular")
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

-----

This package was made during `1120-DS000-ISP-0500`[Advanced Programming
in R](https://github.com/mini-pw/2020Z-ProgramowanieWR) course at Warsaw
University of Technology.
