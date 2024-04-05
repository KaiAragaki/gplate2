
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gplate2

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/KaiAragaki/gplate2/branch/main/graph/badge.svg)](https://app.codecov.io/gh/KaiAragaki/gplate2?branch=main)
[![R-CMD-check](https://github.com/KaiAragaki/gplate2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KaiAragaki/gplate2/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

gplate2 is the successor of gplate, with the goal of providing a simpler
and faster back end and more intuitive and expected user interface. The
main difference is how gplates are constructed. Instead of being a
dataframe, plates are now a list of wells, and wells are a list of
contents.

## Installation

You can install the development version of gplate2 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KaiAragaki/gplate2")
```
