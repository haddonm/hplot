
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hplot

<!-- badges: start -->
<!-- badges: end -->

The goal of hplot is to provide a simple way to include an array of
pre-defined base R graphics and helpers. They include many that I use
personally. I have distilled functions from rutilsMH, rforcpue, and
other of my packages. I will list the origins in the readme each time I
include a new function.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("haddonm/hplot")
```

## Development

- 2023-01-18 hplot 0.0.4 Added median to output stats from addnorm,
  modified categoryplot and plotxyy

- 2022-09-05 hplot 0.0.3 Added RGB, which is a wrapper for using rgb
  with numbered or named colours and a maxColorValue=255.

- 2022-09-04 hplot 0.0.2 Added makepolygon, which simplifies the
  generation of the input data for a polygon.
