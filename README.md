
<!-- README.md is generated from README.Rmd. Please edit that file -->

# summaryStats

This package contains a function called “summaryStats” that can be used
to create a dataframe that contains summary statistics for a given
dataset. The summary statistics that are calculated with this function
include the mean, median, max, min, and sdev (standard deviation).

The function has two mandatory arguments. The first argument
(dataset_of_interest) requires the user to input a dataframe. The second
argument (col_of_interest) requires the user to input a numeric column
in the dataset, that the user wishes to calculate summary statistics on.

The function also has an optional third argument, which is FALSE by
default. If the user inputs TRUE, then this function will also export a
csv file called “results.csv” which contains the results of the
calculated summary statistics.

## Installation

This package is not published on CRAN, so in order to use it, you need
to download it from this repository. You can install summaryStats from
[GitHub](https://github.com/) using the following line of code:

``` r
# install.packages("devtools")
devtools::install_github("stat545ubc-2021/lorenzolindoPackage/summaryStats")
#> Skipping install of 'summaryStats' from a github remote, the SHA1 (3e94c845) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
suppressMessages({
  library(summaryStats)
  library(tidyverse)
  library(datateachr)
})
summaryStats(vancouver_trees, height_range_id)
#> # A tibble: 1 × 5
#>    mean median   max   min  sdev
#>   <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1  2.63      2    10     0  1.54
```
