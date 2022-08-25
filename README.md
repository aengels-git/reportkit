
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reportkit

<!-- badges: start -->
<!-- badges: end -->

The goal of reportkit is to make reporting in word and power point
simple.

## Installation

You can install the development version of reportkit from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aengels-git/reportkit")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyverse)
library(reportkit)
library(prepkit)
library(flextable)
#> 
#> Attaching package: 'flextable'
#> The following object is masked from 'package:purrr':
#> 
#>     compose
tab2<-prep_freq_table(diamonds,cut,color)
#> Loading required package: rlang
#> 
#> Attaching package: 'rlang'
#> The following objects are masked from 'package:purrr':
#> 
#>     %@%, as_function, flatten, flatten_chr, flatten_dbl, flatten_int,
#>     flatten_lgl, flatten_raw, invoke, splice
#> Loading required package: magrittr
#> Warning: package 'magrittr' was built under R version 4.0.5
#> 
#> Attaching package: 'magrittr'
#> The following object is masked from 'package:rlang':
#> 
#>     set_names
#> The following object is masked from 'package:purrr':
#> 
#>     set_names
#> The following object is masked from 'package:tidyr':
#> 
#>     extract
#> Joining, by = "cut"Joining, by = "color"Joining, by = c("cut", "color", "n",
#> "n_cut", "n_color")Joining, by = c("cut", "color", "n", "n_cut", "n_color")
rtab2<-reporter_table$new(tab2,percent_vector = c(0.5,0.25,0.25),fontsize = 22)
#> Loading required package: officer
#> Warning: package 'officer' was built under R version 4.0.5
rtab2$round()
#> `mutate_if()` ignored the following grouping variables:
#> Column `cut`
rtab2$merge_col_cells(column = "cut",h_lines = T)
rtab2$add_label_row(colwidths = c(2,3,3),values = c("Variables","Frequency","Proportions"),align = "left")
rtab2$add_caption("Table 1: Table on diamonds")
rtab2$add_footer("Footer: this is a table")
```

``` r
rtab2$ft
```
