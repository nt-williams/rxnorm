
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxnorm

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

Access drug names based on RxNorm CUI from the National Library of
Medicine RxNorm RESTful API.

## Installation

You can install the development version of `rxnorm` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nt-williams/rxnorm")
```

## Example

``` r
rxnorm::get_rx(1011485)
#> [1] "cetirizine hydrochloride 10 MG Disintegrating Oral Tablet [Zyrtec]"
```
