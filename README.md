
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxnorm

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

> Access drug name and class using RxCUI from the National Library of
> Medicine RxNorm RESTful API.

## Scope

`rxnorm` is an R package providing a basic interface to query the
National Library of Medicine RxNorm RESTful API using RxCUI. The goal is
to provide a simple way to translate RxCUI into drug categories with
incremental levels of generality: specific drug names → brand names →
drug classes.

## Installation

You can install `rxnorm` from [GitHub](https://github.com/) with:

``` r
devtools::install_github("nt-williams/rxnorm")
```

## Example

``` r
library(rxnorm)
```

Query the API for the RxNorm name:

``` r
get_rx(1011485)
#> [1] "cetirizine hydrochloride 10 MG Disintegrating Oral Tablet [Zyrtec]"
```

Query the API for the brand name:

``` r
get_bn(1011485)
#> [1] "ZYRTEC"
```

Query the API for the strength of active ingredients:

``` r
get_rxcui_strength(861819)
#>      activeIngredientName numeratorValue numeratorUnit denominatorValue
#> 1   sitagliptin phosphate             50            MG                1
#> 2 metformin hydrochloride            500            MG                1
#>   denominatorUnit
#> 1            EACH
#> 2            EACH
```

Query the API for the [WHO ATC](https://www.whocc.no/atc_ddd_index/)
drug class:

``` r
get_atc(1011485)
#> [1] "R06AE"
```

We can further parse ATC codes to varying levels of specificy:

``` r
get_atc(1011485, "first")
#> [1] "respiratory system"
get_atc(1011485, "second")
#> [1] "antihistamines for systemic use"
get_atc(1011485, "third")
#> [1] "antihistamines for systemic use"
get_atc(1011485, "fourth")
#> [1] "piperazine derivatives"
```
