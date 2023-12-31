
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ppcountr

<!-- badges: start -->
<!-- badges: end -->

## Purpose

In one of my leagues we use potential points to determine draft order.
MyFantasyLeague does not use players from Taxi Squad or IR to calcluate
the top 21 players. I wrote this package to remedy that.

The optimizer used to find the top 21 uses constraints that are specific
to my league, but they can be updated pretty easily to match whatever
roster constraints your league uses. See `solve_for_pp` and
`calculate_top_21`.

## Installation

You can install the development version of ppcountr like so:

``` r
# install.packages("devtools")
# devtools::install_github("friscojosh/ppcountr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(ppcountr)
# api_key <- "Your API Key"
# league_id <- "Your league ID"
# season <- 2023 # change as needed
# 
# pp <- calculate_top_21(season, league_id, api_key, current_week = 11)
# 
# afc_pp <- make_afc_pp_table(pp)
# nfc_pp <- make_nfc_pp_table(pp)
# afc_pp
# nfc_pp
```
