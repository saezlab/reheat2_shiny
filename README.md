
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Welcome to ReHeaT

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/saezlab/reheat.svg?branch=master)](https://travis-ci.com/saezlab/reheat)
<!-- badges: end -->

> the **Re**ference for the **Hea**rt failure **T**ranscriptome

### About

ReHeaT2 is a user friendly portal to explore, analyze and download the
multicellular program of human heart failure .

The is work is based on a
[meta-analysis](https://www.biorxiv.org/content/10.1101/2024.11.04.621815v1) of 34
independent studies. Corresponding code is available
[here](https://github.com/saezlab/reheat2_pub).

### How to access

There are basically three ways of how to access ReHeaT:

-   You can access a live version running
    [here](https://saezlab.shinyapps.io/reheat2/) on the server from
    `shinyapps.io`.

-   You can run the app locally in an interactive R session. Before make
    sure you have all packages installed listed in
    [`sub/global.R`](https://github.com/saezlab/reheat2/blob/master/sub/global.R).

``` r
shiny::runGitHub("reheat", "saezlab")
```

-   You can run the app locally by cloning this repository. All required
    packages can be easily installed using the
    [`renv`](https://rstudio.github.io/renv/index.html) package.

``` r
# install all required packages using the renv package
renv::restore()
shiny::runApp()
```
