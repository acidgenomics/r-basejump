[`devtools`]: https://cran.r-project.org/package=devtools
[R]: https://www.r-project.org

# seqcloudr

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/steinbaugh/seqcloudr.svg?branch=master)](https://travis-ci.org/steinbaugh/seqcloudr)

## Installation

This is an [R][] data package. [`devtools`][] is required to install the latest version directly from GitHub.

To install, run this code in [R][]:

```{r}
install.packages("devtools")
devtools::install_github("steinbaugh/seqcloudr", build_vignettes = TRUE)
```

Instructions on how to use the functions in this package are available as vignettes:

```{r}
browseVignettes("seqcloudr")
```
