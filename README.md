[`devtools`]: https://cran.r-project.org/package=devtools
[R]: https://www.r-project.org

# seqcloudr

[![Build Status](https://travis-ci.org/seqcloud/seqcloudr.svg?branch=master)](https://travis-ci.org/seqcloud/seqcloudr)

## Installation

This is an [R][] data package. [`devtools`][] is required to install the latest version directly from GitHub.

To install, run this code in [R][]:

```{r}
install.packages("devtools")
devtools::install_github("steinbaugh/worminfo", build_vignettes = TRUE)
```

Instructions on how to use the functions in this package are available as vignettes:

```{r}
browseVignettes("worminfo")
```
