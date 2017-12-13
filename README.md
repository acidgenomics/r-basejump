# basejump

[![Build Status](https://travis-ci.org/steinbaugh/basejump.svg?branch=master)](https://travis-ci.org/steinbaugh/basejump)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/steinbaugh/basejump/branch/master/graph/badge.svg)](https://codecov.io/gh/steinbaugh/basejump)

Base functions for bioinformatics and [R][] package development.


## Installation

This is an [R][] package.

### [Bioconductor][] method

```{r}
source("https://bioconductor.org/biocLite.R")
biocLite(
    "steinbaugh/basejump",
    dependencies = c("Depends", "Imports", "Suggests")
)
```


[Bioconductor]: https://bioconductor.org
[R]: https://www.r-project.org
