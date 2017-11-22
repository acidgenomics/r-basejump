# basejump

[![Build Status](https://travis-ci.org/steinbaugh/basejump.svg?branch=master)](https://travis-ci.org/steinbaugh/basejump)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/steinbaugh/basejump/branch/master/graph/badge.svg)](https://codecov.io/gh/steinbaugh/basejump)

Base functions for bioinformatics and [R][] package development.


## Installation

There is currently an issue with [GenomeInfoDbData][], a dependency of [ensembldb][], not installing automatically. To resolve this issue, we are recommending users install [ensembldb][] prior to installation of basejump.

### [Bioconductor][] method *(recommended)*

```{r}
source("https://bioconductor.org/biocLite.R")
biocLite("ensembldb")
biocLite(
    "steinbaugh/basejump",
    dependencies = c("Depends", "Imports", "Suggests")
)
```

### [devtools][] method

[ensembldb][] must be installed, otherwise this step will currently fail.

```{r}
install.packages("devtools")
devtools::install_github(
    "steinbaugh/basejump",
    dependencies = c("Depends", "Imports", "Suggests")
)
```


[Bioconductor]: https://bioconductor.org
[devtools]: https://cran.r-project.org/package=devtools
[ensembldb]: http://bioconductor.org/packages/release/bioc/html/ensembldb.html
[GenomeInfoDbData]: https://bioconductor.org/packages/release/data/annotation/html/GenomeInfoDbData.html
[R]: https://www.r-project.org
