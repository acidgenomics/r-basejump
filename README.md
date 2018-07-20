# basejump

[![Travis CI](https://travis-ci.org/steinbaugh/basejump.svg?branch=master)](https://travis-ci.org/steinbaugh/basejump)
[![AppVeyor CI](https://ci.appveyor.com/api/projects/status/007vq15089ukn6ej/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/basejump/branch/master)
[![Codecov](https://codecov.io/gh/steinbaugh/basejump/branch/master/graph/badge.svg)](https://codecov.io/gh/steinbaugh/basejump)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Anaconda-Server Badge](https://anaconda.org/bioconda/r-basejump/badges/version.svg)](https://anaconda.org/bioconda/r-basejump)

Base functions for bioinformatics and [R][] package development.


## Installation

This is an [R][] package.

### [Bioconductor][] method

We recommend using [R][] 3.5 / [Bioconductor][] 3.7.

#### R >= 3.5

```r
install.packages("BiocManager")
library(BiocManager)
install("devtools")
install("GenomeInfoDbData")
install("steinbaugh/basejump")
```

#### R < 3.5

Legacy support for [R][] 3.4 / [Bioconductor][] 3.6 is provided.

```r
# try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("devtools")
biocLite("GenomeInfoDbData")
biocLite("steinbaugh/basejump")
```

### [conda][]  method

Ensure that [conda][] is configured to use the [bioconda][] channels.

```bash
conda config --add channels defaults
conda config --add channels conda-forge
conda config --add channels bioconda
```

To avoid version issues, your `.condarc` file should only contain the following channels, in this order:

```
channels:
  - bioconda
  - conda-forge
  - defaults
```
  
We recommend first setting up a clean [R][] [conda][] environment, which should use `r-base` from the `conda-forge` channel.

```bash
conda create --name r-base
conda activate r-base
```

Launch R and check that it is set up correctly with the `capabilities()` function. Note that `X11 = TRUE` is required for graphical output, and requires X11 forwarding over SSH.

Now you're ready to install `r-basejump`.

```bash
conda install -c bioconda r-basejump
```


[bioconda]: https://bioconda.github.io
[Bioconductor]: https://bioconductor.org
[conda]: https://conda.io
[R]: https://www.r-project.org
