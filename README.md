# basejump

[![Travis CI build status](https://travis-ci.org/acidgenomics/basejump.svg?branch=master)](https://travis-ci.org/acidgenomics/basejump)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/007vq15089ukn6ej/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/basejump/branch/master)
[![Codecov percent coverage](https://codecov.io/gh/acidgenomics/basejump/branch/master/graph/badge.svg)](https://codecov.io/gh/acidgenomics/basejump)
[![Anaconda cloud version](https://anaconda.org/bioconda/r-basejump/badges/version.svg)](https://anaconda.org/bioconda/r-basejump)
[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Base functions for bioinformatics and [R][] package development.

## Installation

### [Bioconductor][] method

We recommend installing the package with [BiocManager][].

```r
if (!require("BiocManager")) {
    install.packages("BiocManager")
}
BiocManager::install("remotes")
BiocManager::install("acidgenomics/basejump")
```

For [R][] < 3.5, [BiocManager][] is not supported. Use `BiocInstaller::biocLite()` instead of `BiocManager::install()`. This requires sourcing the legacy [Bioconductor][] `biocLite.R` script.

```r
# try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
```

### [conda][] method

Configure [conda][] to use the [bioconda][] channels.

```bash
conda config --add channels defaults
conda config --add channels bioconda
conda config --add channels conda-forge
```

To avoid version issues, your `.condarc` file should only contain the following channels, in this order:

```
channels:
  - conda-forge
  - bioconda
  - defaults
```

We recommend installing into a clean [conda][] environment:

```bash
conda create --name r
conda activate r
```

Launch [R][] and check that it is set up correctly with the `capabilities()` function. Note that `X11 = TRUE` is required for graphical output, and requires X11 forwarding over SSH.

Now you're ready to install `r-basejump`.

```bash
conda install -c bioconda r-basejump
```

## References

The papers and software cited in our workflows are available as a [shared library](https://paperpile.com/shared/agxufd) on [Paperpile][].

[bioconda]: https://bioconda.github.io/
[BiocManager]: https://cran.r-project.org/package=BiocManager
[Bioconductor]: https://bioconductor.org/
[conda]: https://conda.io/
[Paperpile]: https://paperpile.com/
[R]: https://www.r-project.org/
