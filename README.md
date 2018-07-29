# basejump

[![Travis CI](https://travis-ci.org/steinbaugh/basejump.svg?branch=master)](https://travis-ci.org/steinbaugh/basejump)
[![AppVeyor CI](https://ci.appveyor.com/api/projects/status/007vq15089ukn6ej/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/basejump/branch/master)
[![Codecov](https://codecov.io/gh/steinbaugh/basejump/branch/master/graph/badge.svg)](https://codecov.io/gh/steinbaugh/basejump)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Anaconda-Server Badge](https://anaconda.org/bioconda/r-basejump/badges/version.svg)](https://anaconda.org/bioconda/r-basejump)

Base functions for bioinformatics and [R][] package development.

## Installation

This is an [R][] package.

### [Bioconductor][]

We recommend installing the package with [BiocManager][].

```r
if (!require("BiocManager")) {
    install.packages("BiocManager")
}
BiocManager::install(
    pkgs = c(
        "devtools",
        "remotes",
        "GenomeInfoDbData"
    )
)
BiocManager::install(
    pkgs = "steinbaugh/basejump",
    dependencies = c("Depends", "Imports", "Suggests")
)
```

For [R][] < 3.5, [BiocManager][] is not supported. Use `BiocInstaller::biocLite()` instead of `BiocManager::install()`. This requires sourcing the legacy [Bioconductor][] `biocLite.R` script.

```r
# try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
```

### [conda][]

Configure [conda][] to use the [bioconda][] channels.

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

# References

The papers and software cited in our workflows are available as a [shared library](https://paperpile.com/shared/agxufd) on [Paperpile][].

[bioconda]: https://bioconda.github.io
[BiocManager]: https://cran.r-project.org/package=BiocManager
[Bioconductor]: https://bioconductor.org
[conda]: https://conda.io
[Paperpile]: http://paperpile.com
[R]: https://www.r-project.org
