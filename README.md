# basejump

[![Travis CI build status](https://travis-ci.com/acidgenomics/basejump.svg?branch=master)](https://travis-ci.com/acidgenomics/basejump)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/007vq15089ukn6ej/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/basejump/branch/master)
[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Anaconda cloud version](https://anaconda.org/bioconda/r-basejump/badges/version.svg)](https://anaconda.org/bioconda/r-basejump)

Base functions for bioinformatics and [R][] package development.

## Installation

### [Bioconductor][] method

We recommend installing the package with [BiocManager][].

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
library(BiocManager)
install("remotes")
install("acidgenomics/basejump")
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
