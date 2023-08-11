# basejump

![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)
[![Install with Bioconda](https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg?style=flat)](http://bioconda.github.io/recipes/r-basejump/README.html)

Base functions for bioinformatics and [R][] package development.

## Installation

This is an [R][] package.

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "basejump",
    repos = c(
        "https://r.acidgenomics.com",
        BiocManager::repositories()
    ),
    dependencies = TRUE
)
```

### [Conda][] method

Configure [Conda][] to use the [Bioconda][] channels.

```sh
# Don't install recipe into base environment.
name='r-basejump'
conda create --name="$name" "$name"
conda activate "$name"
R
```

## References

The papers and software cited in our workflows are available as a
[shared library](https://paperpile.com/shared/agxufd) on [Paperpile][].

[bioconda]: https://bioconda.github.io/
[conda]: https://conda.io/
[paperpile]: https://paperpile.com/
[r]: https://www.r-project.org/
