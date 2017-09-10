# basejump 0.0.23

- Upgraded `annotable()` function to query Ensembl using the [ensembldb][] package rather than [annotables][].



# basejump 0.0.22

- Improved unit testing coverage.
- Renamed `prepareSE()` to `prepareSummarizedExperiment()`. Improved row and column name handling in the function. It now outputs more helpful diagnostic messages on error.
- Reworked and simplified `detectHPC()` function to allow for unit testing.



# basejump 0.0.21

- NAMESPACE improvements. Reduced the number of re-exported functions to simplify the package.
- Improved code coverage and unit testing with additional [testthat][] checks. Specifically, added unit testing for remote download functions and improved testing for GTF file utilities.
- Code coverage now above 90%!
- Renamed `packageSE()` to `prepareSE()` for better semantic meaning.
- Made multiple generics more flexible by inclusion of `…`.
- Reduced the number of deprecated functions.
- Initial commit of internal `localOrRemote()` utility function.
- Initial commit of `prepareTemplate()` function.
- Added additional `data-raw/` scripts.
- Added `onLoad.R` script back to ensure proper attachment of [annotables][] data package.
- Removed [tidyverse][] S4 method support.
- Improved remote file handling for `readFileByExtension()`, `readGTF()`, and `readYAML()` functions.



# basejump 0.0.20

- Offloaded [devtools][] functions to personal package.
- Upgraded all functions to S4 functions where possible.
- Assign utilities were kept as S3 functions since S4 dispatch makes `parent.frame()` assignment not work correctly.
- Deprecated snake_case and British spelling variants to reduce the number of exported functions.
- Added more working examples.
- Added unit testing for [annotables][] functions.



# basejump 0.0.19

- Improved documentation and consistency with [bcbio][] packages.
- Improved integration of gene annotation calls using [annotables][] package.



# basejump 0.0.18

- Initial support for [SummarizedExperiment][] creation with `packageSE()`.



# basejump 0.0.17

- Added [annotables][] common code from bcbio packages.
- Added automatic file reading using [readr][] package.
- Combined write_counts functions from [bcbioRNASeq][] and [bcbioSinglecell][].
- Initial commit of `assign_data()` for use in [bcbioSingleCell][] sample loops.



# basejump 0.0.16

- Minor NAMESPACE updates while working on [bcbio][] packages.
- Tweaks for [tidyverse][] S4 generic verbs. In particular, `as_tibble()` now provides better consistency for rowname conversion.



# basejump 0.0.15

- Added [testthat][] support for [lintr][] checks.
- Added S4 generic for `as_tibble()`.



# basejump 0.0.14

- [dplyr][] 0.7 NAMESPACE fixes and function tweaks.



# basejump 0.0.13

- setMethod on tidyverse NAMESPACE collisons ([dplyr][], [tidyr][]) using `signature("data.frame").



# basejump 0.0.12

- Updated exports based on [worminfo][] package.



# basejump 0.0.11

- Improved naming functions to dynamically handle character vectors and objects that support naming assignments.
- Added `removeNA()` utility function.



# basejump 0.0.10

- Added NAMESPACE utilities to deal with [tidyverse][] generic verbs.
- Switched package documentation method to use [roxygen][] with pkgapi.



# basejump 0.0.9

- Added snake_case function variants.



# basejump 0.0.8

- Added back `saveData()` utility functions.



# basejump 0.0.7

- Bug fixes for [dplyr][] 0.6.0 update and improved kable handling.



# basejump 0.0.6

- Dependency fix for successful compilation on the [HMS RC][] Orchestra cluster.



# basejump 0.0.5

- Consolidated functions in the documentation.



# basejump 0.0.4

- Improved documentation.



# basejump 0.0.3

- Removed dependencies and transfer functions to [bcbioRNASeq][].



# basejump 0.0.2

- Added [bcbio][] data import functions.
- Added [ggplot2][] wrapper functions for quality control.



# basejump 0.0.1

- Initial draft release.



[annotables]: https://github.com/stephenturner/annotables
[bcbio]: https://github.com/chapmanb/bcbio-nextgen
[bcbioRNASeq]: http://bioinformatics.sph.harvard.edu/bcbioRNASeq
[bcbioSingleCell]: http://bioinformatics.sph.harvard.edu/bcbioSingleCell
[devtools]: https://github.com/hadley/devtools
[dplyr]: http://dplyr.tidyverse.org
[ensembldb]: http://bioconductor.org/packages/release/bioc/html/ensembldb.html
[ggplot2]: http://ggplot2.tidyverse.org
[HMS RC]: https://rc.hms.harvard.edu
[lintr]: https://github.com/jimhester/lintr
[readr]: http://readr.tidyverse.org
[roxygen]: https://github.com/klutometis/roxygen
[SummarizedExperiment]: https://bioconductor.org/packages/release/bioc/html/SummarizedExperiment.html
[testthat]: https://github.com/hadley/testthat
[tidyr]: http://tidyr.tidyverse.org
[tidyverse]: http://www.tidyverse.org
[worminfo]: http://steinbaugh.com/worminfo
