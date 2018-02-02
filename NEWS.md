## basejump 0.2.0 (2018-01-28)

- Offloaded internal bcbio-specific code to new package named [bcbioBase][]. Consequently, this makes the basejump package leaner, meaner, and easier to manage. The following functions are now exported in that package: `bcbio()`, `checkInterestingGroups()`, `flatFiles()`, `interestingGroups()`, `metrics()`, `plotDot()`, `plotGene()`, `plotQC()`, `plotViolin()`, `prepareSummarizedExperiment()`, `prepareTemplate()`, `readDataVersions()`, `readLogFile()`, `readProgramVersions()`, `readSampleMetadataFile()`, `sampleMetadata()`, `sampleYAML()`, `sampleYAMLMetadata()`, `sampleYAMLMetrics()`, and `selectSamples()`. These functions are now deprecated here in basejump (see `deprecated.R` file for more information).
- `comp()` and `revcomp()` have been deprecated in favor of `complement()` and `reverseComplement()` from the [Biostrings][] package.
- Internally, errors, messages, and warnings now use methods from the [rlang][] package: `abort()`, `inform()`, and `warn()`, in place of `stop()`, `message()`, and `warning()`, respectively.
- Improved error handing for missing files in `loadData()`. Additionally, the file name must match the internal name in the RData file, otherwise `loadData()` will warn the user. This is more strict than the default behavior of `base::load()`, but helps prevent accidental overwrite in the current working environment.
- `localOrRemoteFile()`, previously an internal function, is now exported.
- `annotable()` now uses internal GRCh37 annotations from the [annotables][] package, which is saved in the `extdata/` directory internally. Previously, these genome annotations were accessed from lazy loaded data saved in the `data/` directory of the package repository.
- `annotables()` now checks for all packages attached by [ensembldb][] and [AnnotationHub][] and forces detachment at the end of the function call. Otherwise, this can result in the unwanted effect of [ensembldb][] masking other user-loaded functions, such as the [tidyverse][] suite (e.g. `dplyr::select()`).
- Consistently reformatted code indents to be a multple of 4 spaces, as recommended by [Bioconductor][].
- `camel()` now handles delimited numbers a little differently. Previously, delimiters in between numbers (e.g. the commas in "1,000,000") were stripped. Sometimes this can result in confusing names. For example, if we have a column formatted in dotted case containing a decimal (e.g. "resolution.1.6"), the decimal would be stripped (e.g. "resolution16" in camel). Now, we sanitize a numeric delimiter as a lower case "x" character (e.g. "resolution1x6"). This ensures that numbers containing decimals remain semantically meaningful when sanitized with `camel()`.
- Internally, `gsub()` (and `grepl()`) calls have been simplified to use the default order of "pattern, replacement, x".
- Internally, all implicit integers have been converted to explicit integers, where applicable.


## basejump 0.1.10 (2017-12-20)

- Bug fix for multiplexed sample input into `readSampleMetadataFile()`. We were detecting the presence of `index` column but should instead check against `sequence` column.
- Added `dynamicPlotlist()` and `mdPlotlist()` plotting utilities.
- Added `uniqueSymbols` parameter to `annotable()` function.


## basejump 0.1.9 (2017-12-18)

- Added `plotHeatmap()` functionality.
- Migrated `tpm()` generic from bcbioRNASeq, for future use in bcbioSingleCell.
- Added matrix method support for `plotHeatmap()`.
- Added matrix method support for `plotQuantileHeatmap()`, which works similarly as `plotHeatmap()`.


## basejump 0.1.8 (2017-12-14)

- Improved `matrix` and `dgCMatrix` method support in `aggregateReplicates()` and `aggregateFeatures()` functions. Both of these functions now use a consistent `groupings` parameter, which uses a named factor to define the mappings of either samples (columns) for `aggregateReplicates()` or genes/transcripts (rows) for `aggregateFeatures()`.
- Update for makeNames sanitization functions. Now they will work on `names(x)` for vectors by default.
- Improved `detectOrganism()` to match against "H. sapiens" and "Homo_sapiens".
- Added internal GRCh37 transcript to gene mapping.
- Improved organism matching to detect "Homo_sapiens" and "H. sapiens".
- Factors are now supported in the makeNames utilities: `camel()`, `dotted()`, `snake()`, and `upperCamel()`.
- Improved handling of `NA` values from LibreOffice and Microsoft Excel output in `readFileByExtension()`.. This function now sets `""`, `NA`, and `#N/A` strings as `NA` correctly.


## basejump 0.1.7 (2017-12-11)

- Renamed `fc2lr()` to `foldChangeToLogRatio()` and `lr2fc()` and `logRatioToFoldChange()`.
- Moved `plotDot()` and `plotViolin()` generics here from bcbioSingleCell.
- Added internal GRCh37 gene annotations.


## basejump 0.1.6 (2017-12-06)

- Moved `microplate()` code from the worminfo package here, since it's of general interest.


## basejump 0.1.5 (2017-12-05)

- Added `checkAnnotable()`, `checkGene2symbol()`, `checkTx2gene()`, and `sanitizeAnnotable()` utility functions that will be used in the bcbio R packages.


## basejump 0.1.4 (2017-12-04)

- Added `midnightTheme()` ggplot theme. Originally this was defined as `darkTheme()` in the bcbioSingleCell package, but can be useful for other plots and has been moved here for general bioinformatics usage. The theme now uses `ggplot2::theme_minimal()` as the base, with some color tweaks, namely dark gray axes without white axis lines.
- Improve NAMESPACE imports to include `stats::formula()` and `utils::capture.output()`.


## basejump 0.1.3 (2017-12-01)

- `loadData()` and `loadDataAsName()` now default to `replace = TRUE`. If an object with the same name exists in the destination environment, then a warning is generated.
- `collapseToString()` only attempts to dynamically return the original object class on objects that aren't class `data.frame`. I updated this code to behave more nicely with grouped tibbles (`grouped_df`), which are a virtual class of `data.frame` and therefore can't be coerced using `as(object, "grouped_df")`.
- DNA sequence utility functions `comp()` and `revcomp()` now return `NULL` for integers and numerics.
- For `prepareSummarizedExperiment()`, added support for dropping `NULL` objects in assays list. This is useful for handling output from bcbioRNASeq when `transformLimit` is reached. In this case, the `rlog` and `vst` matrices aren't generated and set `NULL` in the assays list. Using `Filter(Negate(is.null), assays)` we can drop these `NULL` objects and prevent a downstream dimension mismatch in the `SummarizedExperiment()` call.
- Improved support for multiplexed files in `readSampleMetadataFile()`. This now checks for a sequence column containing ACGT nucleotides. When those are detected, the `revcomp` column is generated. Otherwise this step is skipped. This is useful for handling multiplexed sample metadata from 10X Genomics CellRanger single-cell RNA-seq samples.
- Updated `annotable()` function to include nested Entrez identifiers in the `entrez` column. This is useful for downstream functional analysis.


## basejump 0.1.2 (2017-11-30)

- Added bcbio `plotQC()` generic.
- Added back `toStringUnique()` code, which is still in use in the worminfo package.
- Added deprecations for `summarizeRows()` (now `collapseToString()`) and `wash()` functions.
- Updated installation method to include manual installation of ensembldb. Otherwise, basejump installation fails due to GenomeInfoDb and GenomeInfoDbData not getting installed completely.
- Now suggesting that the user installs suggested packages in the README.
- Updated PANTHER annotation scripts.
- Bug fix for `detectOrganism()`. Now allowing `NULL` return for unsupported organism, with a warning.


## basejump 0.1.1 (2017-11-11)

- Added overwrite support for `saveData()`. Now will skip on existing files when `overwrite = FALSE`.
- Bug fix for `readDataVersions()`, which shouldn't have the column types defined, using `col_types = "ccT"`.
- Improved key value pair method for `loadDataAsName()`. Now rather than using a named character vector for the `mappings` argument, the user can simply pass the key value pairs in as dots. For example, `newName1 = "oldName1", newName2 = "oldName2"`. The legacy `mappings` method will still work, as long as the dots argument is a length of 1.
- Ensembl release version now defaults to `NULL` instead of `current` for `annotable()`, `gene2symbol()`, `symbol2gene()` and `tx2gene()` functions.
- Allow rowData to be left unset in `prepareSummarizedExperiment()`. This is useful for setting
up objects that don't contain gene annotations.
- Removed sample selection by pattern matching (`pattern`, `patternCol` arguments) in `readSampleMetadata()`. This feature wasn't fully baked and doesn't offer enough functionality to the user.


## basejump 0.1.0 (2017-10-23)

- Bump version to match bcbioRNASeq package.
- Improved unit testing coverage of `prepareSummarizedExperiment()`.
- Added quiet mode support to functions that output messages, where applicable.
- Moved roxygen function imports to each file from `basejump-package.R` file.
- Deprecated `sampleDirs()` generic.
- Improved organism detection in `detectOrganism()` and added support for chicken genome.
- Clarified warning messages in `prepareSummarizedExperiment()` to make sample loading with `loadRNASeq()` and `loadSingleCell()` in the bcbio packages less confusing.
- Improved `NULL` returns in `readDataVersions`, `readLogFile()`, and `readProgramVersions()` utility functions.
- Fixed export of `*GTF()` alias functions to simply wrap the `*GFF()` functions with S4 methods support.
- Improved lane split technical replicate handling in `readSampleMetadataFile()`.
- Improved `camel()` syntax for both lax and strict modes. Added `upperCamel()` function.
- Switched `str_()` to base `grep()` and `gsub()` in internal functions.


## basejump 0.0.25 (2017-10-13)

- Improved consistency of `setMethod()` calls using `signature()`.
- Converted `loadRemoteData()` to a standard function instead of using S4 dispatch, allowing the `envir` argument to be set properly.


## basejump 0.0.24 (2017-10-10)

- Added additional package version requirements in `DESCRIPTION` file.
- Implicit integers are allowed.
- Using GitHub version of covr package for unit tests.
- Renamed `multiassignAsNewEnv()` to `multiassignAsNewEnvir()`
- Added `*GFF()` function variants for `gene2symbolFromGTF()` and `tx2geneFromGTF()`.
- Added shared bcbio generic functions: `aggregateReplicates()`, `bcbio()`, `bcbio<-()`, `interestingGroups()`, `metrics()`, `plotGene()`, `sampleDirs()`, `sampleMetadata()`, `selectSamples()`. These functions are saved in `bcbioGenerics.R` file.
- Incorporated bcbio utility functions shared across bcbioRNASeq and bcbioSingleCell: `readDataVersions()` (deprecated `.dataVersions()`), `readLogFile()` (deprecated `.logFile()`), `readProgramVersions()` (deprecated `.programs()`), `sampleYAML()` (deprecated `.sampleYAML()`), `sampleYAMLMetadata()` (deprecated `sampleYAMLMetadata()`), `sampleYAMLMetrics()` (deprecated .sampleYAMLMetrics()`).
- Deprecated `metadataTable()` function.
- Moved all roxygen documentation to `methods-*.R` files where applicable.
- Now using `assays` in `prepareSummarizedExperiment()` generic definition as primary object.
- Improved `assignAndSaveData()` to add silent return of file path.
- Now consistently using clear function definitions (`()`) in chain operations with magrittr pipe (`%>%`).
- Added `.prepareSampleMetadata()` utility function, for use with loading sample metadata from an external CSV, Excel, or YAML file.
- Added `loadData()` functionality back to the package.
- Initial commit of `loadDataAsName()` function.
- Improved `annotable()` function documentation and support for Ensembl release versions.
- Improved sanitization rules for `camel()`, `dotted()`, and `snake()` name functions. Added the `strict` argument to `camel()` and `dotted()`.
- Improved the documentation for the makeNames functions, by splitting each into their own separate methods file.
- Improved S4 method support for the logRatio functions.
- Added integer support for `geomean()` function. Also improved internal code of `geomean()` based on Paul McMurdie's Stack Overflow post. See function documentation for more information.
- Added release support for `tx2gene` functions.
- Reduced the number of reexported functions (for documentation) down to the magrittr pipe (`%>%`), `Matrix()`, `DataFrame()`, and `tibble()`.
- Added silent return of file path to `saveData()` function.
- Improved documentation for tibble coercion using `as(object, "tibble")`
- Renamed `collapse()` function to `collapseToString()`, to avoid NAMESPACE collisions with tidyverse packages (dplyr, glue).


## basejump 0.0.23 (2017-09-14)

- Upgraded `annotable()` function to query Ensembl using the [ensembldb][] package rather than [annotables][].


## basejump 0.0.22 (2017-09-08)

- Improved unit testing coverage.
- Renamed `prepareSE()` to `prepareSummarizedExperiment()`. Improved row and column name handling in the function. It now outputs more helpful diagnostic messages on error.
- Reworked and simplified `detectHPC()` function to allow for unit testing.


## basejump 0.0.21 (2017-09-01)

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


## basejump 0.0.20 (2017-08-16)

- Offloaded [devtools][] functions to personal package.
- Upgraded all functions to S4 functions where possible.
- Assign utilities were kept as S3 functions since S4 dispatch makes `parent.frame()` assignment not work correctly.
- Deprecated snake_case and British spelling variants to reduce the number of exported functions.
- Added more working examples.
- Added unit testing for [annotables][] functions.


## basejump 0.0.19 (2017-07-24)

- Improved documentation and consistency with [bcbio][] packages.
- Improved integration of gene annotation calls using [annotables][] package.


## basejump 0.0.18 (2017-07-24)

- Initial support for [SummarizedExperiment][] creation with `packageSE()`.


## basejump 0.0.17 (2017-07-11)

- Added [annotables][] common code from bcbio packages.
- Added automatic file reading using [readr][] package.
- Combined write_counts functions from [bcbioRNASeq][] and [bcbioSinglecell][].
- Initial commit of `assign_data()` for use in [bcbioSingleCell][] sample loops.


## basejump 0.0.16 (2017-06-28)

- Minor NAMESPACE updates while working on [bcbio][] packages.
- Tweaks for [tidyverse][] S4 generic verbs. In particular, `as_tibble()` now provides better consistency for rowname conversion.


## basejump 0.0.15 (2017-06-18)

- Added [testthat][] support for [lintr][] checks.
- Added S4 generic for `as_tibble()`.


## basejump 0.0.14 (2017-06-13)

- [dplyr][] 0.7 NAMESPACE fixes and function tweaks.


## basejump 0.0.13 (2017-06-12)

- setMethod on tidyverse NAMESPACE collisons ([dplyr][], [tidyr][]) using `signature("data.frame").


## basejump 0.0.12 (2017-05-25)

- Updated exports based on [worminfo][] package.


## basejump 0.0.11 (2017-05-18)

- Improved naming functions to dynamically handle character vectors and objects that support naming assignments.
- Added `removeNA()` utility function.


## basejump 0.0.10 (2017-05-14)

- Added NAMESPACE utilities to deal with [tidyverse][] generic verbs.
- Switched package documentation method to use [roxygen][] with pkgapi.


## basejump 0.0.9 (2017-05-13)

- Added snake_case function variants.


## basejump 0.0.8 (2017-05-13)

- Added back `saveData()` utility functions.


## basejump 0.0.7 (2017-0422)

- Bug fixes for [dplyr][] 0.6.0 update and improved kable handling.


## basejump 0.0.6 (2017-04-14)

- Dependency fix for successful compilation on the [HMS RC][] Orchestra cluster.


## basejump 0.0.5 (2017-04-12)

- Consolidated functions in the documentation.


## basejump 0.0.4 (2017-04-07)

- Improved documentation.


## basejump 0.0.3 (2017-03-22)

- Removed dependencies and transfer functions to [bcbioRNASeq][].


## basejump 0.0.2 (2017-03-22)

- Added [bcbio][] data import functions.
- Added [ggplot2][] wrapper functions for quality control.


## basejump 0.0.1 (2017-03-17)

- Initial draft release.


[AnnotationHub]: https://bioconductor.org/packages/release/bioc/html/AnnotationHub.html
[annotables]: https://github.com/stephenturner/annotables
[bcbio]: https://github.com/chapmanb/bcbio-nextgen
[bcbioBase]: http://bioinformatics.sph.harvard.edu/bcbioBase
[bcbioRNASeq]: http://bioinformatics.sph.harvard.edu/bcbioRNASeq
[bcbioSingleCell]: http://bioinformatics.sph.harvard.edu/bcbioSingleCell
[Bioconductor]: https://bioconductor.org
[Biostrings]: http://bioconductor.org/packages/release/bioc/html/Biostrings.html
[devtools]: https://github.com/hadley/devtools
[dplyr]: http://dplyr.tidyverse.org
[ensembldb]: http://bioconductor.org/packages/release/bioc/html/ensembldb.html
[ggplot2]: http://ggplot2.tidyverse.org
[HMS RC]: https://rc.hms.harvard.edu
[lintr]: https://github.com/jimhester/lintr
[readr]: http://readr.tidyverse.org
[rlang]: http://rlang.tidyverse.org
[roxygen]: https://github.com/klutometis/roxygen
[SummarizedExperiment]: https://bioconductor.org/packages/release/bioc/html/SummarizedExperiment.html
[testthat]: https://github.com/hadley/testthat
[tidyr]: http://tidyr.tidyverse.org
[tidyverse]: http://www.tidyverse.org
[worminfo]: http://steinbaugh.com/worminfo
