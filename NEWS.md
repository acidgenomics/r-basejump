## basejump 0.13.3 (2020-11-24)

### Minor changes

- Updated dependency version cutoffs. Applies primarily to cli and magrittr.
- Now rexporting new `sem` function from AcidBase.

## basejump 0.13.2 (2020-10-09)

### Minor changes

- Migrated `transmit` to pipette package. Still reexporting here.

## basejump 0.13.1 (2020-10-07)

### Minor changes

- Reworked `makeSummarizedExperiment` and `makeSingleCellExperiment` generators
  to generate empty objects without requiring any assays to be defined.

## basejump 0.13.0 (2020-10-05)

Migrated some functions to AcidBase, AcidGenomes, AcidPlyr, and pipette
packages.

### Major changes

- `Ensembl2Entrez`: Improved internal matching engine, to allow reverse matching
  of Entrez identifiers to Ensembl identifiers.
- Now exporting `Entrez2Ensembl`, which works like `Entrez2Ensembl`.
- New S4 class `Entrez2Ensembl`, which inherits `Ensembl2Entrez` structure.

### Minor changes

- `makeSummarizedExperiment`: No longer requiring primary `assay` defined to
  be named "counts". This isn't appropriate for `SummarizedExperiment` objects
  defined in the new DepMapAnalysis package.
- `makeGRangesFromEnsembl`: Include `geneSynonyms` column for supported
  organisms, including *Homo sapiens*.
- Deprecated `matchEnsemblReleaseToURL` in favor of `mapEnsemblReleaseToURL`.

## basejump 0.12.16 (2020-09-29)

### Minor changes

- `makeProtein2GeneFromEnsembl`: Improved error message on match failure. Now
  returns the protein IDs that failed to match more clearly.

## basejump 0.12.15 (2020-09-25)

### New classes

- `Protein2Gene`: `DataFrame` with `proteinID`, `geneID`, and `geneName`
  columns. Use corresponding `makeProtein2GeneFromEnsembl` to generate object
  simplify using Ensembl protein IDs as input.

### New functions

- `getEnsDb`: Now exporting function that was used internally to obtain `EnsDb`
  object from AnnotationHub.
- `makeProtein2GeneFromEnsembl`: New utility function that takes Ensembl protein
  identifiers as input, and returns corresponding gene identifiers and gene
  names (i.e. HUGO gene symbols).

### Minor changes

- Reworked some internal code in `makeGRangesFromEnesmbl` to enable export of
  new `getEnsDb` function.
- Reworked internal handling of AnnotationHub and EnsDb metadata.
- `makeGRangesFromEnsembl` / `getEnsDb`: Improved sorting of Ensembl releases
  so that current release greater than 99 returns as expected. Since Ensembl is
  now at 101, we need to convert to integers internally instead of attempting
  to sort on strings.

## basejump 0.12.14 (2020-09-14)

### New functions

- `splitByLevel`: Easily split a data frame into a list using a defined factor
  column (`f` argument). Can easily include the reference level with
  `ref = TRUE`, which is useful for statistical calculations on pairwise
  contrasts.

## basejump 0.12.13 (2020-08-25)

### Minor changes

- Migrated some globals to acidbase package, for improved consistency.

## basejump 0.12.12 (2020-08-18)

### New methods

- `intersectAll`: Defined `list` method.
- `intersectionMatrix`: Defined `list` method.

## basejump 0.12.11 (2020-08-13)

### Minor changes

- Now reexporting `requireNamespaces` from acidbase package.

## basejump 0.12.10 (2020-08-04)

### Minor changes

- Migrated `alphaThreshold` and `lfcThreshold` methods to DESeqAnalysis package.
  These are not used in other packages and may not be generally applicable to
  `SummarizedExperiment` class, so rethinking here.

## basejump 0.12.9 (2020-07-24)

### Minor changes

- Maintenance release, updating minimum R dependency to 4.0.

## basejump 0.12.8 (2020-06-15)

### Minor changes

- `autopadZeros`: Migrated character method support to syntactic package, since
  this is useful for low-level code run inside koopa.

## basejump 0.12.7 (2020-05-22)

### Minor changes

- `aggregate`, `aggregateCols`, `aggregateRows`: Relaxed assert checks on
  validity of dimnames, so we can use internally in acidgsea package, which
  needs to handle gene symbols containing syntactically invalid hyphens.

## basejump 0.12.6 (2020-05-17)

### Minor changes

- `convertGenesToSymbols`: Added method support for `GRanges` class objects.
  Automatically sets names as unique gene symbols.
- `HGNC2Ensembl`: Enforce TSV handling internally in `import` call.
- `MGI2Ensembl`: Fix for column name handling.

## basejump 0.12.5 (2020-05-11)

### Minor changes

- `importSampleData`: The `pipeline = "cpi"` option is now defunct.
  Use `pipeline = "none', sheet = 2L` for CPI samples.
- `importSampleData`: Added optional `autopadZeros` argument for easy handling
  of sample identifiers, which are often input by users without correct padding.
  This helps easily convert sample_1, sample_2, ... sample_10 to expected
  sample_01, sample_02, ... sample_10 sorting. Currently disabled by default.
- `importTx2Gene`: Added `ignoreGeneVersion` option, now enabled by default.
  This helps process gene identifiers by default in a manner suitable for
  downstream tximport-DESeq2 workflow.
- `headtail`: Removed Unicode support in favor of simple ASCII return, to avoid
  build warnings in latest R 4.0 release version.
- Miscellaneous unit test updates to reflect changes in `DataFrame` class and
  NCBI server updates.
- `camelCase`, `dottedCase`, `organism`, `snakeCase`, `upperCamelCase`: S4
  methods for `DataFrame` are now defined directly against `DataFrame`, instead
  of attempting to inherit from `DataTable` virtual class. This will break
  otherwise on bioc-devel 3.12, which seems to have changed inheritance.

## basejump 0.12.4 (2020-03-15)

### Minor changes

- `matchEnsemblReleaseToURL`, `matchHumanOrthologs`: update unit tests to
  reflect Ensembl server migration, which has rendered Ensembl archives
  inaccessible via biomaRt until March 24th. Unit tests now check against
  current release instead of a pinned archive release.

## basejump 0.12.3 (2020-02-24)

### Minor changes

- `makeSingleCellExperiment`, `makeSummarizedExperiment`: Removed `spikeNames`
  support due to a breaking change in SingleCellExperiment, which has removed
  `isSpike` in favor of `altExps`. Refer to the SingleCellExperiment
  documentation for updated current best practice for handling spike-in
  transcripts, which now requires slotting a separate SummarizedExperiment
  object containing things like ERCCs inside the main SingleCellExperiment.

## basejump 0.12.2 (2020-02-18)

### Minor changes

- Migrating `pseudobulk` methods to pointillism package. Rethinking the approach
  used here to work better with per-cluster aggregation operations. Will be
  updated in the next pointillism release.

## basejump 0.12.1 (2020-01-30)

### Major changes

- Reworked `aggregate`, `aggregateCols`, `aggregateRows` support to reflect
  internal migration away from Matrix.utils dependency.

### Minor changes

- NAMESPACE fix for unexpected removal of Matrix.utils from CRAN. Now defining
  the previously imported aggregate.Matrix function directly here in basejump.
- `aggregate`: Now defining matrix method.
- `aggregate*` generics now consistently use "x" instead of "object".

## basejump 0.12.0 (2020-01-20)

### Major changes

- Migrated functions from brio (now renamed to pipette), freerange, and
  transformer, in preparation for Bioconductor submission and reviews.
- Updated messages to utilize the cli package.

## basejump 0.11.24 (2020-01-08)

### New functions

- `integerCounts`: Simple method support for returning a rounded integer counts
  matrix. Intended primarily for downstream handoff to bulk RNA-seq differential
  expression callers, such as DESeq2.

### Minor changes

- Updated dependency package version requirements. Needed primarily for bug
  fix in transformer, improving `mutateAll` functionality.

## basejump 0.11.23 (2019-11-19)

### Minor changes

- Updated package documentation to support roxygen2 7.0 update.
- Reworked `formalsList` global slightly. Using the `synesthesia` color palette
  for `heatmap.color` argument doesn't always perform well enough, so I'm
  switching to a blue/black/yellow palette defined by `blueYellow` in acidplots
  instead for `heatmap.color`. The `synesthesia` palette performs really well
  for correlation heatmaps, and is now recommended by default via the
  `acid.heatmap.correlation.color` global now instead.
- `filterCells`: Improved internal sampleName / sampleID handling.

## basejump 0.11.22 (2019-11-11)

### New functions

- `correlation`: Added S4 method support that mimics base `cor` methods, but
  is more flexible, supporting additional arguments via `...` in generic. This
  way we can provide intelligent and quick correlation calculations for nested
  assays inside a `SummarizedExperiment` and for `DESeqResults`
  (see DESeqAnalysis package).

## basejump 0.11.21 (2019-11-07)

### Major changes

- Updated Bioconductor dependencies to require new 3.10 release.

### Bug fixes

- `filterCells` requires an internal `decode` step to handle `Rle` evaluation,
  which worked previously in Bioconductor 3.9 release.
- Updated unit tests to reflect `SingleCellExperiment` example object resave
  in acidtest 0.2.7 update, which changed the numbers.

## basejump 0.11.20 (2019-10-24)

### New reexports

- Reexporting `metadata2` and `metadata2<-` functions from transformer.
  These will be used internally in the pending DESeqAnalysis update.

## basejump 0.11.19 (2019-10-23)

### New functions

- Genome version detection: `ensemblVersion`, `gencodeVersion`, `refseqVersion`,
  `flybaseVersion`, `wormbaseVersion`. Similar shell variants are available in
  the [koopa][] package.

### Minor changes

- Moved some low-level functions to new [acidbase][] package. Updated NAMESPACE
  and reexports to reflect these changes.

## basejump 0.11.18 (2019-10-13)

### Minor changes

- `importSampleData`: Pipeline now defaulting to "none" instead of "bcbio",
  since this flag is now properly hard coded in bcbio R packages.
  Added new Constellation (CPI) pipeline option.
- `makeSampleData`: Now checks for all NA columns and rows, similar to approach
  in `importSampleData`. This helps improve return consistency. Automatic
  rowname setting has been tweaked a bit to no longer attempt to remove original
  ID column.

## basejump 0.11.17 (2019-10-09)

### Minor changes

- `makeSampleData`: Made function slightly more flexible. Now allowing automatic
  rowname coercion from columns ("sampleID", "rowname", "rn"), similar to
  approach employed by data.table and tibble packages.
- Now exporting `stripGeneVersions` alias, which uses the same code internally
  as `stripTranscriptVersions`.

### Disabled methods

- Disabled DelayedArray class methods for `calculateMetrics`,
  `estimateSizeFactors`, and `nonzeroRowsAndCols` until `is_pristine` bug in
  DelayedArray v0.11.8 is fixed on Bioconductor Devel (3.10). This is causing
  unit tests to fail otherwise.
  See [related issue](https://github.com/Bioconductor/DelayedArray/issues/55)

## basejump 0.11.16 (2019-09-25)

### Minor changes

- Migrated S4 methods from syntactic that work on Bioconductor classes here.
  This keeps the syntactic package very light weight and focused only on
  character string sanitization.

## basejump 0.11.15 (2019-09-16)

### Major changes

- `melt`: Updated `min` and `minMethod` defaults for `matrix` method. The `min`
  argument now defaults to `-Inf`, and the `minMethod` now defaults to
  `"absolute"`, instead of `"perRow"`, since this behavior is more intuitive to
  the user.

### Minor changes

- `nonzeroRowsAndCols`: Added `assay` argument, switching from internal `counts`
  usage, to make the `SummarizedExperiment` method more flexible.

## basejump 0.11.14 (2019-09-09)

### Minor changes

- `mcolnames`: Moved S4 methods previously defined in syntactic here.
- Improved website documentation.

## basejump 0.11.13 (2019-09-06)

### Major changes

- `autopadZeros`: Added improved support for detection and automatic handling
  of zeros in need of padding on the left side of a character vector. This
  addition is necessary for handling of Genewiz processed FASTQ file names.
- `makeTx2Gene` functions now support `ignoreTxVersion` argument, similar to
  conventions defined in tximport package.

### Minor changes

- `importSampleData`: Updated to tentatively support a general pipeline via the
  "none" argument. In this case, only a `sampleID` column is required in
  metadata. This is been developed in conjunction with my new Genewiz to
  kallisto Nextflow processing pipeline being implemented at CPI.
  

## basejump 0.11.12 (2019-09-03)

### Minor changes

- `melt`: Added method support for contingency `table` class.
- Removed `set_*` reexports from magrittr packages.

## basejump 0.11.11 (2019-08-27)

Updated R dependency to 3.6.

### New functions

- `melt`: Added S4 methods for melting data into long format. Provides support
  for `matrix`, `Matrix`, `DataFrame`, `SummarizedExperiment`, and
  `SingleCellExperiment` currently.
- `nonzeroRowsAndCols`: Quickly remove non-zero rows and columns from a matrix
  or `SummarizedExperiment`.

### Major changes

- Migrated `EggNOG` and `PANTHER` S4 classes to separate packages.

### Minor changes

- `calculateMetrics`: Now calls `nonzeroRowsAndCols` internally first when
  `prefilter = FALSE`, speeding up calculations significantly for very large
  `SingleCellExperiment` objects. This was added to improve loading of example
  unfiltered 10X Genomics Chromium data.

### Deprecations

- Deprecated `readSampleData` and `readTx2Gene` in favor of `importSampleData`
  and `importTx2Gene`, respectively.

## basejump 0.11.10 (2019-08-22)

### Minor changes

- `makeSummarizedExperiment`: Now automatically handles non-Ensembl gene symbols
  present in assays that aren't defined in `rowRanges`. This applies primarily
  to 10X Cell Ranger v3 output, which includes some non-Ensembl gene symbols:
  CD11b, CD4, CD8a, HLA-DR, IgG1, PD-1, etc. The function still intentionally
  errors on unannotated Ensembl features, which often indicates an accidental
  release version mismatch.
- Updated dependency versions.

## basejump 0.11.9 (2019-08-21)

### Minor changes

- `meltCounts`: Added initial method support for `SingleCellExperiment`.
  Currently requires deparsing of the count matrix to call `reshape2::melt`.
  Returns columns with S4 run-length encoding (Rle) applied.
- `mapGenes`: Converted warning to message when `strict = FALSE`.

## basejump 0.11.8 (2019-08-19)

### New functions

- Migrated `readSampleData` and `readTx2Gene` from bcbioBase.
- Reworked `readSampleData` internal code, but still supporting bcbio pipeline
  conventions (i.e. "description" column for samples) as the default. I've
  reworked this approach so we can also call `readSampleData` inside the
  Chromium package (for 10X Genomics single-cell RNA-seq data) without having
  to depend on the bcbio R packages.
- Reexporting new dplyr-like methods that support S4 `DataFrame`:
  `inner_join`, `left_join`, `right_join`, `full_join`, `anti_join`;
  `mutate_all`, `mutate_at`, `mutate_if`;
  `select_all`, `select_at`, `select_if`.

### Major changes

- Removed dplyr and magrittr dependencies in internal code, where applicable.
- `aggregateCols`: Sped up return by calling `SingleCellExperiment` rather than
  `makeSingleCellExperiment`. Note that this now doesn't return session
  information in the object.
- `cell2sample`: Renamed return from `tibble` to `tbl_df`, for consistency.
- Made some previous deprecated functions still used in bcbioRNASeq v0.2 release
  series (which has now been updated to v0.3) defunct. This applies primarily
  to `assert*` functions that have been reworked using a goalie approach.
- `estimateSizeFactors`: Removed option to calculate "deseq2-median-ratio"
  using DESeq2. We may revisit this idea in a future release.
- `makeSampleData`: Improved internal code for `DataFrame` method.

### Minor changes

- Updated basejump dependencies, in preparation for bioconda release update
  supporting bcbioRNASeq.
- Now importing AnnotationDbi, BiocParallel, Biostrings, and biomaRt.
- Safe to import `select` from AnnotationDbi, now that we're no longer depending
  on dplyr.
- Switched to using `droplevels` instead of `relevel` on S4 objects internally,
  where applicable. This is better supported by S4Vectors package.
- Simplified reexport documentation for S4 functions and methods.

## basejump 0.11.7 (2019-08-13)

### Minor changes

- `filterCells`: Improved downstream handling of `nCells` argument.
  Ensure double filtering is still allowed.

## basejump 0.11.6 (2019-08-12)

### Minor changes

- Updated goalie dependency.
- Tightened up `appendToBody` and `methodFormals` calls to have better backward
  compatibility with R 3.5.
- `calculateMetrics`: Bug fix for getting `rowData` via `mcols` without
  `use.names = TRUE`. This improves Bioconductor backward compatibility.
  Also updated internal code to call `hasMetrics` from goalie.
- Made `separatorBar` defunct. Use `separator` function instead.

## basejump 0.11.5 (2019-08-11)

### New functions

- `calculateMetrics`: Migrated code here from bcbioSingleCell. Improved method
  to support `DelayedArray` class for large matrices.

### Minor changes

- Improved code coverage and adjusted unit tests for breaking changes seen due
  to new covr update.

## basejump 0.11.4 (2019-08-07)

### Major changes

- `makeSampleData`: Switched to S4 method that works on `data.frame` and
  `DataFrame` class objects. Enforcing lower camel case for column names.
- `makeSummarizedExperiment` and `makeSingleCellExperiment`: Switched to S4
  method approach that currently requires `SimpleList` input for assays.
  Previously, we allowed `list` input for assays, but we're tightening this up
  to simply the package code base and improve consistency.
- `aggregate` methods now consistently return the primary assay named as
  `counts`. This follows the recommended conventions defined in
  SingleCellExperiment. Aggregation functions will now intentionally fail for
  `SummarizedExperiment` objects that don't contain an assay named `counts`.

### Minor changes

- Improved documentation consistency by offloading `params.Rd` file to new
  acidroxygen package. This will be linked in the other Acid Genomics packages.
- Updated unit tests to follow new package conventions (see above changes).

## basejump 0.11.3 (2019-07-30)

100th release!

### Minor changes

- `mapCellsToSamples`: Relaxed grep matching on `cells` input to support legacy
  bcbioSingleCell objects. This change was needed to improve `updateObject`
  method in the upcoming bcbioSingleCell update.

## basejump 0.11.2 (2019-07-29)

### Minor changes

- Reexporting new functions in syntactic: `makeLabel`, `makeTitle`, `makeWords`.
- Updated documentation to include modification timestamp.

## basejump 0.11.1 (2019-07-24)

### Minor changes

- Added back deprecated assert checks that are required for bcbioRNASeq v0.2
  release series.

## basejump 0.11.0 (2019-07-22)

Start of new release series. Version bump reflects changes in dependency
packages. See the acidtest, bioverbs, freerange, syntactic, and transformer
release notes for more details.

## basejump 0.10.12 (2019-07-18)

### Minor changes

- Bug fix for `combine` method on SummarizedExperiment. Needed to ensure row
  names are assigned on rowData to provide backward compatibility for Bioc 3.7.
- Improved unit test exceptions on Docker and AppVeyor.
- Improved installation instructions.
- `mapGenesToRownames`: Improved matching for `SummarizedExperiment` objects
  that don't contain gene-to-symbol mappings defined in `rowData`.
- `meltCounts`: Improved factor handling. Also added `matrix` method support.
  Added advanced option to disable `minCounts` filtering, by setting as `NULL`.

### Deprecations

- Tightened up the list of deprecated functions.

## basejump 0.10.11 (2019-07-17)

### Minor changes

- Made `theme_midnight` and `theme_paperwhite` defunct, in favor if variants in
  new [acidplots][] package.
- Decreased the number of functions reexported from goalie package.
- Updated basejump dependency package versions.
- Improved Travis CI Docker configuration.

## basejump 0.10.10 (2019-06-08)

### New functions

- `matchEnsemblReleaseToURL`: Takes an Ensembl release version (e.g. `96`) as
  input and returns the corresponding archive URL.
- `matchHumanOrthologs`: Convenience function that wraps biomaRt package to
  map model system gene identifiers to HGNC IDs and symbols. This is
  particularly useful for running orthologus GSEA with our pfgsea package.

### Major changes

- `combine` method for `SummarizedExperiment` now includes all matrices defined
  in `assays` slot. Also improved support for `colData` handling on subsets
  where `NA` values have been removed.

### Minor changes

- `Gene2Symbol`: Modified `format` formal to use "unmodified" instead of "long",
  which is more intuitive.
- `makeGene2Symbol`: Added support for `format` argument, similar to
  `Gene2Symbol` generator function.

## basejump 0.10.9 (2019-05-29)

### Minor changes

- Relaxed the deprecations on some functions to provide backward compatibility
  support for bcbioBase and bcbioRNASeq packages: `readFileByExtension`,
  `readYAML`, `fixNA`.
- Now ensuring `theme_midnight` and `theme_paperwhite` are deprecated but
  exported with support, by suggesting acidplots package.
- Added back defunct function warnings: `assertHasRownames`, `tx2geneFromGFF`.

## basejump 0.10.8 (2019-05-05)

### Major changes

- Now pinned to R >= 3.5.

## basejump 0.10.7 (2019-04-30)

### New functions

- Added method support for `alphaThreshold` and `lfcThreshold` against
  `Annotated` class. These values get stored in the `metadata` slot of the
  object.

## basejump 0.10.6 (2019-04-29)

### New functions

- `showHeader`: Utility function for `show` methods defined in other packages.

### Minor changes

- Updated basejump dependencies (see `DESCRIPTION` for details).

## basejump 0.10.5 (2019-04-25)

### Minor changes

- Reworked S4 generic reexport method, in an attempt to get pkgdown to build
  vignette correctly. Otherwise, `sampleData` is erroring.

## basejump 0.10.4 (2019-04-22)

### Major changes

- `meltCounts`: Switched from using `nonzeroGenes` formal approach to
  `minCounts` and `minCountsMethod`, which is more flexible.

### Minor changes

- Consolidated minimalism and firestarter code into acidplots package.
- Improve global options defined in `formalsList`.
- Added back basejump vignette.

## basejump 0.10.3 (2019-04-07)

### Minor changes

- Bug fix release for [freerange][] update. `emptyRanges` format has been
  renamed from `mcolsNames` to `mcolnames`.

## basejump 0.10.2 (2019-04-07)

### Major changes

- `aggregateRows`, `aggregateCols`, `aggregateCellsToSamples`: Improved internal
  code for SummarizedExperiment metadata handling. Applies primarily to colData
  and rowData juggling for these methods.
- `interestingGroups`: Reworked to define method against `Annotated` class.
  `SummarizedExperiment` inherits from this class, supporting `metadata`.
- `mapCellsToSamples`: Tightened up match assert checks.
- `mcolnames` now uses S4 methods, primarily against `Vector` class.
- `organism`: Reworked S4 methods. Added support for `Annotated` and
  `DataTable` classes from [S4Vectors][].

### Minor changes

- Added additional unit tests, to improve code coverage.
- Now covering the aggregation functions in better detail, using a minimal
  SingleCellExperiment object that works with `aggregateCols`.
- Consistently use "acid" prefix instead of "basejump" for global options.
- Miscellaneous working example improvements.
- `detectLanes`: Renamed primary argument from `object` to `path`. Improved
  `pattern` formal to evaluate `lanePattern` global.
- For S4 generators, renamed test paramter from `basejump.test` to `acid.test`.
- `sampleData`: Improved error message when `sampleName` factor column is
  missing.
- `zeroVsDepth` now returns `depth` column as `integer` instead of `numeric`.

## basejump 0.10.1 (2019-04-01)

### New functions

- `rankedMatrix`: New utility function for quickly performing ranked matrix
  calculations. Particularly useful for differential expression comparison
  across studies using log2 fold change or Wald test statistic.

### Minor changes

- Simplified reexport methods for functions that define S4 methods.

## basejump 0.10.0 (2019-03-28)

### Major changes

- Split out Ensembl (AnnotationHub/ensembldb) annotation processing and GFF/GTF
  file loading utilites to new [freerange][] package. All of these functions
  remain re-exported here in [basejump][]. This includes: `annotable`,
  `convertUCSCBuildToEnsembl`, `detectOrganism`, `emptyRanges`,
  `makeGRangesFromEnsDb`, `makeGRangesFromEnsembl`, `makeGRangesFromGFF`,
  `makeGRangesFromGTF`.
- Split out heatmap functions to firestarter package. This includes
  `plotHeatmap`, `plotCorrelationHeatmap`, and `plotQuantileHeatmap`.
- Split out all ggplot2 functions to new minimalism package.

### Minor changes

- Offloaded `removeNA` and `sanitizeNA` code to [brio][] package, so these
  functions can be imported in thew new [freerange][] package.
- Moved `organism_mappings` internal dataset to [freerange][] package.

## basejump 0.9.16 (2019-03-23)

### Minor changes

- `convertSampleIDsToNames`: Removed code to assign `sampleName` column to
  `NULL`. This step doesn't work consistently for `DataFrame` across
  Bioconductor installations, and has been found to error on R 3.4 and the
  current bioc-devel on AppVeyor.
- Miscellaneous documentation fixes, removing extra formatting in titles.
- `makeGRangesFromGFF`: Compressed Ensembl GTF file example was erroring out
  on AppVeyor CI, due to Windows' poor handling of temp files on non-admin
  accounts. Switched to a non-gzipped example file to avoid this issue. Also
  removed tabular table from documentation, which currently doesn't render
  correctly via pkgdown.

## basejump 0.9.15 (2019-03-22)

### Minor changes

- Migrated code to [Acid Genomics][].

## basejump 0.9.14 (2019-03-18)

### Minor changes

- Additional bug fixes for `sampleData` and blacklisted metadata handling.

## basejump 0.9.13 (2019-03-18)

### Minor changes

- Updated dependencies, specificially brio and goalie.
- Miscellaneous documentation improvements.

## basejump 0.9.12 (2019-03-11)

### Major changes

- `makeGRangesFromGFF`: Reworked internal code, making it more modular. Added
  initial support for RefSeq GFF3 files. Also improved sanitization and special
  handling of files from FlyBase and WormBase.

### Minor changes

- `plotHeatmap` family: Bug fix needed for internal `is.na` call on
  `annotationCol`, which should be wrapped with `any` to return boolean. This
  errors (as it should) on R 3.6, but I missed it on R 3.5.
- Consolidating GRanges return code defined in `.makeGRanges`, which is run
  for both GFF file and ensembldb import. We've improved the Rle encoding steps
  here to work with complex GFF3 files (e.g. GENCODE).

## basejump 0.9.11 (2019-02-25)

### Minor changes

- Updated [transformer][] package reexports to include new [data.table][]
  coercion methods.
- Added additional useful compression function reexports from [brio][] package.

## basejump 0.9.10 (2019-02-17)

### Minor changes

- Working on making the current basejump code base completely backward
  compatible with bcbioBase v0.4.1 and bcbioRNASeq v0.2.8 release series.
- Now reexporting `goalie::bapply` and additional useful pipes from [magrittr][]
  package.
- Keeping the now deprecated `plotGene` generic reexported, while encouring
  users to update their code to use `plotCounts` instead.
- `interestingGroups`: Simplified internal assert checks, removing
  `matchesInterestingGroups`, which can become circular.

## basejump 0.9.9 (2019-02-11)

### Minor changes

- Code fixes to provide backward compatibility support for R 3.4. Tested using
  R 3.4.1 with Bioconductor 3.6 release.
- Needed to add `unname` to some assert checks for expected `logical(1)` return,
  which only happens in R 3.4 but not R 3.5.
- `uniteInterestingGroups`: Improved internal assert checks and name handling.

## basejump 0.9.8 (2019-02-08)

### Minor changes

- Deprecating `plotGene` in favor of `plotCounts`. This change will be reflected
  in future updates of packages that depend on [basejump][], including the bcbio
  R packages.
- Split out subpackage reexports into separate files.
- Reexporting `assert` from goalie package.

## basejump 0.9.7 (2019-01-23)

### Minor changes

- `decode` and `encode` are properly reexported from [brio][].
- Updated Travis CI and AppVeyor CI configurations.

## basejump 0.9.6 (2019-01-22)

### Minor changes

- Note that S4Transformer package import has been renamed to transformer.
- Needed to add `decode` call internally for some plotting functions, to ensure
  that run-length encoded (Rle) rowData gets handled correctly.
- Bug fix for internal `interestingGroups` handling in plot functions.

### Offloaded to brio

- `decode`, `encode`. These are useful for data sanitization. Still re-exported
  here in [basejump][].

### Offloaded to goalie

- `printString`. This is a low-level function that is useful for setting the
  cause attribute in error messages. Still reexported here in basejump.

### Deprecations

- `sanitizeRowData` has been deprecated in favor of `atomize`.
- `sanitizeAnnotable` deprecation has been updated to point to `atomize`.

## basejump 0.9.5 (2019-01-22)

### Minor changes

- Updated basejump subpackage dependencies.
- Needed to add `decode` call internally for some plotting functions, to ensure
  that run-length encoded (Rle) rowData gets handled correctly.
- Bug fix for internal `interestingGroups` handling in plot functions.

### Offloaded to brio

- `decode`, `encode`. These are useful for data sanitization. Still re-exported
  here in [basejump][].

### Offloaded to goalie

- `printString`. This is a low-level function that is useful for setting the
  cause attribute in error messages. Still reexported here in basejump.

### Deprecations

- `sanitizeRowData` has been deprecated in favor of `atomize`.
- `sanitizeAnnotable` deprecation has been updated to point to `atomize`.

## basejump 0.9.4 (2019-01-12)

### Minor changes

- Consolidated reexports from [basejump][] sub-packages into `reexports.R` file.
- Miscellaneous documentation updates, improving link appearance for functions
  exported in other packages.

## basejump 0.9.3 (2019-01-08)

### Minor changes

- Reorganized imports in `DESCRIPTION` file to make them more human readable.
  Note that [basejump][] sub-packages are imported first, then [Bioconductor][]
  packages, followed by [CRAN][] packages, and required default packages.
- Split out NAMESPACE imports into a separate `imports.R` file.

## basejump 0.9.2 (2019-01-07)

This release defines the initial point where [basejump][] becomes even more
modular, offloading some functions to new [brio][], [syntactic][], and
S4Transformer packages.

Note that all offloaded functions will continue to be reexported in
[basejump][]. If you notice a function that is missing and not correctly
re-exported, please file an issue.

Note that S4Transformer has since been renamed to [transformer][].

### Offloaded to S4Transformer

- `as` coercion methods moved to S4Transformer package. This methods define our
  useful interconversions between Bioconductor and tidyverse data classes,
  including `DataFrame` and `tbl_df` (tibble).
- `coerceS4ToList` / `flatFiles`.

### Offloaded to bb8

- `cleanSystemLibrary`. This doesn't scale well to all installations and is
  really only intended for personal use, so bb8 package is more appropriate.
- Documentation functions, including `parseRd`, `RdTags`, `saveRdExamples`, and
  `tabular` are outside the scope of [basejump][].

### Offloaded to brio

- `basenameSansExt`.
- `dots`.
- `export`.
- `import`.
- `initDir`.
- `loadData`.
- `localOrRemoteFile`.
- `pasteURL`.
- `realpath`.
- `sanitizeColData`.
- `sanitizeRowData`.
- `sanitizeSampleData`.
- `saveData`.
- `transmit`.
- `writeCounts`.

### Offloaded to goalie

- `matchArgsToDoCall`.
- `MethodDefinition`.
- `standardizeCall`.

### Minor changes

- Added `nullOK` support to [goalie][] assert checks, where applicable.

## basejump 0.9.1 (2018-12-22)

This release defines the initial point where [basejump][] begins to import
[bioverbs][].

### Major changes

- Now importing generics using our [bioverbs][] S4 generic package. All generics
  previously defined in [basejump][] will continue to be reexported, to maintain
  backward compatibility for reverse dependencies (revdeps).

### Minor changes

- `aggregateCellsToSamples`: Split out S4 method to a separate file. Previously
  was defined in `aggregate-methods.R`.
- Reorganized collapse family of functions. Refer to changes in
  `collapse-methods.R`, which is now split out to `collaseToString-methods.R`.
- Split out the markdown family of functions back out into separate files.
- `matchesGene2Symbol`, `matchesInterestingGroups`: Reworked internal code and
  moved to separate files. No longer relies upon `makeTestFunction` from
  [checkmate][] package.

### Documentation

- Switched documentation titles to sentence case from title case. It's generally
  more readable.

## basejump 0.9.0 (2018-12-12)

I bumped the release series from v0.8 to v0.9 because this represents a
significant change to the internal codebase, where I have now switched to using
my new [goalie][] assert check engine from [assertive][].

### New functions

- `decode`: Decode S4 run-length encoding (Rle).
- `encode`: Apply S4 run-length encoding (Rle).
- `geneNames`: Convenience function that returns gene names (symbols) mapped to
  the stable, but not human-friendly gene identifiers.
- `matchesGene2Symbol`, `matchesInterestingGroups`: New functions designed to
  match corresponding `Gene2Symbol` objects or `interestingGroups`.
- `pasteURL`: Convenience function that generates URL strings.
- `sanitizeColData`: rework of previous `sanitizeSampleData` approach.

### Major changes

- Now using [goalie][] package instead of [assertive][] for internal assert
  checks. The new `goalie::assert` function is more flexible in many cases.
  Similarly, `goalie::validate` is now being used in place of
  `assertthat::validate_that`.
- `plotHeatmap` now calculates the z-score normalization internally, rather than
  relying upon the codebase inside [pheatmap][].
- `export`: Improved `SummarizedExperiment` method to also write `Gene2Symbol`
  and `Ensembl2Entrez` mappings to disk, when defined. The human-friendly output
  formal has been renamed from `human` to `humanize`, to reflect an action
  (verb). This corresponds better to our `humanize` generic function. Also
  reworked some internal code that handles output of colData and rowData to
  disk.
- `makeGRangesFromEnsembl`: Switched to using S4 run-length encoding (Rle) in
  our metadata column (mcols) return. This functionality matches the conventions
  used by GenomicRanges in the `GRanges` return, and reduces the memory
  footprint of very large annotation objects.

### Minor changes

- Note that some internal instances of `has_length` aren't quite strict enough.
  Switch to using `length(x) > 0L` or improved `hasLength` assert check defined
  in the [goalie][] package.
- `geneSynonyms`: Switched to using new `pasteURL` function internally instead
  of using `paste` with `/` separator.
- `HGNC2Ensembl` generator: switch to using `pasteURL` internally.
- `loadData`, and other related load family functions: simplified internal code
  using our new [goalie][] asserts.

### Deprecations

- Removed `assertFormalGene2symbol` from deprecations.

### Documentation

- Improved documentation style throughout the package, switching from the usage
  of scalar types like `string` to `character(1)`, and `boolean` to
  `logical(1)`. This better matches the actual data structure in R. Some other
  packages like [checkmate][] also use this convention, which I think is more
  readable than my previous approach.

## basejump 0.8.8 (2018-12-03)

### New functions

- `deg`: Utility function to quickly obtain differentially expressed gene
  (DEG) identifiers as a character vector.

### Major changes

- `plotHeatmap`: Now defining row scaling internally, rather than relying on the
  functionality defined in `pheatmap`.

## basejump 0.8.7 (2018-12-01)

### New functions

- `humanize`. New generic that enables easy conversion to human-friendly column
  and/or row names. Useful for CSV file export in particular.

### Minor changes

- `export`: Added `humanize` argument support.
- Improved grep pattern matching inside `makeNames` functions
  (e.g. `.sanitizeAcronyms`).
- `sampleData`: Improved blacklist pattern matching against `Seurat` objects
  for `SingleCellExperiment` method.

## basejump 0.8.6 (2018-11-30)

### Minor changes

- Now importing `hasUniqueCols` from goalie. Switched from previous approach
  using `areSamplesUnique`.
- Documentation improvement for `genomeBuild`, used in `makeGRanges` functions.
- Miscellaneous documentation improvements to pass build checks.

## basejump 0.8.5 (2018-11-29)

### New functions

- `relevelRowRanges`, `relevelColData`.

### Deprecations

- `markdownPlotlist`. Renamed to `markdownPlots`.

### Minor changes

- `autopadZeros`: Improved internal code to keep track of names for `character`
  method. Also added method support for `SummarizedExperiment`, which works on
  the column names (e.g. sample names) only by default.
- `Ensembl2Entrez`: Simplify validity check to require `integer` in `entrezID`
  column. Also reworked and improved internal code that supports run-length
  encoding (Rle) using `decode`.
- `export`: Improved documentation for `name` argument.
- `makeGRangesFromEnsembl`: Improved messages to user.
- `plotCountsPerBiotype`: Improved error messages for when the biotype isn't
  defined.
- `plotPCA`: Add support for unique sample detection with `areSamplesUnique`,
  similar to the approach used in the bcbioRNASeq quality control R Markdown.
- `sampleNames`: Simplified `sampleName` extraction, using `[[` internally.
- Miscellaneous documentation fixes.
- Updated unit test for `sanitizeRowData`.

## basejump 0.8.4 (2018-11-28)

### New functions

- `autopadZeros`. Useful for padding zeros inside of a character vector.
- `basenameSansExt`. Quickly get the basename without the file extension for
  desired file path(s). Surprisingly, this isn't defined in the tools package
  so I wrote my own.
- `cleanSystemLibrary`. I ran into some shared library configuration issues
  on the Azure infrastructure, so this utility function is useful for checking
  wheter an R installation has a clean library.
- `plotGenderMarkers`: Migrated the `SummarizedExperiment` method from
  bcbioRNASeq package.

### Minor changes

- Split out call functions defined in `calls.R` into separate R files.
  Refer to `dots`, for example.
- Split out `environment.R` into separate R files. See `detectHPC` for example.
- Miscellaneous documentation improvements.

## basejump 0.8.3 (2018-11-25)

### Minor changes

- Improved usage of `assertthat::validate_that` in S4 class validity checks.
  Removed former approach using internal `.valid` function.
- Temporarily soft deprecated some functions that will be formally deprecated
  in a future release. See `deprecated.R` file.
- `interestingGroups` doesn't attempt validity check using `validObject` by
  default, which can be enabled instead using `check = TRUE`.
- `sampleData`: Tightened up internal assert checks.
- `PANTHER`: Minor tweaks to internal variable names inside `.splitTerms`.
- `transmit`: Improved messages. Temporarily disabled working example, since it
  consistently fails on [Travis CI][].

## basejump 0.8.2 (2018-11-19)

### Major changes

Migrating some additional base code that can be dispatched on either
`SummarizedExperiment` or `SingleCellExperiment` from the bcbio R packages.
We're going to split out some of the single-cell RNA-seq functionality into
separate packages, since a lot of my work moving forward deals with the 10X
Genomics Cell Ranger platform, rather than the bcbio-supported inDrops platform.

In particular:

- `barcodeRanksPerSample`.
- `filterCells`.
- `plotBarcodeRanks`.
- `plotCellCounts`.
- `plotGenesPerCell`.
- `plotMitoRatio`.
- `plotMitoVsCoding`.
- `plotNovelty`.
- `plotReadsPerCell`.
- `plotUMIsPerCell`.
- `plotUMIsVsGenes`.

### Minor changes

- Migrated some `plotHeatmap` and `plotPCA` code from bcbio R packages.
- Added new `formalsList` global variable, which stashes `getOption` defaults
  used in some functions, namely the save/load functions and some plotting
  functions.
- Switched to using `formalsList` along with `formals` declaration internally
  to make the parameters more consistent across functions.
- Miscellaneous fixes to working examples and documentation.
- Split out sanitization functions into separate R files (e.g. `removeNA`).

## basejump 0.8.1 (2018-11-15)

### Major changes

The split-out sub-package approach isn't working quite right, so the code base
has been consolidated back into a single basejump package. Development toward
splitting the package will continue, but a conceptual re-imagining of how to
organize the functions is needed.

## basejump 0.8.0 (2018-11-11)

### Major changes

This release is the beginning of an attempt to rework the basejump codebase a
bit and make the package easier to unit test on [Travis CI][]. bcbio R packages
will be pinned to v0.7.2 during this development period.

Here we are working to split out the functionality of basejump into
several, smaller sub-packages:

- basejump.annotations
- basejump.assertions
- basejump.classes
- basejump.coercion
- basejump.developer
- basejump.experiment
- basejump.generics
- basejump.globals
- basejump.io
- basejump.markdown
- basejump.plots
- basejump.sanitization

## basejump 0.7.2 (2018-08-29)

### New functions

- `assertAllAreURL`.
- `assertAllAreValidNames`.
- `validNames`.

### Major changes

- Added a draft vignette explaining the functions available in the package.
- `gene2symbol`, `makeGene2symbolFromEnsembl`, and
  `makeGene2symbolFromGFF` functions now support the `unique` argument, which
  returns sanitized values in the `geneName` column, ensuring there are no
  duplicates. This is enabled by default (recommended) but can be disabled using
  `unique = FALSE`. This functionality was added to ensure consistent gene name
  handling in single-cell RNA-seq analyses.
- `saveData` now supports `basejump.save.ext` and `basejump.save.compress`
  global options, so the desired file type (e.g. RDS instead of RDA) and
  compression (e.g. xz instead of gzip) can be easily specified for an entire
  project.

### Minor changes

- `sampleNames` now supports assignment for `SummarizedExperiment` method.
- Now exporting `lanePattern` regular expression pattern as a global, which was
  previously defined in the [bcbioBase][] package.
- Bug fix for `as` coercion method support. Need to ensure
  `exportMethods(coerce)` is included in `NAMESPACE` file, otherwise
  `tibble` coercion using `as(x, "tbl_df")` won't work when called from an
  Rscript without the package library loaded. Thanks @roryk for noticing this.
- Updated `gene2symbol` generic to use `...`, since we've added the
  `unique = TRUE` argument in this release.
- `annotable`: Moved to `makeGRanges.R` file, and improved the internal code
  to export supported formals also used in `makeGRangesFromEnsembl`. The
  function should work exactly the same as previous releases, but now with
  clearer supported arguments in the documentation.
- Skipping code coverage for `cleanSystemLibrary`, since [Travis CI][]
  installs packages into the system library, and causes this check to return
  `FALSE`.
- Consistently using `as(from, "tbl_df")` for internal tibble coercion in all
  functions.
- `geneSynonyms` and `panther`: `organism` argument matching no longer suggests
  a default.
  The current list of supported organisms is in the documentation, and described
  in the internal `match.arg` call.
- All `SummarizedExperiment` methods use `validObject` validity checks, where
  applicable.
- Consolidated documentation for all `makeGRanges`, `makeGene2symbol`, and
  `makeTx2gene` functions.
- Heatmap functions: simplified the internal code responsible for defining
  `annotationCol` and `annotationColors` automatically.
- `sampleData`: Made validity check stricter, requiring `sampleName` column
  to be defined, otherwise the function will intentionally error.
- Now using `formals` internall to keep ggplot2 theme formals consistent.
- Updated example data scripts and resaved internal data.
- Updated contribution guidelines in `CONTRIBUTING.md` file.

## basejump 0.7.1 (2018-08-18)

### New functions

- `cleanSystemLibrary`: Utility function to check whether a user has installed
  packages into the [R][] system library. Refer to `.libPaths` documentation
  for more information on library paths.

### Major changes

- Now using `build` instead of `genomeBuild` for [Ensembl][] annotation
  functions. The `genomeBuild` argument still works but now will inform the user
  about the change.

### Minor changes

- Migrated `prepareTemplate` from [bcbioBase][] package. Simplifed this
  function to copy all files inside `extdata/rmarkdown/shared` within a
  specified package. Currently in use for [bcbioRNASeq][], [bcbioSingleCell][],
  and the new [pointillism][] clustering package.
- Made [Ensembl][] release matching stricter, based on the metadata columns.

## basejump 0.7.0 (2018-08-07)

In this release, we are migrating some of the S4 generics previously exported
in the [bcbioBase][] package. We are consolidating these functions here for
simplicity and stability.

### New functions

- `makeSummarizedExperiment`: Renamed `prepareSummarizedExperiment`,
  previously exported in the [bcbioBase][] package. We are using the `make`
  prefix here for consistency (see other gene annotation functions).
- Migrated S4 generics and methods from [bcbioBase][]: `flatFiles`,
  `metrics`, `plotCorrleationHeatmap`, `plotGene`, `plotHeatmap`,
  `plotQC`, and `plotQuantileHeatmap`.

### Major changes

- Now using `curl::has_internet` internally to check for Internet connection.
  This applies to the annotation functions that query web databases.
- Added coercion method support for converting a `SummarizedExperiment` to
  an unstructured `list`. This is the method used internally for `flatFiles`.

## basejump 0.6.4 (2018-08-03)

### New functions

- `matchInterestingGroups`: New developer function to automatically handle
  `interestingGroups` argument used across various plotting functions and in the
  [bcbio][] infrastructure packges.

## basejump 0.6.3 (2018-08-01)

### Minor changes

- Migrated `separatorBar` and `updateMessage` global export from [bcbioBase][].
  Improved `separatorBar` appeareance to automatically scale to current session
  width, using `getOption("width")`.

## basejump 0.6.2 (2018-07-31)

### New functions

- `convertSymbolsToGenes`: provides `SummarizedExperiment` method support for
  converting objects containing gene symbols ("geneName") as rownames back to
  gene identifiers ("geneID").
- `eggnog`: quickly download current annotations from [EggNOG][] database.
  Useful for annotating gene-to-protein matches; currently in use with the
  [brightworm][] RNAi screening package, which contains [WormBase][] gene ID and
  [EggNOG][] ID annotations.

### Major changes

- Now suggesting [BiocManager][] instead of [BiocInstaller][] for installation.
- `broadClass` now supports `GRanges` and `SummarizedExperiment`. Support for
  `data.frame` and/or `DataFrame` class objects has been removed.

### Minor changes

- `convertGenesToSymbols` and `convertTranscriptsToGenes` now have
  `organism` and `gene2symbol` arguments set `NULL` by default.
- Upgraded to [roxygen2][] v6.1 for documentation, which improves handling of
  aliases in the Rd manual files.
- Added `dplyr::pull` to reexported functions.
- Improved package documentation by declaring the supported class(es) for
  each function argument.
- Moved `foldChangeToLogRatio` and `logRatioToFoldChange` constructors into
  `numeric` method declarations.
- Simplified internal code for `gene2symbol` `SummarizedExperiment` method.
- `toStringUnique` now uses `x` instead of `atomic` as primary argument.

## basejump 0.6.1 (2018-07-21)

### Minor changes

- Bug fix for `convertGenesToSymbols` method for `SummarizedExperiment`.
  Previously, if `geneName` column was a factor, this function would error.
  This issue has been fixed by ensuring that the symbols provided in `geneName`
  are coerced to a character vector.
- Improved [conda][] installation instructions.

## basejump 0.6.0 (2018-07-17)

### Major changes

- Now importing [SummarizedExperiment][] package and providing basic method
  support for generics that were previously used in the [bcbioBase][] package.
- Improved GFFv3 handling in `makeGRangesFromGFF` and other GFF utility
  functions, including `makeGene2symbolFromGFF` and `makeTx2geneFromGFF`.
  Note that `makeGRangesFromGFF` now returns additional metadata columns
  accessible with `S4Vectors::mcols`, and that these columns are now sorted
  alphabetically.

### Migrated functions

Previously, these functions were exported in the [bcbioBase][] package, but
they provide non-[bcbio][]-specific functionality, and should be included here
in the [basejump][] package instead:

- `assertFormalInterestingGroups`.
- `gene2symbol`.
- `interestingGroups`, `uniteInterestingGroups`.
- `sampleData`, `sanitizeSampleData`.
- `sampleNames`.
- `selectSamples`.

Providing basic `SummarizedExperiment` class method support for `counts`.

### Minor changes

- `geometricMean` generic was not exported correctly.

## basejump 0.5.11 (2018-07-09)

### Minor changes

- Now requiring [ggplot2][] v3.0 internally.
- `theme_midnight` and `theme_paperwhite` now extend
  `ggplot2::theme_linedraw`, improving the consistency between these themes.
- Example data is now consistenly formatted using snake case: `rnaseq_counts`
  and `single_cell_counts`, instead of the previous camel case conventions:
  `rnaseqCounts`, `singleCellCounts`.
- Camel variants of the ggplot themes are now deprecated.
- Updated internal gene synonyms data from [NIH][].

## basejump 0.5.10 (2018-06-28)

### Minor changes

- Removed `makeNames` argument from `readFileByExtension` function. Use the
  `makeNames` family of functions manually after data import instead. This
  helps avoid unwanted sanitization of data.
- Simplified assert checks for internal load function.
- Improved code coverage.
- [AppVeyor CI][] updates to work with [Bioconductor][] 3.8 devel.

## basejump 0.5.9 (2018-06-12)

### Minor changes

- Markdown function consistency improvements. Now all relevant [Markdown][]
  functions use `text` as the primary argument, instead of `object`.

## basejump 0.5.8 (2018-06-05)

### Minor changes

- `makeGRangesFromEnsembl` now supports remapping of [UCSC][] genome build to
  Ensembl. However, this isn't recommended, and will warn the user.
- `convertUCSCBuildToEnsembl` now returns `NULL` instead of erroring on
  genome build match failure.
- `stripTranscriptVersions` now matches "`.`", "`-`", and "`_`" version
  delimiters.
- Made unused `dynamicPlotlist` function defunct.
- Consider deprecating `assertIsCharacterOrNULL` and
  `assertIsDataFrameOrNULL` in a future release.

### Infrastructure changes

- Reenable [Travis CI][] blocklist, excluding develop branch from checks.
- Reorganized documentation of deprecated and defunct functions.

## basejump 0.5.7 (2018-05-24)

### Minor changes

- Tweaked gray color accents for `theme_midnight` and `theme_paperwhite`.

## basejump 0.5.6 (2018-05-19)

### Minor changes

- Fixed NAMESPACE issue with `GenomeInfoDb::seqnames`.
- Improved `readGFF` working example to reflect switch to `GRanges` return.
- Added [macOS][] bioc-release image to [Travis CI][] build checks.

## basejump 0.5.5 (2018-05-15)

### Major changes

- `readGFF` now uses `rtracklayer::import` internally to return GFF file
  as a `GRanges` object instead of a `data.frame`.

### Minor changes

- `assertIsGFF` and `parseGFFAttributes` functions are now defunct.
- Simplified internal GFF handling code for `makeGRangesFromGFF`,
  `makeGene2symbolFromGFF`, and `makeTx2geneFromGFF`.

## basejump 0.5.4 (2018-05-08)

- Migrated `sanitizeSampleData` to [bcbioBase][] package.
- Updated [Bioconductor][] install method for 3.7.

## basejump 0.5.3 (2018-04-30)

### Minor changes

- Improved internal S4 method code for `fixNA` and `removeNA`.
- Tweaked gray accent colors in `theme_midnight` and `theme_paperwhite`.
  Now using British spelling internally for ggplot code.
- Improved `strip.background` for `theme_paperwhite`, removing the black box
  around the labels when facet wrapping is enabled.

## basejump 0.5.2 (2018-04-26)

### Minor changes

- Improved documentation for assert check functions.
- Deprecated `geomean` in favor of `geometricMean`.
- Simplified internal code for `grepString`.
- Added message during `hgnc2gene` call.
- Miscellaneous documentation fixes.
- Moved internal constructors into the S4 method definitions, where applicable.
- Simplified default parameter definition for `panther(organism = "XXX")`.
- Improved code coverage, using `nocov` where appropriate.

## basejump 0.5.1 (2018-04-16)

### Minor changes

- `emptyRanges`: Now using `match.arg` internally to capture `seqname`
  argument.
- Removed legacy `.assignCamelArgs` and `.assignCamelFormals` internal
  functions.
- Improved internal handling of XLSX files in `localOrRemoteFile`.

## basejump 0.5.0 (2018-04-13)

### New functions

- `emptyRanges` enables easy creation of placeholder ranges for `GRanges`
  objects, where transgene and FASTA spike-ins are needed.
- `hgnc2gene` enables easy mapping of [HGNC][] to [Ensembl][] gene
  identifiers.
- `mgi2gene` enables easy mapping of [MGI][] to [Ensembl][] gene identifiers.
- `panther` function enables easy querying of the [PANTHER][] website.
  Human, mouse, nematode worm, and fruit fly are currently supported. The
  specific [PANTHER][] release (e.g. 13) can be declared using the `release`
  argument. Otherwise, the function will return the most recent annotations
  from the [PANTHER][] website.
- Added `isURL` check function.
- `readJSON` adds support for JSON files. Like the other read functions, it
  supports both local files and remote URLs.

### ggplot2 themes

- `theme_midnight` and `theme_paperwhite` provide minimal, high contrast
  [ggplot2][] themes with bold sans serif labels.

### Major changes

- `loadData` now supports `.rda`, `.rds`, and `.RData` files. The function
  will error by design if multiple data extensions are detected inside the
  directory specified with the `dir` argument.

### Minor changes

- Consolidated assert check function code.
- Moved assertive imports to `basejump-package.R` file.
- Consolidated globals inside package to `globals.R` file.
- Removed internal `.biocLite` function. Now using `requireNamespace`
  instead, without attempting to install automatically.
- Added internal support for safe loading RDS files.
- Switched back to using `message`, `warning`, and `stop` instead of
  the [rlang][] equivalents.
- Improved internal method declaration using `getMethod` where applicable.
- `multiassignAsEnvir` is now recommended in place of
  `multiassignAsNewEnvir`.
- `readFileByExtension` will now attempt to use the [rio][] package for
  file extensions that are not natively supported.
- `writeCounts` now uses `mapply` internally.
- Migrated `assertFormalAnnotationCol` to [bcbioBase][] package.

## basejump 0.4.0 (2018-03-22)

### Major changes

- Introducing new functions for the acquistion of gene and transcript
  annotations from [Ensembl][]: `ensembl`, `genes`, and `transcripts`.
  These functions allow the return of `GRanges`, `DataFrame`, and `data.frame`
  class objects from [AnnotationHub][] using [ensembldb][].
- Improved internal `broadClass` definition code to match against chromosome
  from [Ensembl][] if available.
- `loadDataAsName` now works with unquoted names, improving consistency
  with `loadData` (non-standard evaluation).

### Minor changes

- Added new `convertUCSCBuildToEnsembl` function, for easy remapping of
  [UCSC][] to [Ensembl][] genome build names (e.g. `hg38` to `GRCh38`).
- Migrated matrix methods for `plotCorrelationHeatmap` here from
  [bcbioRNASeq][], for improved consistency with other heatmap functions.
- Exporting `makeNames` variant of `base::make.names` that sanitizes
  using underscores rather than dots.
- Converted `readYAML` from a generic to standard function.
- Added support for [AppVeyor CI][] code testing on [Windows][].
- Made [Travis CI][] build checks stricter, adding support for `BiocCheck`.
- Added new assert checks: `assertAreGeneAnnotations`,
  `assertAreTranscriptAnnotations`, `isAnImplicitInteger`.
- Simplified working examples for assert checks to just show successes.

### Deprecations

- `annotable` function has been deprecated in favor of the new `ensembl`
  function.
- `checkAnnotable` deprecated in favor of `assertIsAnnotable`.
- `checkGene2symbol` deprecated in favor of `assertIsGene2symbol`.
- `checkTx2gene` deprecated in favor of `assertIsTx2gene`.
- `assertFormalColorFunction` deprecated in favor of
  `assertIsHexColorFunctionOrNULL`.
- `initializeDir` deprecated in favor of `initializeDirectory`.
- Defunct: `summarizeRows`, `wash`, `packageSE`, `prepareSE`,
  `metadataTable`, `comp`, `revcomp`, `symbol2gene`.

## basejump 0.3.1 (2018-02-19)

- Now exporting all assert checks in camel case instead of snake case, to
  match consistency in the rest of the package.
- Added `sanitizeColData` function.
- Added `assertAllAreNonExisting` function.
- Now exporting `midnightTheme` as a `theme_midnight` alias to match the
  syntax in the [ggplot2][] package.
- Added working examples and code coverage for all assert check functions.
- Simplified the internal collapse code for `annotable` to simply work on
  the [Entrez][] identifier column (`entrez`). If a manually passed in data
  frame still has duplicates, the function will now abort instead of attempting
  to use `collapseToString`.
- Added [ggplot2][] color palette assert checks:
  `assertColorScaleContinuousOrNULL`, `assertColorScaleDiscreteOrNULL`,
  `assertFillScaleContinuousOrNULL`, `assertFillScaleDiscreteOrNULL`.

## basejump 0.3.0 (2018-02-16)

- Switch to using [assertive][] internally for assert checks.
- Now exporting these assert check functions:
  `assert_formal_annotation_col`, `assert_formal_color_function`,
  `assert_formal_compress`, `assert_formal_gene2symbol`,
  `assert_formal_header_level`, `assert_has_rownames`,
  `assert_is_a_number_or_null`, `assert_is_a_string_or_null`,
  `assert_is_an_implicit_integer`, `assert_is_an_implicit_integer_or_null`
  `assert_is_an_integer_or_null`, `assert_is_annotable`,
  `assert_is_character_or_null`, `assert_is_data.frame_or_null`,
  `assert_is_gene2symbol`, `assert_is_implicit_integer`,
  `assert_is_implicit_integer_or_null`, `assert_is_tx2gene`,
  `has_rownames`, `initializeDirectory`, `is_implicit_integer`.
- Renamed `md*` functions to `markdown*`.

## basejump 0.2.1 (2018-02-07)

- Added `convertGenesToSymbols` and `convertTranscriptsToGenes`
  functions. Previously some of this functionality was contained within the
  `gene2symbol` and `tx2gene` generics for the character method. This
  behavior was inconsistent with `gene2symbol` and `tx2gene` usage in the
  [bcbio][] R packages, so I decided to split these out into separate functions.
  Now `gene2symbol` and `tx2gene` work consistently with the `annotable`
  function to return gene-to-symbol and transcript-to-gene identifier mappings
  in a `data.frame`.
- `markdownHeader`, `markdownList`, and `markdownPlotlist` are now
  exported as S4 generics. The `md*` function variants are now exported as
  aliases.
- `geomean` has been renamed to `geometricMean`.
- Miscellaneous improvements to error messages and warnings.

## basejump 0.2.0 (2018-01-28)

- Offloaded internal [bcbio][]-specific code to new package named [bcbioBase][].
  Consequently, this makes the [basejump][] package leaner, meaner, and easier
  to manage. The following functions are now exported in that package:
  `bcbio`, `checkInterestingGroups`, `flatFiles`, `interestingGroups`,
  `metrics`, `plotDot`, `plotGene`, `plotQC`, `plotViolin`,
  `prepareSummarizedExperiment`, `prepareTemplate`, `readDataVersions`,
  `readLogFile`, `readProgramVersions`, `readSampleMetadataFile`,
  `sampleMetadata`, `sampleYAML`, `sampleYAMLMetadata`,
  `sampleYAMLMetrics`, and `selectSamples`. These functions are now
  deprecated here in [basejump][] (see `deprecated.R` file for more
  information).
- `comp` and `revcomp` have been deprecated in favor of `complement`
  and `reverseComplement` from the [Biostrings][] package.
- Internally, errors, messages, and warnings now use methods from the
  [rlang][] package: `abort`, `inform`, and `warn`, in place of `stop`,
  `message`, and `warning`, respectively.
- Improved error handing for missing files in `loadData`. Additionally, the
  file name must match the internal name in the RData file, otherwise
  `loadData` will warn the user. This is more strict than the default
  behavior of `base::load`, but helps prevent accidental overwrite in the
  current working environment.
- `localOrRemoteFile`, previously an internal function, is now exported.
- `annotable` now uses internal [GRCh37][] annotations from the [annotables][]
  package, which is saved in the `extdata/` directory internally. Previously,
  these genome annotations were accessed from lazy loaded data saved in the
  `data/` directory of the package repository.
- `annotables` now checks for all packages attached by [ensembldb][] and
  [AnnotationHub][] and forces detachment at the end of the function call.
  Otherwise, this can result in the unwanted effect of [ensembldb][] masking
  other user-loaded functions, such as the [tidyverse][] suite (e.g.
  `dplyr::select`).
- Consistently reformatted code indents to be a multple of 4 spaces, as
  recommended by [Bioconductor][].
- `camel` now handles delimited numbers a little differently. Previously,
  delimiters in between numbers (e.g. the commas in "1,000,000") were stripped.
  Sometimes this can result in confusing names. For example, if we have a
  column formatted in dotted case containing a decimal (e.g. "resolution.1.6"),
  the decimal would be stripped (e.g. "resolution16" in camel). Now, we
  sanitize a numeric delimiter as a lower case "x" character (e.g.
  "resolution1x6"). This ensures that numbers containing decimals remain
  semantically meaningful when sanitized with `camel`.
- Internally, `gsub` (and `grepl`) calls have been simplified to use the
  default order of "pattern, replacement, x".
- Internally, all implicit integers have been converted to explicit integers,
  where applicable.

## basejump 0.1.10 (2017-12-20)

- Bug fix for multiplexed sample input into `readSampleMetadataFile`. We
  were detecting the presence of `index` column but should instead check
  against `sequence` column.
- Added `dynamicPlotlist` and `mdPlotlist` plotting utilities.
- Added `uniqueSymbols` parameter to `annotable` function.

## basejump 0.1.9 (2017-12-18)

- Added `plotHeatmap` functionality.
- Migrated `tpm` generic from [bcbioRNASeq][], for future use in
  [bcbioSingleCell][].
- Added matrix method support for `plotHeatmap`.
- Added matrix method support for `plotQuantileHeatmap`, which works
  similarly as `plotHeatmap`.

## basejump 0.1.8 (2017-12-14)

- Improved `matrix` and `dgCMatrix` method support in `aggregateReplicates`
  and `aggregateFeatures` functions. Both of these functions now use a
  consistent `groupings` parameter, which uses a named factor to define the
  mappings of either samples (columns) for `aggregateReplicates` or
  genes/transcripts (rows) for `aggregateFeatures`.
- Update for `makeNames` sanitization functions. Now they will work on
  `names(x)` for vectors by default.
- Improved `detectOrganism` to match against "H. sapiens", etc.
- Added internal [GRCh37][] transcript to gene mapping.
- Improved organism matching to detect "Homo_sapiens" and "H. sapiens".
- Factors are now supported in the `makeNames` utilities: `camel`,
  `dotted`, `snake`, and `upperCamel`.
- Improved handling of `NA` values from [LibreOffice][] and
  [Microsoft Excel][Excel] output in `readFileByExtension`. This function now
  sets `""`, `NA`, and `#N/A` strings as `NA` correctly.

## basejump 0.1.7 (2017-12-11)

- Renamed `fc2lr` to `foldChangeToLogRatio` and `lr2fc` and
  `logRatioToFoldChange`.
- Moved `plotDot` and `plotViolin` generics here from [bcbioSingleCell][].
- Added internal [GRCh37][] gene annotations.

## basejump 0.1.6 (2017-12-06)

- Moved `microplate` code from the [wormbase][r-wormbase] package here, since
  it's of general interest.

## basejump 0.1.5 (2017-12-05)

- Added `checkAnnotable`, `checkGene2symbol`, `checkTx2gene`, and
  `sanitizeAnnotable` utility functions that will be used in the bcbio R
  packages.

## basejump 0.1.4 (2017-12-04)

- Added `midnightTheme` [ggplot2][] theme. Originally this was defined as
  `darkTheme` in the [bcbioSingleCell][] package, but can be useful for other
  plots and has been moved here for general bioinformatics usage. The theme now
  uses `ggplot2::theme_minimal` as the base, with some color tweaks, namely
  dark gray axes without white axis lines.
- Improve NAMESPACE imports to include `stats::formula` and
  `utils::capture.output`.

## basejump 0.1.3 (2017-12-01)

- `loadData` and `loadDataAsName` now default to `replace = TRUE`. If an
  object with the same name exists in the destination environment, then a
  warning is generated.
- `collapseToString` only attempts to dynamically return the original
  object class on objects that aren't class `data.frame`. I updated this code
  to behave more nicely with grouped tibbles (`grouped_df`), which are a
  virtual class of `data.frame` and therefore can't be coerced using
  `as(object, "grouped_df")`.
- DNA sequence utility functions `comp` and `revcomp` now return `NULL`
  for integers and numerics.
- For `prepareSummarizedExperiment`, added support for dropping `NULL`
  objects in assays list. This is useful for handling output from
  [bcbioRNASeq][] when `transformLimit` is reached. In this case, the `rlog` and
  `vst` matrices aren't generated and set `NULL` in the assays list. Using
  `Filter(Negate(is.null), assays)` we can drop these `NULL` objects and
  prevent a downstream dimension mismatch in the
  `SummarizedExperiment::SummarizedExperiment` call.
- Improved support for multiplexed files in `readSampleMetadataFile`. This
  now checks for a sequence column containing ACGT nucleotides. When those are
  detected, the `revcomp` column is generated. Otherwise this step is skipped.
  This is useful for handling multiplexed sample metadata from 10X Genomics
  [Cell Ranger][] single-cell RNA-seq samples.
- Updated `annotable` function to include nested [Entrez][] identifiers in the
  `entrez` column. This is useful for downstream functional analysis.

## basejump 0.1.2 (2017-11-30)

- Added [bcbio][] `plotQC` generic.
- Added back `toStringUnique` code, which is still in use in the
  [wormbase][r-wormbase] package.
- Added deprecations for `summarizeRows` (now `collapseToString`) and
  `wash` functions.
- Updated installation method to include manual installation of [ensembldb][].
  Otherwise, [basejump][] installation fails due to [GenomeInfoDb][] and
  [GenomeInfoDbData][] not getting installed completely.
- Now suggesting that the user installs suggested packages in the README.
- Updated [PANTHER][] annotation scripts.
- Bug fix for `detectOrganism`. Now allowing `NULL` return for unsupported
  organism, with a warning.

## basejump 0.1.1 (2017-11-11)

- Added overwrite support for `saveData`. Now will skip on existing files
  when `overwrite = FALSE`.
- Bug fix for `readDataVersions`, which shouldn't have the column types
  defined, using `col_types = "ccT"`.
- Improved key value pair method for `loadDataAsName`. Now rather than
  using a named character vector for the `mappings` argument, the user can
  simply pass the key value pairs in as dots. For example, `newName1 =
  "oldName1", newName2 = "oldName2"`. The legacy `mappings` method will still
  work, as long as the dots argument is a length of 1.
- [Ensembl][] release version now defaults to `NULL` instead of `current` for
  `annotable`, `gene2symbol`, `symbol2gene` and `tx2gene` functions.
- Allow `rowData` to be left unset in `prepareSummarizedExperiment`. This is
  useful for setting up objects that don't contain gene annotations.
- Removed sample selection by pattern matching (`pattern`, `patternCol`
  arguments) in `readSampleMetadata`. This feature wasn't fully baked and
  doesn't offer enough functionality to the user.

## basejump 0.1.0 (2017-10-23)

- Bump version to match [bcbioRNASeq][] package.
- Improved unit testing coverage of `prepareSummarizedExperiment`.
- Added quiet mode support to functions that output messages, where applicable.
- Consolidated [roxygen2][] function imports to `basejump-package.R` file.
- Deprecated `sampleDirs` generic.
- Improved organism detection in `detectOrganism` and added support for
  chicken genome.
- Clarified warning messages in `prepareSummarizedExperiment` to make
  sample loading with `loadRNASeq` and `loadSingleCell` in the [bcbio][]
  packages less confusing.
- Improved `NULL` returns in `readDataVersions`, `readLogFile`, and
  `readProgramVersions` utility functions.
- Fixed export of `*GTF` alias functions to simply wrap the `*GFF`
  functions with S4 methods support.
- Improved lane split technical replicate handling in
  `readSampleMetadataFile`.
- Improved `camel` syntax for both lax and strict modes. Added
  `upperCamel` function.
- Switched `str_` to base `grep` and `gsub` in internal functions.

## basejump 0.0.25 (2017-10-13)

- Improved consistency of `setMethod` calls using `signature`.
- Converted `loadRemoteData` to a standard function instead of using S4
  dispatch, allowing the `envir` argument to be set properly.

## basejump 0.0.24 (2017-10-10)

- Added additional package version requirements in `DESCRIPTION` file.
- Implicit integers are allowed.
- Using [GitHub][] version of [covr][] package for unit tests.
- Renamed `multiassignAsNewEnv` to `multiassignAsNewEnvir`
- Added `*GFF` function variants for `gene2symbolFromGTF` and
  `tx2geneFromGTF`.
- Added shared [bcbio][] generic functions: `aggregateReplicates`, `bcbio`,
  `bcbio<-`, `interestingGroups`, `metrics`, `plotGene`,
  `sampleDirs`, `sampleMetadata`, `selectSamples`. These functions are
  saved in `bcbioGenerics.R` file.
- Incorporated [bcbio][] utility functions shared across [bcbioRNASeq][] and
  [bcbioSingleCell][]: `readDataVersions` (deprecated `.dataVersions`),
  `readLogFile` (deprecated `.logFile`), `readProgramVersions`
  (deprecated `.programs`), `sampleYAML` (deprecated `.sampleYAML`),
  `sampleYAMLMetadata` (deprecated `sampleYAMLMetadata`),
  `sampleYAMLMetrics` (deprecated `.sampleYAMLMetrics`).
- Deprecated `metadataTable` function.
- Moved all [roxygen2][] documentation to `methods-*.R` files where applicable.
- Now using `assays` in `prepareSummarizedExperiment` generic definition as
  primary object.
- Improved `assignAndSaveData` to add silent return of file path.
- Now consistently using clear function definitions in chain operations with
  magrittr pipe (`%>%`).
- Added `.prepareSampleMetadata` utility function, for use with loading
  sample metadata from an external CSV, [Excel][], or YAML file.
- Added `loadData` functionality back to the package.
- Initial commit of `loadDataAsName` function.
- Improved `annotable` function documentation and support for [Ensembl][]
  release versions.
- Improved sanitization rules for `camel`, `dotted`, and `snake` name
  functions. Added the `strict` argument to `camel` and `dotted`.
- Improved the documentation for the `makeNames` functions, by splitting each
  into their own separate methods file.
- Improved S4 method support for the logRatio functions.
- Added integer support for `geomean` function. Also improved internal code
  of `geomean` based on Paul McMurdie's Stack Overflow post. See function
  documentation for more information.
- Added release support for `tx2gene` functions.
- Reduced the number of reexported functions (for documentation) down to the
  [magrittr][] pipe (`%>%`), `Matrix`, `DataFrame`, and `tibble`.
- Added silent return of file path to `saveData` function.
- Improved documentation for [tibble][] coercion using `as(object, "tibble")`
- Renamed `collapse` function to `collapseToString`, to avoid NAMESPACE
  collisions with tidyverse packages ([dplyr][], [glue][]).

## basejump 0.0.23 (2017-09-14)

- Upgraded `annotable` function to query [Ensembl][] using the [ensembldb][]
  package rather than [annotables][].

## basejump 0.0.22 (2017-09-08)

- Improved unit testing coverage.
- Renamed `prepareSE` to `prepareSummarizedExperiment`. Improved row and
  column name handling in the function. It now outputs more helpful diagnostic
  messages on error.
- Reworked and simplified `detectHPC` function to allow for unit testing.

## basejump 0.0.21 (2017-09-01)

- NAMESPACE improvements. Reduced the number of re-exported functions to
  simplify the package.
- Improved code coverage and unit testing with additional [testthat][]
  checks. Specifically, added unit testing for remote download functions and
  improved testing for GTF file utilities.
- Code coverage now above 90%!
- Renamed `packageSE` to `prepareSE` for better semantic meaning.
- Made multiple generics more flexible by inclusion of passthrough (`…`).
- Reduced the number of deprecated functions.
- Initial commit of internal `localOrRemote` utility function.
- Initial commit of `prepareTemplate` function.
- Added additional `data-raw/` scripts.
- Added `onLoad.R` script back to ensure proper attachment of [annotables][]
  data package.
- Removed [tidyverse][] S4 method support.
- Improved remote file handling for `readFileByExtension`, `readGTF`, and
  `readYAML` functions.

## basejump 0.0.20 (2017-08-16)

- Offloaded [devtools][] functions to personal package.
- Upgraded all functions to S4 functions where possible.
- Assign utilities were kept as S3 functions since S4 dispatch makes
  `parent.frame` assignment not work correctly.
- Deprecated snake case and British spelling variants to reduce the number of
  exported functions.
- Added more working examples.
- Added unit testing for [annotables][] functions.

## basejump 0.0.19 (2017-07-24)

- Improved documentation and consistency with [bcbio][] packages.
- Improved integration of gene annotation calls using [annotables][] package.

## basejump 0.0.18 (2017-07-24)

- Initial support for [SummarizedExperiment][] creation with `packageSE`.

## basejump 0.0.17 (2017-07-11)

- Added [annotables][] common code from [bcbio][] packages.
- Added automatic file reading using [readr][] package.
- Combined write counts functions from [bcbioRNASeq][] and [bcbioSinglecell][].
- Initial commit of `assign_data` for use in [bcbioSingleCell][] sample loops.

## basejump 0.0.16 (2017-06-28)

- Minor NAMESPACE updates while working on [bcbio][] packages.
- Tweaks for [tidyverse][] S4 generic verbs. In particular, `as_tibble` now
  provides better consistency for rowname conversion.

## basejump 0.0.15 (2017-06-18)

- Added [testthat][] support for [lintr][] checks.
- Added S4 generic for `as_tibble`.

## basejump 0.0.14 (2017-06-13)

- [dplyr][] 0.7 NAMESPACE fixes and function tweaks.

## basejump 0.0.13 (2017-06-12)

- `setMethod` for [tidyverse][] NAMESPACE collisons ([dplyr][], [tidyr][])
  using `signature("data.frame")`.

## basejump 0.0.12 (2017-05-25)

- Updated exports based on [wormbase][r-wormbase] package.

## basejump 0.0.11 (2017-05-18)

- Improved naming functions to dynamically handle character vectors and objects
  that support naming assignments.
- Added `removeNA` utility function.

## basejump 0.0.10 (2017-05-14)

- Added NAMESPACE utilities to deal with [tidyverse][] generic verbs.
- Switched package documentation method to use [roxygen2][] with pkgapi.

## basejump 0.0.9 (2017-05-13)

- Added snake case function variants.

## basejump 0.0.8 (2017-05-13)

- Added back `saveData` utility functions.

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

[acid genomics]: https://acidgenomics.com/
[acidbase]: https://acidbase.acidgenomics.com/
[acidplots]: https://acidplots.acidgenomics.com/
[annotables]: https://github.com/stephenturner/annotables/
[annotationhub]: https://bioconductor.org/packages/AnnotationHub/
[appveyor ci]: https://ci.appveyor.com/
[assertive]: https://cran.r-project.org/package=assertive
[basejump]: https://basejump.acidgenomics.com/
[bb8]: https://bb8.acidgenomics.com/
[bcbio]: https://github.com/bcbio/bcbio-nextgen/
[bcbiobase]: https://bioinformatics.sph.harvard.edu/bcbioBase/
[bcbiornaseq]: https://bioinformatics.sph.harvard.edu/bcbioRNASeq/
[bcbiosinglecell]: https://bioinformatics.sph.harvard.edu/bcbioSingleCell/
[biocinstaller]: https://bioconductor.org/packages/BiocInstaller/
[biocmanager]: https://cran.r-project.org/package=BiocManager
[bioconductor]: https://bioconductor.org/
[biostrings]: https://bioconductor.org/packages/Biostrings/
[bioverbs]: https://bioverbs.acidgenomics.com/
[brightworm]: https://brightworm.acidgenomics.com/
[brio]: https://brio.acidgenomics.com/
[cell ranger]: https://support.10xgenomics.com/single-cell-gene-expression/software/pipelines/latest/what-is-cell-ranger
[checkmate]: https://cran.r-project.org/package=checkmate
[conda]: https://conda.io/
[covr]: https://cran.r-project.org/package=covr
[cran]: https://cran.r-project.org/
[data.table]: https://cran.r-project.org/package=data.table
[devtools]: https://cran.r-project.org/package=devtools
[dplyr]: http://dplyr.tidyverse.org/
[eggnog]: http://eggnogdb.embl.de/
[ensembl]: https://www.ensembl.org/
[ensembldb]: https://bioconductor.org/packages/ensembldb/
[entrez]: https://www.ncbi.nlm.nih.gov/Web/Search/entrezfs.html
[excel]: https://products.office.com/en-us/excel/
[freerange]: https://freerange.acidgenomics.com/
[genomeinfodb]: https://bioconductor.org/packages/GenomeInfoDb/
[genomeinfodbdata]: https://bioconductor.org/packages/GenomeInfoDbData/
[ggplot2]: http://ggplot2.tidyverse.org/
[github]: https://github.com/
[glue]: https://glue.tidyverse.org/
[goalie]: https://goalie.acidgenomics.com/
[grch37]: https://grch37.ensembl.org/
[hgnc]: https://www.genenames.org/
[hms rc]: https://rc.hms.harvard.edu/
[koopa]: https://koopa.acidgenomics.com/
[libreoffice]: https://www.libreoffice.org/
[lintr]: https://github.com/jimhester/lintr/
[macos]: https://www.apple.com/macos/
[magrittr]: https://magrittr.tidyverse.org/
[markdown]: https://daringfireball.net/projects/markdown/
[mgi]: http://www.informatics.jax.org/
[nih]: https://www.nih.gov/
[panther]: http://pantherdb.org/
[pheatmap]: https://cran.r-project.org/package=pheatmap
[pointillism]: https://pointillism.acidgenomics.com/
[r-wormbase]: https://wormbase.acidgenomics.com/
[r]: https://www.r-project.org/
[readr]: http://readr.tidyverse.org/
[rio]: https://cran.r-project.org/package=rio
[rlang]: http://rlang.tidyverse.org/
[roxygen2]: https://cran.r-project.org/package=roxygen2
[s4vectors]: https://bioconductor.org/packages/S4Vectors/
[summarizedexperiment]: https://bioconductor.org/packages/SummarizedExperiment/
[syntactic]: https://syntactic.acidgenomics.com/
[testthat]: https://cran.r-project.org/package=testthat
[tibble]: https://tibble.tidyverse.org/
[tidyr]: http://tidyr.tidyverse.org/
[tidyverse]: http://www.tidyverse.org/
[transformer]: https://transformer.acidgenomics.com/
[travis ci]: https://travis-ci.com/
[ucsc]: https://genome.ucsc.edu/
[windows]: https://www.microsoft.com/en-us/windows/
[wormbase]: http://www.wormbase.org/
