#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param alpha `scalar numeric` or `NULL`. Adjusted P value ("alpha") cutoff.
#'   If left `NULL`, will use the cutoff defined in the object.
#' @param aspectRatio `scalar integer`. Aspect ratio.
#' @param assay `scalar`. Name or index of count matrix slotted in
#'   `SummarizedExperiment::assays()`. When passing in a `string`, the name must
#'   be defined in `SummarizedExperiment::assayNames()`.
#' @param bpparam `bpparamClass`. BiocParallel parameter specifying the back-end
#'   to be used for computations. See `BiocParallel::bpparam()` for details.
#'
#'   We recommend one of the following:
#'   - `BiocParallel::bpparam()`.
#'   - `BiocParallel::SerialParam()`.
#'   - `BiocParallel::MulticoreParam()`.
#' @param check `boolean`. Perform assert checks.
#' @param color `ggproto`/`ScaleDiscrete` or `NULL`. Desired ggplot2 color
#'   scale. Must supply discrete values. When set to `NULL`, the default ggplot2
#'   color palette will be used. If manual color definitions are desired, we
#'   recommend using `ggplot2::scale_color_manual()`. To set the discrete color
#'   palette globally, use
#'   `options(basejump.color.discrete = ggplot2::scale_color_viridis_d())`.
#' @param counts `matrix`. Count matrix. Normalized counts are recommended.
#' @param countsAxisLabel `string`. Counts axis label.
#' @param dark `boolean`. Plot against a dark background using
#'   `basejump::theme_midnight()` ggplot2 theme.
#' @param dimsUse `integer`. Vector of length 2 that denotes the columns from
#'   the reduced dimension matrix to use for `centerX` and `centerY` column
#'   calculations. Defaults the first and second dimensions.
#' @param dir `string`. Directory path.
#' @param direction `string`. Plot "`both`", "`up`", or "`down`" directions.
#' @param ensemblRelease `scalar integer`. Ensembl release version (e.g. `90`).
#' @param envir `environment` to use for assignment. Defaults to
#'   `parent.frame()`, which will assign into the calling environment.
#' @param expression `string`. Calculation to apply. Uses `base::match.arg()`
#'   and defaults to the first argument in the `character` vector.
#' @param file `string`. File path.
#' @param fill `ggproto`/`ScaleDiscrete` or `NULL`. Desired ggplot2 fill scale.
#'   Must supply discrete values. When set to `NULL`, the default ggplot2 color
#'   palette will be used. If manual color definitions are desired, we recommend
#'   using `ggplot2::scale_fill_manual()`. To set the discrete fill palette
#'   globally, use `options(basejump.fill.discrete = scale_fill_viridis_d())`.
#' @param flip `boolean`. Flip x and y axes. Recommended for plots containing
#'   many samples.
#' @param gene2symbol `Gene2Symbol`. Gene-to-symbol mappings. Must contain
#'   `geneID` and `geneName` columns. See `Gene2Symbol()` for more information.
#' @param genes `character`. Gene identifiers. It is considered better practice
#'   to input the stable gene identifiers from Ensembl (e.g. "ENSG00000000003")
#'   and not the (HGNC) gene symbols (e.g. "TSPN6"), if possible.
#' @param geom `string`. Plot type. Uses `base::match.arg()` and defaults to the
#'   first argument in the `character` vector.
#' @param headerLevel `scalar integer` (`1`-`7`). Markdown header level.
#' @param i Indices specifying elements to extract or replace. Indices are
#'   `numeric` or `character` vectors, empty (`missing`), or `NULL`. See
#'   `help(topic = "Extract", package = "base")` for more information.
#' @param inherits `boolean`. Should the enclosing frames of the `environment`
#'   be searched?
#' @param interestingGroups `character` or `NULL`. Groups of interest that
#'   define the samples. If left unset, defaults to `sampleName`.
#' @param label `boolean`. Superimpose sample text labels on the plot.
#' @param labelSize `scalar integer`. Size of the text label.
#' @param lfcThreshold `scalar numeric` or `NULL`. Log fold change ratio (base
#'   2) cutoff threshold. If left `NULL`, will use the cutoff defined in the
#'   object.
#' @param legend `boolean`. Show plot legend.
#' @param level `string`. Return ranges as "`genes`" or "`transcripts`".
#' @param limit `scalar numeric`. Threshold to denote on the plot, using a
#'   dashed line.
#' @param max `scalar numeric`. Recommended maximum value cutoff.
#' @param min `scalar numeric`. Recommended minimum value cutoff.
#' @param minCounts `scalar integer`. Minimum number of counts per gene in the
#'   count matrix.
#' @param n `scalar integer`. Number to include.
#' @param ntop `scalar integer`. Number of top genes to label.
#' @param object Object.
#' @param organism `string`. Full Latin organism name (e.g. "`Homo sapiens`").
#' @param perMillion `boolean`. Display as counts per million.
#' @param perSample `boolean`. Visualize the distributions per sample.
#' @param plotlist `list` containing `ggplot` objects.
#' @param pointAlpha `scalar numeric` (`0`-`1`). Alpha transparency level.
#'   Useful when there many cells in the dataset, and some cells can be masked.
#' @param pointsAsNumbers `boolean`. Plot the points as numbers (`TRUE`) or
#'   dots (`FALSE`).
#' @param pointSize `scalar numeric`. Cell point size.
#' @param prefilter `boolean`. Apply prefiltering to remove zero count genes.
#' @param progress `boolean`. Show progress, using progress bars.
#' @param reducedDim `string`. Name of reduced dimension matrix slotted in
#'   `SingleCellExperiment::reducedDims()`. Includes TNSE, UMAP, PCA, for
#'   example.
#' @param return `string`. Return type. Uses `base::match.arg()`
#'   internally and defaults to the first argument in the `character` vector.
#' @param rowRanges `GRanges`. Genomic ranges (e.g. genome annotations).
#' @param samples `character` or `NULL`. Samples to include.
#' @param sigPointColor `character`. Color names for labeling upregulated and
#'   downregulated genes. Also supports a character string for labeling DEGs
#'   with the same color, regardless of direction.
#' @param subtitle `string` or `NULL`. Plot subtitle.
#' @param title `string` or `NULL`. Plot title.
#' @param trans `string`. Name of the axis scale transformation to apply. See
#'   `help("scale_x_continuous", "ggplot2")` for more information.
#' @param trendline `boolean`. Include trendline on plot.
#' @param tx2gene `Tx2Gene`. Transcript-to-gene mappings.
#' @param url `string`. Uniform Resource Locator (URL). HTTP or FTP address.
#' @param value Value to assign.
#' @param verbose `boolean`. Run the function with verbose messages? It is only
#'   recommended to enable this when debugging.
#' @param x Object.
#' @param ... Additional arguments.
#'
#' @return No value.
NULL
