#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param alpha `scalar numeric`. Adjusted P value ("alpha") cutoff.
#'   If left `NULL`, will use the cutoff defined in the object.
#' @param aspectRatio `scalar integer`. Aspect ratio.
#' @param assay `scalar`. Name or index of count matrix slotted in
#'   `SummarizedExperiment::assays()`. When passing in a `string`, the name must
#'   be defined in `SummarizedExperiment::assayNames()`.
#' @param assays `list`. Count matrices, which must have matching dimensions.
#'   Counts can be passed in as either a dense matrix (`matrix`) or sparse
#'   matrix (`sparseMatrix`).
#' @param bpparam `bpparamClass`. BiocParallel parameter specifying the back-end
#'   to be used for computations. See `BiocParallel::bpparam()` for details.
#'
#'   We recommend one of the following:
#'   - `BiocParallel::bpparam()`.
#'   - `BiocParallel::SerialParam()`.
#'   - `BiocParallel::MulticoreParam()`.
#' @param censorSamples `character`. Specify a subset of samples to censor.
#' @param check `boolean`. Perform assert checks.
#' @param colData `DataFrame`. Metadata describing the assay columns. For bulk
#'   RNA-seq, this data describes the samples. For single-cell RNA-seq, this
#'   data describes the cells.
#' @param color `ggproto`/`ScaleDiscrete`. Desired ggplot2 color
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
#'   We recommend setting this value if possible, for improved reproducibility.
#'   When left unset, the latest release available via AnnotationHub/ensembldb
#'   is used. Note that the latest version available can vary, depending on the
#'   versions of AnnotationHub and ensembldb in use.
#' @param envir `environment` to use for assignment. Defaults to
#'   `parent.frame()`, which will assign into the calling environment.
#' @param expression `string`. Calculation to apply. Uses `base::match.arg()`
#'   and defaults to the first argument in the `character` vector.
#' @param file `string`. File path.
#' @param fill `ggproto`/`ScaleDiscrete`. Desired ggplot2 fill scale. Must
#'   supply discrete values. When set to `NULL`, the default ggplot2 color
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
#' @param gffFile `string`. GFF/GTF (General Feature Format) file.
#'   Generally, we recommend using a GTF (GFFv2) instead of a GFFv3 file if
#'   possible.
#' @param headerLevel `scalar integer` (`1`-`7`). Markdown header level.
#' @param i Indices specifying elements to extract or replace. Indices are
#'   `numeric` or `character` vectors, empty (`missing`), or `NULL`. See
#'   `help(topic = "Extract", package = "base")` for more information.
#' @param inherits `boolean`. Should the enclosing frames of the `environment`
#'   be searched?
#' @param interestingGroups `character`. Groups of interest that
#'   define the samples. If left unset, defaults to `sampleName`.
#' @param label `boolean`. Superimpose sample text labels on the plot.
#' @param labelSize `scalar integer`. Size of the text label.
#' @param lfcThreshold `scalar numeric`. Log fold change ratio (base
#'   2) cutoff threshold. If left `NULL`, will use the cutoff defined in the
#'   object.
#' @param legend `boolean`. Show plot legend.
#' @param level `string`. Return ranges as "`genes`" or "`transcripts`".
#' @param limit `scalar numeric`. Threshold to denote on the plot, using a
#'   dashed line.
#' @param max `scalar numeric`. Recommended maximum value cutoff.
#' @param metadata `list`. Metadata.
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
#'   Useful when there many points in the dataset (e.g. single-cell data), and
#'   some points can be masked.
#' @param pointColor `string`. Default point color for the plot.
#' @param pointScalar `scalar integer`. Default point size for the plot.
#' @param pointsAsNumbers `boolean`. Plot the points as numbers (`TRUE`) or
#'   dots (`FALSE`).
#' @param pointSize `scalar numeric`. Point size for dots in the plot.
#' @param prefilter `boolean`. Apply prefiltering to remove zero count genes.
#' @param progress `boolean`. Show progress, using progress bars.
#' @param reducedDim `string`. Name of reduced dimension matrix slotted in
#'   `SingleCellExperiment::reducedDims()`. Includes TNSE, UMAP, PCA, for
#'   example.
#' @param return `string`. Return type. Uses `base::match.arg()`
#'   internally and defaults to the first argument in the `character` vector.
#' @param rowData `DataFrame`. *Use rowRanges (GRanges) instead, if possible*.
#'   Metadata describing the assay rows, if genomic ranges are not available.
#' @param rowRanges `GRanges`. Genomic ranges (e.g. genome annotations).
#'   Metadata describing the assay rows.
#' @param sampleMetadataFile `string`. Sample metadata file path. CSV
#'   or TSV is preferred, but Excel worksheets are also supported. Check the
#'   documentation for conventions and required columns.
#' @param samples `character`. Specify a subset of samples to include.
#' @param sigPointColor `character`. Color names for labeling upregulated and
#'   downregulated genes. Also supports a character string for labeling DEGs
#'   with the same color, regardless of direction.
#' @param spikeNames `character`. Vector indicating which assay rows denote
#'   spike-in sequences (e.g. ERCCs).
#' @param subtitle `string`. Plot subtitle.
#' @param title `string`. Plot title.
#' @param trans `string`. Name of the axis scale transformation to apply. See
#'   `help("scale_x_continuous", "ggplot2")` for more information.
#' @param transgeneNames `character`. Vector indicating which assay
#'   rows denote transgenes (e.g. EGFP, TDTOMATO).
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
