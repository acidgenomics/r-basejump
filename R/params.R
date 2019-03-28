#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param assay `vector(1)`.
#'   Name or index of count matrix slotted in
#'   [`assays()`][SummarizedExperiment::assays]. When passing in a string, the
#'   name must be defined in
#'   [`assayNames()`][SummarizedExperiment::assayNames].

#' @param colnames `logical(1)`.
#'   Apply to column names.
#' @param counts `matrix`.
#'   Count matrix. Normalized counts are recommended.
#' @param dir `character(1)`.
#'   Directory path.
#' @param ensemblRelease,release `integer(1)`.
#'   Ensembl release version (e.g. `90`). We recommend setting this value if
#'   possible, for improved reproducibility. When left unset, the latest release
#'   available via AnnotationHub/ensembldb is used. Note that the latest version
#'   available can vary, depending on the versions of AnnotationHub and
#'   ensembldb in use.
#' @param envir `environment`.
#'   Environment to use for assignment. Defaults to
#'   [`parent.frame()`][base::parent.frame], which will assign into the calling
#'   environment.
#' @param expression `character(1)`.
#'   Calculation to apply.
#'   Uses [`match.arg()`][base::match.arg] internally and defaults to the first
#'   argument in the `character` vector.
#' @param file `character(1)`.
#'   File path.
#' @param gene2symbol `Gene2Symbol`.
#'   Gene-to-symbol mappings. Must contain `geneID` and `geneName` columns. See
#'   `Gene2Symbol` for more information.
#' @param genes `character`.
#'   Gene identifiers. It is considered better practice to input the stable gene
#'   identifiers from Ensembl (e.g. "ENSG00000000003") and not the (HGNC) gene
#'   symbols (e.g. "TSPN6"), if possible.
#' @param genomeBuild `character(1)`.
#'   Ensembl genome build assembly name (e.g. `"GRCh38"`). If set `NULL`,
#'   defaults to the most recent build available. Note: don't pass in UCSC build
#'   IDs (e.g. `"hg38"`).
#' @param headerLevel `integer(1)` (`1`-`7`).
#'   Markdown header level.
#' @param i
#'   Indices specifying elements to extract or replace. Indices are `numeric` or
#'   `character` vectors, empty (`missing`), or `NULL`.
#'
#'   For more information:
#'
#'   ```
#'   help(topic = "Extract", package = "base")
#'   ```
#' @param inherits `logical(1)`.
#'   Should the enclosing frames of the `environment` be searched?
#' @param interestingGroups `character`.
#'   Groups of interest that define the samples. If left unset, defaults to
#'   `sampleName`.
#' @param level `character(1)`.
#'   Return as genes or transcripts.
#' @param n `integer(1)`.
#'   Number to include.
#' @param ntop `integer(1)`.
#'   Number of top genes to label.
#' @param object Object.
#' @param organism `character(1)`.
#'   Full Latin organism name (e.g. "`Homo sapiens`").
#' @param progress `logical(1)`.
#'   Show progress, using progress bars.
#' @param return `character(1)`.
#'   Return type. Uses [`match.arg()`][base::match.arg] internally and defaults
#'   to the first argument in the `character` vector.
#' @param rownames `logical(1)`.
#'   Apply to row names.
#' @param samples `character`.
#'   Specify a subset of samples to include.
#' @param sort `logical(1)`.
#'   Resort using `sort`.
#' @param tx2gene `Tx2Gene`.
#'   Transcript-to-gene mappings.
#' @param url `character(1)`.
#'   Uniform Resource Locator (URL). HTTP or FTP address.
#' @param value Value to assign.
#' @param x Object.
#' @param ... Additional arguments.
#'
#' @return No value.
NULL
