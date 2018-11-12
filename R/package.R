#' basejump
#'
#' Toolkit for bioinformatics and R package development.
#'
#' @keywords internal
#'
#' @importClassesFrom GenomicRanges GRanges GRangesList
#' @importClassesFrom IRanges SimpleDataFrameList
#' @importClassesFrom Matrix sparseMatrix
#' @importClassesFrom S4Vectors DataFrame List
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom SummarizedExperiment coerce
#'
#' @importFrom AnnotationHub AnnotationHub query snapshotDate
#' @importFrom Biobase sampleNames sampleNames<-
#' @importFrom BiocGenerics as.data.frame as.list colSums do.call lapply match
#'   rowMeans rowSums setdiff t
#' @importFrom GenomicFeatures genes transcripts
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom Matrix readMM readMM writeMM
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom R.utils gzip
#' @importFrom RColorBrewer brewer.pal
#' @importFrom RCurl getURL url.exists
#' @importFrom S4Vectors %in% DataFrame List Rle aggregate complete.cases cor
#'   expand head mcols mcols<- metadata metadata<- na.omit tail
#' @importFrom SingleCellExperiment SingleCellExperiment isSpike<-
#'   reducedDimNames reducedDims spikeNames
#' @importFrom SummarizedExperiment SummarizedExperiment assay assayNames assays
#'   assays<- colData colData<- rowData rowData<- rowRanges rowRanges<-
#' @importFrom assertive.base assert_all_are_not_na assert_all_are_true
#'   assert_any_are_true assert_are_identical assert_is_identical_to_na
#'   is_not_na
#' @importFrom assertive.code assert_all_are_existing is_existing
#' @importFrom assertive.data assert_all_are_hex_colors is_hex_color
#' @importFrom assertive.files assert_all_are_dirs assert_all_are_existing_files
#'   assert_all_are_non_empty_files
#' @importFrom assertive.numbers assert_all_are_greater_than
#'   assert_all_are_greater_than_or_equal_to assert_all_are_in_closed_range
#'   assert_all_are_in_open_range assert_all_are_in_range
#'   assert_all_are_non_negative assert_all_are_positive is_positive
#' @importFrom assertive.properties assert_are_same_length assert_has_colnames
#'   assert_has_cols assert_has_dimnames assert_has_dims assert_has_names
#'   assert_has_no_duplicates assert_has_rownames assert_has_rows
#'   assert_is_atomic assert_is_empty assert_is_non_empty assert_is_not_null
#'   assert_is_null assert_is_of_length assert_is_scalar assert_is_vector
#'   has_colnames has_dimnames has_dims has_names has_rownames has_rows
#'   is_scalar
#' @importFrom assertive.sets assert_are_disjoint_sets
#'   assert_are_intersecting_sets assert_are_set_equal assert_is_subset
#'   is_subset
#' @importFrom assertive.strings assert_all_are_matching_regex
#'   assert_all_are_non_empty_character
#'   assert_all_are_non_missing_nor_empty_character
#'   assert_any_are_matching_regex
#' @importFrom assertive.types assert_is_a_bool assert_is_a_number
#'   assert_is_a_string assert_is_all_of assert_is_an_integer assert_is_any_of
#'   assert_is_call assert_is_character assert_is_data.frame
#'   assert_is_environment assert_is_factor assert_is_function assert_is_integer
#'   assert_is_list assert_is_matrix assert_is_name assert_is_numeric
#'   assert_is_symbol assert_is_tbl_df is_a_number is_a_string
#' @importFrom assertthat assert_that validate_that
#' @importFrom cowplot plot_grid
#' @importFrom curl has_internet
#' @importFrom data.table as.data.table fread
#' @importFrom dplyr arrange bind_rows case_when desc everything filter funs
#'   group_by left_join mutate mutate_all mutate_at mutate_if n pull rename
#'   select select_if slice summarise summarise_all top_n ungroup
#' @importFrom ensembldb ensemblVersion organism
#' @importFrom ggplot2 aes coord_flip element_blank element_line element_rect
#'   element_text expand_limits facet_wrap geom_bar geom_boxplot geom_density
#'   geom_hline geom_jitter geom_label geom_point geom_violin geom_vline ggplot
#'   guides labs position_jitterdodge scale_x_continuous scale_y_continuous
#'   stat_ecdf stat_summary theme theme_linedraw
#' @importFrom ggrepel geom_label_repel
#' @importFrom grDevices colorRampPalette
#' @importFrom grid arrow unit
#' @importFrom jsonlite read_json
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom magrittr %>% set_colnames set_rownames
#' @importFrom methods as formalArgs getGeneric getMethod is isGeneric new
#'   selectMethod setAs setClass setGeneric setMethod setOldClass setValidity
#'   show signature slot slotNames validObject .hasSlot
#' @importFrom pbapply pblapply
#' @importFrom pheatmap pheatmap
#' @importFrom purrr map
#' @importFrom readr cols read_lines read_tsv write_csv write_lines
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#' @importFrom rlang !! !!! := dots_list eval_bare has_length sym syms UQ
#' @importFrom rtracklayer import
#' @importFrom sessioninfo session_info
#' @importFrom stats as.formula quantile
#' @importFrom stringr regex str_dup str_extract str_length str_match str_pad
#'   str_replace str_replace_all str_replace_na str_subset str_trunc
#' @importFrom tidyr gather separate unite
#' @importFrom tidyselect everything matches starts_with
#' @importFrom tibble as_tibble column_to_rownames tibble
#' @importFrom tools Rd_db
#' @importFrom utils capture.output data download.file getFromNamespace
#'   globalVariables installed.packages packageVersion read.delim read.table
#'   sessionInfo
#' @importFrom yaml yaml.load_file
"_PACKAGE"



# Conflicts with rtracklayer:
# @importFrom rio import

# Conflicts with BiocGenerics:
# @importMethodsFrom Matrix colSums rowMeans rowSums t



globalVariables(".")

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`



#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param alpha `scalar numeric` or `NULL`. Adjusted P value ("alpha") cutoff.
#'   If left `NULL`, will use the cutoff defined in the object.
#' @param assay `scalar`. Name or index of count matrix slotted in
#'   `SummarizedExperiment::assays()`. When passing in a `string`, the name must
#'   be defined in `SummarizedExperiment::assayNames()`.
#' @param check `boolean`. Perform assert checks.
#' @param color `ggproto`/`ScaleDiscrete` or `NULL`. Desired ggplot2 color
#'   scale. Must supply discrete values. When set to `NULL`, the default ggplot2
#'   color palette will be used. If manual color definitions are desired, we
#'   recommend using `ggplot2::scale_color_manual()`. To set the discrete color
#'   palette globally, use
#'   `options(basejump.discrete.color = ggplot2::scale_color_viridis_d())`.
#' @param counts `matrix`. Count matrix. Normalized counts are recommended.
#' @param countsAxisLabel `string`. Counts axis label.
#' @param dir `string`. Directory path.
#' @param direction `string`. Plot "`both`", "`up`", or "`down`" directions.
#' @param envir `environment` to use for assignment. Defaults to
#'   `parent.frame()`, which will assign into the calling environment.
#' @param file `string`. File path.
#' @param fill `ggproto`/`ScaleDiscrete` or `NULL`. Desired ggplot2 fill scale.
#'   Must supply discrete values. When set to `NULL`, the default ggplot2 color
#'   palette will be used. If manual color definitions are desired, we recommend
#'   using `ggplot2::scale_fill_manual()`. To set the discrete fill palette
#'   globally, use `options(bcbio.discrete.fill = scale_fill_viridis_d())`.
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
#' @param plotlist `list` containing `ggplot` objects.
#' @param pointColor `string`. Default point color for the plot.
#' @param prefilter `boolean`. Apply prefiltering to remove zero count genes.
#' @param return `string`. Return type. Uses `base::match.arg()`
#'   internally and defaults to the first argument in the `character` vector.
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
