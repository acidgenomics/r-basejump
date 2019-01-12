# Conflicts with BiocGenerics:
# @importMethodsFrom Matrix colSums rowMeans rowSums t



#' @importClassesFrom GenomicRanges GRanges GRangesList
#' @importClassesFrom IRanges SimpleDataFrameList
#' @importClassesFrom Matrix sparseMatrix
#' @importClassesFrom S4Vectors DataFrame List
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importMethodsFrom S4Vectors as.data.frame as.list coerce do.call lapply
#'   match setdiff t
#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom SummarizedExperiment coerce
#' @importMethodsFrom transformer as.data.frame coerce
#'
#' @importFrom AnnotationHub AnnotationHub query snapshotDate
#' @importFrom BiocGenerics as.data.frame as.list colSums do.call lapply match
#'   rowSums setdiff t
#' @importFrom GenomeInfoDb seqnames
#' @importFrom GenomicFeatures genes transcripts
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom RColorBrewer brewer.pal
#' @importFrom S4Vectors DataFrame List Rle %in% aggregate cor decode expand
#'   head mcols mcols<- metadata metadata<- na.omit tail
#' @importFrom SingleCellExperiment SingleCellExperiment isSpike<- spikeNames
#' @importFrom SummarizedExperiment SummarizedExperiment assay assayNames
#'   assayNames<- assays assays<- colData colData<- rowData rowData<- rowRanges
#'   rowRanges<-
#' @importFrom cowplot plot_grid
#' @importFrom dplyr arrange bind_rows case_when desc filter funs group_by
#'   left_join mutate mutate_all mutate_at mutate_if n pull rename select
#'   select_if slice summarise summarise_all top_n ungroup
#' @importFrom ensembldb ensemblVersion
#' @importFrom ggplot2 aes coord_fixed coord_flip element_blank element_line
#'   element_rect element_text expand_limits facet_wrap geom_bar geom_boxplot
#'   geom_density geom_hline geom_jitter geom_label geom_point geom_violin
#'   geom_vline ggplot guides labs position_jitterdodge scale_x_continuous
#'   scale_y_continuous stat_ecdf stat_summary theme theme_linedraw
#' @importFrom ggrepel geom_label_repel
#' @importFrom goalie allAreHexColors areDisjointSets areIntersectingSets
#'   areSameLength areSetEqual assert false getNameInParent hasDims hasInternet
#'   hasLength hasNames hasNoDuplicates hasNonZeroRowsAndCols hasRows
#'   hasRownames hasUniqueCols hasValidDimnames isADirectory  isAny isCharacter
#'   isFlag isGGScale isHeaderLevel isHexColorFunction isInClosedRange isInRange
#'   isInt isMatchingRegex isNonNegative isNotMatchingRegex isNumber isPositive
#'   isScalar isString isSubset isSuperset validNames validate validateClasses
#' @importFrom grDevices colorRampPalette
#' @importFrom grid arrow unit
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom magrittr %>% set_colnames set_rownames
#' @importFrom matrixStats colVars rowVars
#' @importFrom methods as coerce formalArgs getGeneric getMethod is isGeneric
#'   new selectMethod setAs setClass setGeneric setMethod setOldClass show
#'   signature slot slotNames validObject .hasSlot
#' @importFrom pheatmap pheatmap
#' @importFrom purrr map
#' @importFrom readr cols read_lines read_tsv
#' @importFrom reshape2 melt
#' @importFrom rlang !! !!! := UQ sym syms
#' @importFrom sessioninfo session_info
#' @importFrom stats as.formula dist hclust prcomp quantile
#' @importFrom stringr str_dup str_extract str_length str_match str_pad
#'   str_replace_all str_subset str_trunc
#' @importFrom tidyr gather separate
#' @importFrom tidyselect everything matches
#' @importFrom tibble as_tibble column_to_rownames tibble
#' @importFrom transformer as_tibble
#' @importFrom utils capture.output data getFromNamespace globalVariables
#'   installed.packages packageVersion
NULL



#' @export
BiocGenerics::as.data.frame

#' @export
SummarizedExperiment::assay

#' @export
SummarizedExperiment::assayNames

#' @export
SummarizedExperiment::assays

#' @export
SummarizedExperiment::colData

#' @export
SummarizedExperiment::rowData

#' @export
SummarizedExperiment::rowRanges

#' @export
magrittr::`%>%`

#' @export
tibble::as_tibble

#' @export
tibble::tibble



#' @importFrom brio assignAndSaveData
#' @export
brio::assignAndSaveData

#' @importFrom brio basenameSansExt
#' @export
brio::basenameSansExt

#' @importFrom brio dots
#' @export
brio::dots

#' @importFrom brio export
#' @export
brio::export

#' @importFrom brio extPattern
#' @export
brio::extPattern

#' @importFrom brio import
#' @export
brio::import

#' @importFrom brio initDir
#' @export
brio::initDir

#' @importFrom brio loadData
#' @export
brio::loadData

#' @importFrom brio loadDataAsName
#' @export
brio::loadDataAsName

#' @importFrom brio loadRemoteData
#' @export
brio::loadRemoteData

#' @importFrom brio localOrRemoteFile
#' @export
brio::localOrRemoteFile

#' @importFrom brio naStrings
#' @export
brio::naStrings

#' @importFrom brio pasteURL
#' @export
brio::pasteURL

#' @importFrom brio rdataExtPattern
#' @export
brio::rdataExtPattern

#' @importFrom brio rdataLoadError
#' @export
brio::rdataLoadError

#' @importFrom brio realpath
#' @export
brio::realpath

#' @importFrom brio sanitizeColData
#' @export
brio::sanitizeColData

#' @importFrom brio sanitizeRowData
#' @export
brio::sanitizeRowData

#' @importFrom brio sanitizeRowRanges
#' @export
brio::sanitizeRowRanges

#' @importFrom brio saveData
#' @export
brio::saveData

#' @importFrom brio transmit
#' @export
brio::transmit

#' @importFrom brio writeCounts
#' @export
brio::writeCounts



#' @importFrom goalie extractLocal
#' @export
goalie::extractLocal

#' @importFrom goalie hasLocal
#' @export
goalie::hasLocal

#' @importFrom goalie matchArgsToDoCall
#' @export
goalie::matchArgsToDoCall

#' @importFrom goalie methodFormals
#' @export
goalie::methodFormals

#' @importFrom goalie methodFunction
#' @export
goalie::methodFunction

#' @importFrom goalie standardizeCall
#' @export
goalie::standardizeCall



#' @importFrom syntactic camel
#' @export
syntactic::camel

#' @importFrom syntactic dotted
#' @export
syntactic::dotted

#' @importFrom syntactic makeDimnames
#' @export
syntactic::makeDimnames

#' @importFrom syntactic makeNames
#' @export
syntactic::makeNames

#' @importFrom syntactic snake
#' @export
syntactic::snake

#' @importFrom syntactic upperCamel
#' @export
syntactic::upperCamel



#' @importFrom transformer as.SummarizedExperiment
#' @export
transformer::as.SummarizedExperiment

#' @importFrom transformer atomize
#' @export
transformer::atomize

#' @importFrom transformer coerceS4ToList
#' @export
transformer::coerceS4ToList

#' @importFrom transformer factorize
#' @export
transformer::factorize

#' @importFrom transformer flatFiles
#' @export
transformer::flatFiles

#' @exportMethod coerce
NULL

setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "DataFrame",
            to = "tbl_df"
        ),
        where = "transformer"
    )
)

setAs(
    from = "data.frame",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "data.frame",
            to = "tbl_df"
        ),
        where = "transformer"
    )
)

setAs(
    from = "sparseMatrix",
    to = "DataFrame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "sparseMatrix",
            to = "DataFrame"
        ),
        where = "transformer"
    )
)

setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "sparseMatrix",
            to = "data.frame"
        ),
        where = "transformer"
    )
)

setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "tbl_df",
            to = "DataFrame"
        ),
        where = "transformer"
    )
)

setAs(
    from = "GRanges",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "GRanges",
            to = "tbl_df"
        ),
        where = "transformer"
    )
)
