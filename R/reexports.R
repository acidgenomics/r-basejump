#' S4 generics exported from other packages
#'
#' These generics are imported from other packages.
#'
#' @section bioverbs:
#' - `plotDEGHeatmap`
#' - `plotDEGPCA`
#'
#' @section brio:
#' - `export`
#' - `removeNA`
#' - `sanitizeNA`
#' - `sanitizePercent`
#'
#' @section syntactic:
#' - `camelCase`
#' - `capitalize`
#' - `dottedCase`
#' - `kebabCase`
#' - `mcolnames`, `mcolnames<-`
#' - `snakeCase`
#' - `upperCamelCase`
#'
#' @section transformer:
#' - `as.data.frame`
#' - `atomize`
#' - `decode`
#' - `encode`
#' - `factorize`
#' - `flatFiles`
#'
#' @name reexports-S4
#' @docType import
#'
#' @param object,x Object.
#' @param ... Additional arguments.
#'
#' @keywords internal
#'
#' @return No return.
#'
#' @examples
#' showMethods("camelCase")
NULL



#' @importFrom GenomicRanges GRanges
#' @export
GenomicRanges::GRanges

#' @importFrom GenomicRanges GRangesList
#' @export
GenomicRanges::GRangesList

#' @importFrom IRanges IRanges
#' @export
IRanges::IRanges

#' @importFrom S4Vectors DataFrame
#' @export
S4Vectors::DataFrame

#' @importFrom S4Vectors SimpleList
#' @export
S4Vectors::SimpleList

#' @importFrom S4Vectors mcols
#' @export
S4Vectors::mcols

#' @importFrom S4Vectors mcols<-
#' @export
S4Vectors::`mcols<-`

#' @importFrom S4Vectors metadata
#' @export
S4Vectors::metadata

#' @importFrom S4Vectors metadata<-
#' @export
S4Vectors::`metadata<-`

#' @importFrom SingleCellExperiment reducedDims
#' @export
SingleCellExperiment::reducedDims

#' @importFrom SingleCellExperiment reducedDims<-
#' @export
SingleCellExperiment::`reducedDims<-`

#' @importFrom SummarizedExperiment assayNames
#' @export
SummarizedExperiment::assayNames

#' @importFrom SummarizedExperiment assay
#' @export
SummarizedExperiment::assay

#' @importFrom SummarizedExperiment assay<-
#' @export
SummarizedExperiment::`assay<-`

#' @importFrom SummarizedExperiment assays
#' @export
SummarizedExperiment::assays

#' @importFrom SummarizedExperiment assays<-
#' @export
SummarizedExperiment::`assays<-`

#' @importFrom SummarizedExperiment colData
#' @export
SummarizedExperiment::colData

#' @importFrom SummarizedExperiment colData<-
#' @export
SummarizedExperiment::`colData<-`

#' @importFrom SummarizedExperiment rowData
#' @export
SummarizedExperiment::rowData

#' @importFrom SummarizedExperiment rowData<-
#' @export
SummarizedExperiment::`rowData<-`

#' @importFrom SummarizedExperiment rowRanges
#' @export
SummarizedExperiment::rowRanges

#' @importFrom SummarizedExperiment rowRanges<-
#' @export
SummarizedExperiment::`rowRanges<-`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' @importFrom magrittr %$%
magrittr::`%$%`

## Pipe
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

## Tee pipe
#' @importFrom magrittr %T>%
#' @export
magrittr::`%T>%`

#' @importFrom magrittr set_colnames
#' @export
magrittr::set_colnames

#' @importFrom magrittr set_names
#' @export
magrittr::set_names

#' @importFrom magrittr set_rownames
#' @export
magrittr::set_rownames

#' @importFrom tibble tibble
#' @export
tibble::tibble
