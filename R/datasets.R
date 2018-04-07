#' Example RNA-Seq Counts
#'
#' Generated with `DESeq2::makeExampleDESeqDataSet()`.
#'
#' @author Michael Steinbaugh
#'
#' @examples
#' summary(rnaseqCounts)
"rnaseqCounts"



#' Example Single-Cell RNA-Seq Counts
#'
#' Modified version of `scater::sc_example_counts`, with dimnames formatted
#' in camel case for package consistency.
#'
#' @examples
#' summary(singleCellCounts)
"singleCellCounts"



#' Gene Name Synonyms
#'
#' A list of tibbles per organism containing gene name (a.k.a. symbol) synonyms.
#'
#' @author Michael Steinbaugh
#'
#' @examples
#' names(synonyms)
#' glimpse(synonyms[["homoSapiens"]])
"synonyms"
