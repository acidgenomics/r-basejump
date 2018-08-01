#' Example RNA-Seq Counts
#'
#' Generated with `DESeq2::makeExampleDESeqDataSet()`.
#'
#' @aliases rnaseqCounts
#' @author Michael Steinbaugh
#'
#' @examples
#' summary(rnaseq_counts)
"rnaseq_counts"



#' `RangedSummarizedExperiment` Coerced from `bcbioRNASeq`
#'
#' @family Minimal Example Data
#' @author Michael Steinbaugh
"rse_bcb"



#' `RangedSummarizedExperiment` Coerced from `DESeqDataSet`
#'
#' @family Minimal Example Data
#' @author Michael Steinbaugh
"rse_dds"



#' Example Single-Cell RNA-Seq Counts
#'
#' Modified version of `scater::sc_example_counts`, with dimnames formatted
#' in camel case for package consistency.
#'
#' @aliases singleCellCounts
#' @author Michael Steinbaugh
#'
#' @examples
#' summary(single_cell_counts)
"single_cell_counts"
