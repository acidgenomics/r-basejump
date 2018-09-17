#' Sexually Dimorphic Gender Marker Genes
#'
#' *Homo sapiens* and *Mus musculus* are currently defined.
#'
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @examples
#' names(gender_markers)
"gender_markers"



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



#' Example RNA-Seq Data Set
#'
#' Generated with `DESeq2::makeExampleDESeqDataSet()`. Contains actual gene
#' identifiers and Ensembl annotations.
#'
#' @family Minimal Example Data
#' @author Michael Steinbaugh
#'
#' @examples
#' print(rse_small)
"rse_small"



#' Example Single-Cell RNA-Seq Counts
#'
#' Modified version of `scater::sc_example_counts`, with dimnames formatted
#' in camel case for package consistency.
#'
#' @aliases singleCellCounts
#' @author Michael Steinbaugh
#'
#' @examples
#' dim(single_cell_counts)
#' class(single_cell_counts)
#' is(single_cell_counts, "sparseMatrix")
"single_cell_counts"



#' Example Transcript-Level Data Set
#'
#' @family Minimal Example Data
#' @author Michael Steinbaugh
#'
#' @examples
#' print(tx_se_small)
"tx_se_small"
