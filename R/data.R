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



#' Organism Mappings
#'
#' Strings to match gene/transcript identifiers, Ensembl genome build,
#' and UCSC genome build.
#'
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @examples
#' print(organism_mappings)
"organism_mappings"



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



#' Example Single-Cell RNA-Seq Data Set
#'
#' @family Minimal Example Data
#' @author Michael Steinbaugh
#'
#' @seealso `scater::sc_example_counts`.
#'
#' @examples
#' print(sce_small)
"sce_small"



#' Example Transcript-Level Data Set
#'
#' @family Minimal Example Data
#' @author Michael Steinbaugh
#'
#' @examples
#' print(tx_se_small)
"tx_se_small"
