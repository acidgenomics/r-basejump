#' @importFrom AcidGenerics aggregate
#' @export
#' @importMethodsFrom AcidExperiment aggregate
#' @exportMethod aggregate
AcidGenerics::aggregate

#' @importFrom AcidGenerics aggregateCols
#' @export
#' @importMethodsFrom AcidExperiment aggregateCols
#' @exportMethod aggregateCols
AcidGenerics::aggregateCols

#' @importFrom AcidGenerics aggregateRows
#' @export
#' @importMethodsFrom AcidExperiment aggregateRows
#' @exportMethod aggregateRows
AcidGenerics::aggregateRows

#' @importFrom AcidGenerics autopadZeros
#' @export
#' @importMethodsFrom AcidExperiment autopadZeros
#' @exportMethod autopadZeros
AcidGenerics::autopadZeros

#' @importFrom AcidGenerics as.SummarizedExperiment
#' @export
#' @importMethodsFrom AcidExperiment as.SummarizedExperiment
#' @exportMethod as.SummarizedExperiment
AcidGenerics::as.SummarizedExperiment

#' @importFrom AcidGenerics export
#' @export
#' @importMethodsFrom AcidExperiment export
#' @importMethodsFrom AcidGenomes export
#' @exportMethod export
AcidGenerics::export

#' @importFrom AcidGenerics geneNames
#' @export
#' @importMethodsFrom AcidExperiment geneNames
#' @exportMethod geneNames
AcidGenerics::geneNames

## FIXME
## > #' @importMethodsFrom AcidGenerics mcols mcols<-
## > #' @exportMethod mcols mcols<-
## > NULL

#' @importFrom AcidGenerics mcols
#' @export
AcidGenerics::mcols

#' @importFrom AcidGenerics mcols<-
#' @export
AcidGenerics::`mcols<-`

## > #' @importMethodsFrom AcidGenerics metadata metadata<-
## > #' @exportMethod metadata metadata<-
## > NULL

#' @importFrom AcidGenerics metadata
#' @export
AcidGenerics::metadata

#' @importFrom AcidGenerics metadata<-
#' @export
AcidGenerics::`metadata<-`



#' @importFrom AcidGenomes Ensembl2Entrez
#' @export
AcidGenomes::Ensembl2Entrez

#' @importFrom AcidGenomes Entrez2Ensembl
#' @export
AcidGenomes::Entrez2Ensembl

#' @importFrom AcidGenomes Gene2Symbol
#' @export
AcidGenomes::Gene2Symbol

#' @importFrom AcidGenomes HGNC
#' @export
AcidGenomes::HGNC

#' @importFrom AcidGenomes HGNC2Ensembl
#' @export
AcidGenomes::HGNC2Ensembl

#' @importFrom AcidGenomes MGI2Ensembl
#' @export
AcidGenomes::MGI2Ensembl

#' @importFrom AcidGenomes Tx2Gene
#' @export
AcidGenomes::Tx2Gene
