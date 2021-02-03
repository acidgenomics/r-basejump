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

#' @importFrom AcidGenerics antiJoin
#' @export
#' @importMethodsFrom AcidPlyr antiJoin
#' @exportMethod antiJoin
AcidGenerics::antiJoin

#' @importFrom AcidGenerics as.SummarizedExperiment
#' @export
#' @importMethodsFrom AcidExperiment as.SummarizedExperiment
#' @exportMethod as.SummarizedExperiment
AcidGenerics::as.SummarizedExperiment

#' @importFrom AcidGenerics autopadZeros
#' @export
#' @importMethodsFrom AcidExperiment autopadZeros
#' @exportMethod autopadZeros
AcidGenerics::autopadZeros

#' @importFrom AcidGenerics export
#' @export
#' @importMethodsFrom AcidExperiment export
#' @importMethodsFrom AcidGenomes export
#' @exportMethod export
AcidGenerics::export

#' @importFrom AcidGenerics fullJoin
#' @export
#' @importMethodsFrom AcidPlyr fullJoin
#' @exportMethod fullJoin
AcidGenerics::fullJoin

#' @importFrom AcidGenerics geneNames
#' @export
#' @importMethodsFrom AcidExperiment geneNames
#' @exportMethod geneNames
AcidGenerics::geneNames

#' @importFrom AcidGenerics innerJoin
#' @export
#' @importMethodsFrom AcidPlyr innerJoin
#' @exportMethod innerJoin
AcidGenerics::innerJoin

#' @importFrom AcidGenerics leftJoin
#' @export
#' @importMethodsFrom AcidPlyr leftJoin
#' @exportMethod leftJoin
AcidGenerics::leftJoin

## NOTE This doesn't work:
## > #' @importMethodsFrom AcidGenerics mcols
## > #' @exportMethod mcols

#' @importFrom AcidGenerics mcols
#' @export
AcidGenerics::mcols

## NOTE This doesn't work:
## > #' @importMethodsFrom AcidGenerics mcols<-
## > #' @exportMethod mcols<-

#' @importFrom AcidGenerics mcols<-
#' @export
AcidGenerics::`mcols<-`

## NOTE This doesn't work:
## > #' @importMethodsFrom AcidGenerics metadata
## > #' @exportMethod metadata

#' @importFrom AcidGenerics metadata
#' @export
AcidGenerics::metadata

## NOTE This doesn't work:
## > #' @importMethodsFrom AcidGenerics metadata<-
## > #' @exportMethod metadata<-

#' @importFrom AcidGenerics metadata<-
#' @export
AcidGenerics::`metadata<-`

#' @importFrom AcidGenerics mutateAll
#' @export
#' @importMethodsFrom AcidPlyr mutateAll
#' @exportMethod mutateAll
AcidGenerics::mutateAll

#' @importFrom AcidGenerics mutateAt
#' @export
#' @importMethodsFrom AcidPlyr mutateAt
#' @exportMethod mutateAt
AcidGenerics::mutateAt

#' @importFrom AcidGenerics mutateIf
#' @export
#' @importMethodsFrom AcidPlyr mutateIf
#' @exportMethod mutateIf
AcidGenerics::mutateIf

#' @importFrom AcidGenerics organism
#' @export
#' @importMethodsFrom AcidExperiment organism
#' @importMethodsFrom AcidGenomes organism
#' @exportMethod organism
AcidGenerics::organism

#' @importFrom AcidGenerics organism<-
#' @export
#' @importMethodsFrom AcidGenomes organism<-
#' @exportMethod organism<-
AcidGenerics::`organism<-`

#' @importFrom AcidGenerics rightJoin
#' @export
#' @importMethodsFrom AcidPlyr rightJoin
#' @exportMethod rightJoin
AcidGenerics::rightJoin

#' @importFrom AcidGenerics selectIf
#' @export
#' @importMethodsFrom AcidPlyr selectIf
#' @exportMethod selectIf
AcidGenerics::selectIf

#' @importFrom AcidGenerics semiJoin
#' @export
#' @importMethodsFrom AcidPlyr semiJoin
#' @exportMethod semiJoin
AcidGenerics::semiJoin

#' @importFrom AcidGenerics splitByLevel
#' @export
#' @importMethodsFrom AcidPlyr splitByLevel
#' @exportMethod splitByLevel
AcidGenerics::splitByLevel

#' @importFrom AcidGenerics stripGeneVersions
#' @export
#' @importMethodsFrom AcidExperiment stripGeneVersions
#' @importMethodsFrom AcidGenomes stripGeneVersions
#' @exportMethod stripGeneVersions
AcidGenerics::stripGeneVersions

#' @importFrom AcidGenerics stripTranscriptVersions
#' @export
#' @importMethodsFrom AcidExperiment stripTranscriptVersions
#' @importMethodsFrom AcidGenomes stripTranscriptVersions
#' @exportMethod stripTranscriptVersions
AcidGenerics::stripTranscriptVersions

#' @importFrom AcidGenerics transmuteAt
#' @export
#' @importMethodsFrom AcidPlyr transmuteAt
#' @exportMethod transmuteAt
AcidGenerics::transmuteAt

#' @importFrom AcidGenerics transmuteIf
#' @export
#' @importMethodsFrom AcidPlyr transmuteIf
#' @exportMethod transmuteIf
AcidGenerics::transmuteIf



#' @importFrom AcidGenomes Ensembl2Entrez
#' @export
#' @importMethodsFrom AcidGenomes Ensembl2Entrez
#' @exportMethod Ensembl2Entrez
AcidGenomes::Ensembl2Entrez

#' @importFrom AcidGenomes Entrez2Ensembl
#' @export
#' @importMethodsFrom AcidGenomes Entrez2Ensembl
#' @exportMethod Entrez2Ensembl
AcidGenomes::Entrez2Ensembl

#' @importFrom AcidGenomes Gene2Symbol
#' @export
AcidGenomes::Gene2Symbol

#' @importFrom AcidGenomes Tx2Gene
#' @export
#' @importMethodsFrom AcidGenomes Tx2Gene
#' @exportMethod Tx2Gene
AcidGenomes::Tx2Gene
