# Legacy support for S4 class definitions.
# Safe to remove in a future update, once depency packages are updated.
# Refer to basejump.classes for documentation.



#' Classes exported from other packages
#'
#' These classes are imported from other packages.
#' Follow the links below to see their documentation.
#'
#' - `help(topic = "EggNOG-class", package = "basejump.classes")`.
#' - `help(topic = "Ensembl2Entrez-class", package = "basejump.classes")`.
#' - `help(topic = "Gene2Symbol-class", package = "basejump.classes")`.
#' - `help(topic = "HGNC2Ensembl-class", package = "basejump.classes")`.
#' - `help(topic = "MGI2Ensembl-class", package = "basejump.classes")`.
#' - `help(topic = "PANTHER-class", package = "basejump.classes")`.
#' - `help(topic = "Tx2Gene-class", package = "basejump.classes")`.
#'
#' @name classes
#' @docType import
#' @keywords internal
NULL



#' @rdname classes
setClass(Class = "EggNOG", contains = "SimpleDataFrameList")

#' @rdname classes
setClass(Class = "Ensembl2Entrez", contains = "DataFrame")

#' @rdname classes
setClass(Class = "Gene2Symbol", contains = "DataFrame")

#' @rdname classes
setClass(Class = "HGNC2Ensembl", contains = "DataFrame")

#' @rdname classes
setClass(Class = "MGI2Ensembl", contains = "DataFrame")

#' @rdname classes
setClass(Class = "PANTHER", contains = "DataFrame")

#' @rdname classes
setClass(Class = "Tx2Gene", contains = "DataFrame")
