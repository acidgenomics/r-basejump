# Legacy support for S4 class definitions.
# Safe to remove in a future update, once depency packages are updated.
# Refer to basejump.classes for documentation.

#' @inherit basejump.classes::`EggNOG-class`
#' @keywords internal
setClass(Class = "EggNOG", contains = "SimpleDataFrameList")

#' @inherit basejump.classes::`Ensembl2Entrez-class`
#' @keywords internal
setClass(Class = "Ensembl2Entrez", contains = "DataFrame")

#' @inherit basejump.classes::`Gene2Symbol-class`
#' @keywords internal
setClass(Class = "Gene2Symbol", contains = "DataFrame")

#' @inherit basejump.classes::`HGNC2Ensembl-class`
#' @keywords internal
setClass(Class = "HGNC2Ensembl", contains = "DataFrame")

#' @inherit basejump.classes::`MGI2Ensembl-class`
#' @keywords internal
setClass(Class = "MGI2Ensembl", contains = "DataFrame")

#' @inherit basejump.classes::`PANTHER-class`
#' @keywords internal
setClass(Class = "PANTHER", contains = "DataFrame")

#' @inherit basejump.classes::`Tx2Gene-class`
#' @keywords internal
setClass(Class = "Tx2Gene", contains = "DataFrame")
