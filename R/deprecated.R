## nocov start
## nolint start



#' @name deprecated
#' @inherit AcidRoxygen::deprecated description examples return seealso title
#' @inheritParams AcidRoxygen::params
#' @keywords internal
NULL



#' @rdname deprecated
#' @export
matchesGene2Symbol <- function(...) {
    .Defunct()
}



## NOTE methods reexports here are used in bcbioRNASeq and pointillism.
## These are safe to remove once downstream packages are updated.

#' @importFrom methods as
#' @export
methods::as

#' @importFrom methods formalArgs
#' @export
methods::formalArgs

#' @importFrom methods is
#' @export
methods::is

#' @importFrom methods new
#' @export
methods::new

#' @importFrom methods validObject
#' @export
methods::validObject



## nolint end
## nocov end
