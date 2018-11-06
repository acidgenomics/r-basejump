# TODO Create small working example with dimoprhic genes.
# TODO Move this code to `plotGene-methods.R`?



#' @name plotGenderMarkers
#' @inheritParams plotGene
#' @inheritParams params
#' @include plotGene-methods.R
NULL



# SummarizedExperiment =========================================================
plotGenderMarkers.SummarizedExperiment <-  # nolint
    function() {
        validObject(object)

        # Load the relevant internal gender markers data.
        organism <- organism(object)
        data("gender_markers", package = "basejump", envir = environment())
        markers <- get("gender_markers", inherits = FALSE)
        assert_is_list(markers)
        # Error if the organism is not supported.
        # Convert from camel case back to full Latin.
        supportedOrganisms <- names(markers) %>%
            snake() %>%
            sub("^([a-z])", "\\U\\1", ., perl = TRUE) %>%
            sub("_", " ", .)
        if (!organism %in% supportedOrganisms) {
            stop(paste0(
                organism, " is not supported.\n",
                "Supported organisms: ",
                toString(supportedOrganisms)
            ))
        }
        markers <- markers[[camel(organism)]]
        assert_is_tbl_df(markers)

        # Message the user instead of erroring, since many datasets don't
        # contain the dimorphic gender markers.
        genes <- tryCatch(
            mapGenesToRownames(
                object = object,
                genes = markers[["geneID"]]
            ),
            error = function(e) {
                message(as.character(e))
                character()
            }
        )
        if (!has_length(genes)) {
            return(invisible())
        }

        do.call(
            what = plotGene,
            args = matchArgsToDoCall(
                args = list(genes = genes)
            )
        )
    }
f <- formals(plotGene.SummarizedExperiment)
f <- f[setdiff(names(f), "genes")]
f[["style"]] <- "wide"
formals(plotGenderMarkers.SummarizedExperiment) <- f



#' @rdname plotGenderMarkers
#' @export
setMethod(
    "plotGenderMarkers",
    signature("SummarizedExperiment"),
    definition = plotGenderMarkers.SummarizedExperiment
)



# SingleCellExperiment =========================================================
#' @rdname plotGenderMarkers
#' @usage NULL
#' @export
setMethod(
    "plotGenderMarkers",
    signature("SingleCellExperiment"),
    definition = function(object, ...) {
        stop("SingleCellExperiment is not currently supported.")
    }
)
