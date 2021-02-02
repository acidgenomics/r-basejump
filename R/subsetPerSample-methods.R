#' @name subsetPerSample
#' @inherit AcidGenerics::subsetPerSample
#' @note Updated 2019-08-11.
#'
#' @inheritParams AcidRoxygen::params
#' @param minCells `integer(1)`.
#'   Minimum number of cells required per sample.
#' @param assignAndSave `logical(1)`.
#'   Assign and save the individual datasets.
#' @param envir `environment`.
#'   Where to assign the subsets.
#'   Only applicable when `assignAndSave = TRUE`.
#' @param dir `character(1)`.
#'   Output directory.
#'   Only applicable when `assignAndSave = TRUE`.
#' @param ... Additional arguments.
#'
#' @return
#' - `assignAndSave = FALSE`: Per sample objects in a `list`.
#' - `assignAndSave = TRUE`: Subset file paths.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#' sce <- SingleCellExperiment
#'
#' ## SingleCellExperiment ====
#' object <- sce
#'
#' ## List mode (default).
#' list <- subsetPerSample(object, assignAndSave = FALSE)
#' names(list)
#'
#' ## Assign and save mode (useful for large datasets).
#' subsetPerSample(
#'     object = object,
#'     assignAndSave = TRUE,
#'     envir = parent.frame(),
#'     dir = "subsetPerSample"
#' )
#' sort(list.files("subsetPerSample"))
#'
#' ## Clean up.
#' unlink("subsetPerSample", recursive = TRUE)
NULL



## Updated 2019-08-11.
`subsetPerSample,SingleCellExperiment` <-  # nolint
    function(
        object,
        minCells = 1L,
        assignAndSave = FALSE,
        envir = parent.frame(),
        dir = "."
    ) {
        assert(
            isInt(minCells),
            isPositive(minCells),
            isFlag(assignAndSave),
            is.environment(envir)
        )
        dir <- initDir(dir)
        ## Return objects or file paths.
        samples <- levels(cell2sample(object))
        return <- lapply(
            X = samples,
            FUN = function(sampleId) {
                subset <- selectSamples(object, sampleId = sampleId)
                ## Skip if subset doesn't have enough cells.
                if (ncol(subset) < minCells) {
                    alertWarning(sprintf(
                        "'%s' didn't pass minimum cell cutoff.",
                        sampleId
                    ))
                    return(NULL)
                }
                if (isTRUE(assignAndSave)) {
                    assignAndSaveData(
                        name = sampleId,
                        object = subset,
                        envir = envir,
                        dir = dir
                    )
                } else {
                    subset
                }
            }
        )
        names(return) <- samples
        return <- Filter(Negate(is.null), return)
        if (isTRUE(assignAndSave)) {
            ## File paths.
            names <- names(return)
            return <- unlist(return)
            return <- realpath(return)
            names(return) <- names
            invisible(return)
        } else {
            ## Individual objects.
            return
        }
    }



#' @rdname subsetPerSample
#' @export
setMethod(
    f = "subsetPerSample",
    signature = signature("SingleCellExperiment"),
    definition = `subsetPerSample,SingleCellExperiment`
)
