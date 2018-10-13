#' Subset Per Sample
#'
#' @name subsetPerSample
#' @family SingleCellExperiment Functions
#'
#' @inheritParams general
#' @param minCells `scalar integer`. Minimum number of cells required per
#'   sample.
#' @param assignAndSave `boolean`. Assign and save the individual datasets.
#' @param envir `environment`. Where to assign the subsets. Only applicable when
#'   `assignAndSave = TRUE`.
#' @param dir `string`. Output directory. Only applicable when
#'   `assignAndSave = TRUE`.
#'
#' @return
#' - `assignAndSave = FALSE`: Per sample objects in a `list`.
#' - `assignAndSave = TRUE`: Subset file paths.
#'
#' @examples
#' ## SingleCellExperiment ====
#' object <- sce_small
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
#' list.files("subsetPerSample")
#'
#' ## Clean up.
#' unlink("subsetPerSample", recursive = TRUE)
NULL



.subsetPerSample.SCE <-  # nolint
    function(
        object,
        minCells = 100L,
        assignAndSave = FALSE,
        envir = parent.frame(),
        dir = "."
    ) {
        assertIsAnImplicitInteger(minCells)
        assert_all_are_positive(minCells)
        assert_is_a_bool(assignAndSave)
        assert_is_environment(envir)
        dir <- initializeDirectory(dir)

        # Return objects or file paths.
        samples <- levels(cell2sample(object))
        return <- lapply(
            X = samples,
            FUN = function(sampleID) {
                subset <- selectSamples(object, sampleID = sampleID)
                # Skip if subset doesn't have enough cells.
                if (ncol(subset) < minCells) {
                    warning(paste(
                        sampleID, "didn't pass minimum cell cutoff."
                    ), call. = FALSE)
                    return(NULL)
                }
                if (isTRUE(assignAndSave)) {
                    assignAndSaveData(
                        name = sampleID,
                        object = subset,
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
            # File paths.
            names <- names(return)
            return <- unlist(return)
            return <- realpath(return)
            names(return) <- names
            invisible(return)
        } else {
            # Individual objects.
            return
        }
    }



#' @rdname subsetPerSample
#' @export
setMethod(
    f = "subsetPerSample",
    signature = signature("SingleCellExperiment"),
    definition = .subsetPerSample.SCE
)
