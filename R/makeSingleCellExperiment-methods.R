#' Make a SingleCellExperiment object
#'
#' @name makeSingleCellExperiment
#' @include makeSummarizedExperiment-methods.R
#' @inherit makeSummarizedExperiment
#' @note Updated 2019-08-22.
#'
#' @inheritParams acidroxygen::params
#'
#' @return `SingleCellExperiment`.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#'
#' ## SimpleList ====
#' object <- SingleCellExperiment
#' assays <- assays(object)
#' rowRanges <- rowRanges(object)
#' colData <- colData(object)
#' metadata <- metadata(object)
#' reducedDims <- reducedDims(object)
#'
#' x <- makeSingleCellExperiment(
#'     assays = assays,
#'     rowRanges = rowRanges,
#'     colData = colData,
#'     metadata = metadata,
#'     reducedDims = reducedDims
#' )
#' print(x)
NULL



## Updated 2019-08-22.
`makeSingleCellExperiment,SimpleList` <- function(
    assays,
    rowRanges,
    colData,
    metadata,
    reducedDims,
    transgeneNames = NULL,
    spikeNames = NULL
) {
    assert(
        isAny(reducedDims, c("SimpleList", "list", "NULL"))
    )
    ## Coerce reducedDims to SimpleList, for consistency.
    if (!is(reducedDims, "SimpleList")) {
        reducedDims <- SimpleList(reducedDims)
    }
    ## Don't enforce camel case here, since it's currently common to slot PCA,
    ## TSNE, UMAP assays (note upper case).
    if (hasLength(reducedDims)) {
        assert(hasValidNames(reducedDims))
    }
    ## Make SummarizedExperiment first.
    ## Supports automatic resizing of rowRanges and helps slot FASTA spike-ins.
    se <- makeSummarizedExperiment(
        assays = assays,
        rowRanges = rowRanges,
        colData = colData,
        metadata = metadata,
        transgeneNames = transgeneNames,
        spikeNames = spikeNames
    )
    ## SCE constructor currently errors on empty rowRanges.
    rowRanges <- rowRanges(se)
    if (!hasLength(rowRanges)) {
        rowRanges <- emptyRanges(names = rownames(se))
    }
    ## Then coerce to SingleCellExperiment.
    ## Note that `as` method isn't currently returning valid.
    sce <- SingleCellExperiment(
        assays = assays(se),
        rowRanges = rowRanges,
        colData = colData(se),
        metadata = metadata(se),
        reducedDims = reducedDims
    )
    ## Optionally, use `isSpike` internally to define the `spikeNames`.
    if (is.character(spikeNames)) {
        for (i in seq_along(spikeNames)) {
            isSpike(sce, spikeNames[[i]]) <- spikeNames[[i]]
        }
    }
    validObject(sce)
    sce
}

args <- c("rowRanges", "colData", "metadata")
formals(`makeSingleCellExperiment,SimpleList`)[args] <-
    formals(`makeSummarizedExperiment,SimpleList`)[args]
args <- "reducedDims"
formals(`makeSingleCellExperiment,SimpleList`)[args] <-
    formals(SingleCellExperiment)[args]



#' @rdname makeSingleCellExperiment
#' @export
setMethod(
    f = "makeSingleCellExperiment",
    signature = signature(assays = "SimpleList"),
    definition = `makeSingleCellExperiment,SimpleList`
)



## Updated 2019-08-07.
`makeSingleCellExperiment,list` <-  # nolint
    `makeSingleCellExperiment,SimpleList`



#' @rdname makeSingleCellExperiment
#' @export
setMethod(
    f = "makeSingleCellExperiment",
    signature = signature(assays = "list"),
    definition = `makeSingleCellExperiment,list`
)
