#' Map cells to samples
#'
#' This function extracts `sampleID` from the `cellID` column using grep
#' matching.
#'
#' @name mapCellsToSamples
#' @note Updated 2019-08-18.
#' @export
#'
#' @inheritParams acidroxygen::params
#'
#' @return `factor`.
#' Cells as the names and samples as the levels.
#'
#' @examples
#' samples <- paste0("sample", seq_len(2L))
#' print(samples)
#' cells <- paste(samples, c("AAAAAAAA", "CCCCCCCC"), sep = "_")
#' print(cells)
#' mapCellsToSamples(cells, samples)
mapCellsToSamples <- function(cells, samples) {
    assert(
        isCharacter(cells), hasNoDuplicates(cells),
        isCharacter(samples), hasNoDuplicates(samples)
    )
    ## Early return as simple factor for single sample.
    ## This code is useful for working with some example objects (e.g. PBMC).
    if (isString(samples)) {
        ## Check that cells input contains expected barcodes with DNA bases.
        assert(allAreMatchingRegex(x = cells, pattern = "[ACGT]+$"))
        cell2sample <- factor(replicate(n = length(cells), expr = samples))
        names(cell2sample) <- cells
        return(cell2sample)
    }
    ## Check that all cells contain a separator.
    assert(allAreMatchingRegex(x = cells, pattern = "[_-]"))
    list <- lapply(X = samples, FUN = function(sample) {
        pattern <- paste0("^(", sample, barcodePattern)
        match <- str_match(string = cells, pattern = pattern)
        if (all(is.na(match[, 1L]))) {
            stop(sprintf(
                "'%s' sample failed to match any cells.",
                deparse(sample)
            ))
        }
        ## Trailing number is for matching Cell Ranger output.
        ## This gets used in the Chromium package.
        colnames(match) <- c(
            "cellID",
            "sampleID",
            "cellularBarcode",
            "trailingNumber"
        )
        match <- match[, c("cellID", "sampleID")]
        keep <- !is.na(match[, "sampleID", drop = TRUE])
        match <- match[keep, , drop = FALSE]
        out <- match[, "sampleID", drop = TRUE]
        names(out) <- match[, "cellID", drop = TRUE]
        out
    })
    cell2sample <- unlist(list, recursive = FALSE, use.names = TRUE)
    assert(identical(length(cells), length(cell2sample)))
    as.factor(cell2sample)
}
