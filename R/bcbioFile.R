#' Import a bcbio project file
#'
#' @author Michael Steinbaugh
#' @keywords bcbio import
#'
#' @import readr
#'
#' @param project bcbio project list
#' @param file File name
#' @param input Input format
#' @param output Desired output format
#' @param rownames Column identifier to use for rownames
#'
#' @export
bcbioFile <- function(project,
                      file,
                      input = "tsv",
                      output = "data.frame",
                      rownames = NULL) {
    realpath <- file.path(project$projectDir, file)
    if (!file.exists(realpath)) {
        stop("File could not be found.")
    }

    # File import
    if (input == "csv") {
        data <- readr::read_csv(realpath)
    } else if (input == "tsv") {
        data <- readr::read_tsv(realpath)
    }

    # Coerce to data frame
    data <- as.data.frame(data)

    # Set rownames
    if (!is.null(rownames)) {
        rownames(data) <- data[[rownames]]
        data[[rownames]] <- NULL
    }

    # Convert to desired output
    if (output == "matrix") {
        data <- as.matrix(data)
    }

    return(data)
}
