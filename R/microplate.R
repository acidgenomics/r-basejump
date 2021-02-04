## FIXME MOVE TO ACIDEXPERIMENT.



#' Microtiter plate well identifiers
#'
#' Quickly generate identifiers (with optional prefixes) for 96 and 384 well
#' plates.
#'
#' These plate formats are frequently used for high-throughput screening assays.
#'
#' @note Updated 2019-08-18.
#' @export
#'
#' @param plates `integer(1)`.
#'   Number of plates.
#' @param wells `integer(1)`.
#'   Number of wells (`96`, `384`).
#' @param controls `integer(1)`.
#'   Number of control wells.
#' @param prefix `character(1)` or `NULL`.
#'   Plate name prefix.
#'
#' @return `character`.
#' Well identifiers.
#'
#' @examples
#' ## Single 96-well plate.
#' microplate(wells = 96L)
#'
#' ## 2 96-well plates.
#' microplate(plates = 2L, wells = 96L)
#'
#' ## Single 384-well plate.
#' microplate(wells = 384L)
#'
#' ## 2 96-well plates with 6 control wells per plate.
#' microplate(plates = 2L, wells = 96L, controls = 6L)
microplate <- function(
    plates = 1L,
    wells = 96L,
    controls = 0L,
    prefix = NULL
) {
    plates <- as.integer(plates)
    wells <- as.integer(wells)
    controls <- as.integer(controls)
    assert(
        isInt(plates),
        isPositive(plates),
        isInt(wells),
        isPositive(wells),
        isSubset(x = wells, y = c(96L, 384L)),
        isInt(controls),
        isInRange(x = controls, lower = 0L, upper = 12L),
        isString(prefix, nullOK = TRUE)
    )
    if (wells == 96L) {
        row <- 8L
        col <- 12L
    } else if (wells == 384L) {
        row <- 16L
        col <- 24L
    }
    row <- LETTERS[seq_len(row)]
    col <- seq_len(col)
    col <- str_pad(col, width = max(str_length(col)), pad = "0")
    plates <- seq_len(plates)
    plates <- str_pad(plates, width = max(str_length(plates)), pad = "0")
    df <- expand.grid(plates, row, col)
    vector <- sort(paste0(df[["Var1"]], "-", df[["Var2"]], df[["Var3"]]))
    ## Prepare control wells.
    if (controls > 0L) {
        ## Create a grep string matching the control wells.
        grep <- str_pad(
            seq_len(controls),
            width = max(str_length(col)),
            pad = "0"
        )
        grep <- paste(grep, collapse = "|")
        grep <- paste0("A(", grep, ")$")
        ## Remove the control wells using `grepl`.
        keep <- !grepl(grep, vector)
        vector <- vector[keep]
    }
    ## Add a prefix, if desired.
    if (isString(prefix)) {
        vector <- paste0(prefix, "-", vector)
    }
    ## Return.
    vector
}
