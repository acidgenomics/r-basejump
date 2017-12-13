#' Microtiter Plate Well Identifiers
#'
#' @importFrom rlang is_string
#' @importFrom stringr str_length str_pad
#'
#' @param plates Number of plates.
#' @param wells Number of wells (`96`, `384`).
#' @param controls Number of control wells.
#' @param prefix *Optional*. Plate name prefix.
#'
#' @return [data.frame].
#' @export
#'
#' @examples
#' # Single 96-well plate
#' microplate(wells = 96L)
#'
#' # 2 96-well plates
#' microplate(plates = 2L, wells = 96L)
#'
#' # Single 384-well plate
#' microplate(wells = 384L)
#'
#' # 2 96-well plates with 6 control wells per plate
#' microplate(plates = 2L, wells = 96L, controls = 6L)
microplate <- function(
    plates = 1L,
    wells = 96L,
    controls = 0L,
    prefix) {
    if (!is.numeric(plates) | plates < 1L) {
        stop("Invalid 'plates' argument")
    }
    if (wells == 96L) {
        col <- 12L
        row <- 8L
    } else if (wells == 384L) {
        col <- 24L
        row <- 16L
    } else {
        stop("Invalid 'wells' argument")
    }
    col <- 1L:col %>%
        str_pad(max(str_length(.)), pad = "0")
    row <- LETTERS[1L:row]
    plates <- 1L:plates %>%
        str_pad(max(str_length(.)), pad = "0")
    df <- expand.grid(plates, row, col)
    vector <- paste0(df[["Var1"]], "-", df[["Var2"]], df[["Var3"]]) %>%
        sort()
    # Remove control wells from vector:
    if (!is.numeric(controls) | !controls %in% 0L:12L) {
        stop("'controls' argument supports 0:12")
    }
    if (controls > 0L) {
        # Create a grep string matching the control wells
        grep <- 1L:controls %>%
            str_pad(max(str_length(col)), pad = "0") %>%
            paste(collapse = "|") %>%
            paste0("A(", ., ")$")
        # Remove the control wells using `grepl()`:
        vector <- vector[!grepl(grep, vector)]
    }
    # Add a prefix, if desired:
    if (!missing(prefix)) {
        if (!is_string(prefix)) {
            stop("Prefix must be a string")
        }
        vector <- paste0(prefix, "-", vector)
    }
    return(vector)
}
