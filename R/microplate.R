#' Microtiter Plate Well Identifiers
#'
#' @importFrom stringr str_length str_pad
#'
#' @param plates Number of plates.
#' @param wells Number of wells (`96`, `384`).
#' @param controls Number of control wells.
#' @param prefix *Optional*. Plate name prefix.
#'
#' @return Character vector containing well identifiers.
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
    prefix = NULL) {
    assert_is_integer(plates)
    assert_is_scalar(plates)
    assert_is_integer(wells)
    assert_is_scalar(wells)
    assert_is_subset(wells, c(96L, 384L))
    assert_is_integer(controls)
    assert_is_scalar(controls)
    assert_is_subset(controls, 0L:12L)
    assert_is_a_string_or_null(prefix)

    if (wells == 96L) {
        row <- 8L
        col <- 12L
    } else if (wells == 384L) {
        row <- 16L
        col <- 24L
    }

    row <- LETTERS[1L:row]
    col <- 1L:col %>%
        str_pad(width = max(str_length(.)), pad = "0")
    plates <- 1L:plates %>%
        str_pad(width = max(str_length(.)), pad = "0")

    df <- expand.grid(plates, row, col)
    vector <- sort(paste0(df[["Var1"]], "-", df[["Var2"]], df[["Var3"]]))

    # Prepare control wells
    if (controls > 0L) {
        # Create a grep string matching the control wells
        grep <- str_pad(
            1L:controls,
            width = max(str_length(col)),
            pad = "0") %>%
            paste(collapse = "|") %>%
            paste0("A(", ., ")$")
        # Remove the control wells using `grepl()`:
        vector <- vector[!grepl(grep, vector)]
    }

    # Add a prefix, if desired:
    if (is_a_string(prefix)) {
        vector <- paste0(prefix, "-", vector)
    }

    vector
}
