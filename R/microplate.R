#' Microtiter Plate Well Identifiers
#'
#' @family Metadata Functions
#' @export
#'
#' @param plates `scalar integer`. Number of plates.
#' @param wells `scalar integer`. Number of wells (`96`, `384`).
#' @param controls `scalar integer`. Number of control wells.
#' @param prefix `string` or `NULL`. Plate name prefix.
#'
#' @return `character`. Well identifiers.
#'
#' @examples
#' # Single 96-well plate.
#' microplate(wells = 96L)
#'
#' # 2 96-well plates.
#' microplate(plates = 2L, wells = 96L)
#'
#' # Single 384-well plate.
#' microplate(wells = 384L)
#'
#' # 2 96-well plates with 6 control wells per plate.
#' microplate(plates = 2L, wells = 96L, controls = 6L)
microplate <- function(
    plates = 1L,
    wells = 96L,
    controls = 0L,
    prefix = NULL
) {
    # Plates
    assertIsAnImplicitInteger(plates)
    plates <- as.integer(plates)
    assert_all_are_positive(plates)
    # Wells
    assertIsAnImplicitInteger(wells)
    wells <- as.integer(wells)
    assert_all_are_positive(wells)
    assert_is_subset(wells, c(96L, 384L))
    # Controls
    assertIsAnImplicitInteger(controls)
    controls <- as.integer(controls)
    assert_all_are_non_negative(controls)
    assert_is_subset(controls, 0L:12L)
    # Prefix
    assertIsAStringOrNULL(prefix)

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
            pad = "0"
        ) %>%
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
