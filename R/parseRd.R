#' Parse R Documentation
#'
#' Modified version of `tools:::.Rd_get_metadata()` that keeps whitespace and
#' returns `character` instead of `matrix`.
#'
#' @family Documentation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @param object `Rd`. R documentation, returned from [tools::Rd_db()].
#' @param tag `string`. Desired metadata type. For example, these are supported:
#'   - "`title`".
#'   - "`description`".
#'   - "`usage`".
#'   - "`arguments`".
#'   - "`value`".
#'   - "`references`".
#'   - "`seealso`".
#'   - "`examples`".
#'
#' @seealso [tools::Rd_db()].
#'
#' @examples
#' db <- tools::Rd_db("base")
#' head(names(db))
#' Rd <- db[["nrow.Rd"]]
#' class(Rd)
#' summary(Rd)
#' RdTags(Rd)
#' examples <- parseRd(Rd, tag = "examples")
#' print(examples)
parseRd <- function(object, tag) {
    assert_is_all_of(object, "Rd")
    assert_is_a_string(tag)

    tags <- RdTags(object)
    assert_is_subset(tag, tags)

    # Get the metadata that matches the requested tag.
    data <- object[tags == tag]

    # Coerce to character, not a character matrix.
    data <- as.character(sapply(data, as.character))

    # Strip trailing newlines and superfluous whitespace.
    data <- trimws(data, which = "right")

    # Strip leading and trailing carriage returns, if present.
    if (data[[1L]] == "") {
        data <- data[-1L]
    }
    if (data[[length(data)]] == "") {
        data <- data[-length(data)]
    }

    data
}
