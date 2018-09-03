# Using the unexported `RdTags()` parser internally.
.RdTags <- tools:::RdTags



#' Parse R Documentation
#'
#' Modified version of `tools:::.Rd_get_metadata()` that keeps whitespace and
#' returns `character` instead of `matrix`.
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso
#' - [tools::Rd_db()].
#'
#' @examples
#' db <- tools::Rd_db("base")
#' head(names(db))
#' Rd <- db[["nrow.Rd"]]
#' class(Rd)
#' summary(Rd)
#' examples <- parseRd(Rd, kind = "examples")
#' print(examples)
parseRd <- function(object, kind) {
    assert_is_all_of(object, "Rd")
    assert_is_a_string(kind)

    # Get the metadata that matches the requested kind.
    data <- object[.RdTags(object) == sprintf("\\%s", kind)]

    # Error if there is no match to `kind` argument.
    if (!length(data)) {
        stop(paste("Rd does not contain", deparse(kind)))
    }

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
