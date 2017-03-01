# This function fails with `rmarkdown::render()`. Likely need to change
# environment settings for getting objects from dots. Not exported until
# this issue is fixed.

#' Write CSV files
#'
#' Quickly write comma separated values (CSV) files. Uses non-standard
#' evaluation to obtain objects.
#'
# @export
#' @importFrom readr write_csv
#' @importFrom tibble rownames_to_column
#' @importFrom utils write.csv
#' @param ... Unquoted names of existing objects to export
#' @param dir Target directory
#' @examples
#' \dontrun{
#' x <- data.frame(col1 = 1:10, col2 = 11:20)
#' y <- data.frame(col3 = 21:30, col4 = 31:40)
#' writeCsv(x, y)
#' }
writeCsv <- function(..., dir = "results") {
    if (!isString(dir)) {
        stop("dir must be a string")
    }
    if (!file.exists(dir)) {
        dir.create(dir)
    }
    objs <- get_objs_from_dots(dots(...))
    lapply(objs, function(object) {
        path <- file.path(dir, paste0(object, ".csv"))
        message("Writing ", object, " as ", basename(path))
        df <- get(object)
        if (is.data.frame(df) & !is_tibble(df)) {
            df <- tibble::rownames_to_column(df)
        }
        readr::write_csv(df, path)
    })
    invisible()
}
# 2x faster than base method:
# utils::write.csv(object, path)
