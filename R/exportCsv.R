#' Export CSV file
#'
#' Quickly export a comma separated values (CSV) file to the `results` folder.
#'
#' @export
#' @importFrom utils write.csv
#' @param ... Unquoted names of existing objects to export
#' @param dir Target directory
#' @examples
#' \dontrun{
#' x <- data.frame(col1 = 1:10, col2 = 11:20)
#' y <- data.frame(col3 = 21:30, col4 = 31:40)
#' exportCsv(x, y)
#' }
exportCsv <- function(..., dir = "results") {
    if (!isString(dir)) {
        stop("dir must be a string")
    }
    if (!file.exists(dir)) {
        dir.create(dir)
    }
    objs <- get_objs_from_dots(dots(...))
    paths <- file.path(dir, paste0(objs, ".csv"))
    message("Exporting ", paste(unlist(objs), collapse = ", "),
            " as ", paste(basename(paths), collapse = ", "))
    for (a in 1:length(objs)) {
        utils::write.csv(list(...)[[a]], paths[a])
    }
    invisible()
}
