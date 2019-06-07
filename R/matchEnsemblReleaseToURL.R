#' Match Ensembl release to archive URL.
#' @param release `integer(1)` or `character(1)`.
#'   Ensembl release (e.g. 96).
#' @return URL.
#' @examples
#' matchEnsemblReleaseToURL(96L)
# Last modified 2019-06-07.
matchEnsemblReleaseToURL <- function(release) {
    requireNamespace("biomaRt", quietly = TRUE)
    if (is.null(release)) {
        return("http://useast.ensembl.org")
    }
    release <- as.character(release)
    assert(isString(release))
    map <- tryCatch(
        expr = biomaRt::listEnsemblArchives(),
        error = function(e) {
            stop("biomaRt timed out connecting to Ensembl.", call. = FALSE)
        }
    )
    assert(
        is.data.frame(map),
        isSubset(c("url", "version"), colnames(map))
    )
    if (!release %in% map[["version"]]) {
        stop(paste(
            "Supported Ensembl releases:",
            toString(map[["version"]])
        ))
    }
    url <- map[["url"]][match(x = release, table = map[["version"]])]
    assert(grepl("ensembl\\.org", url))
    url
}
