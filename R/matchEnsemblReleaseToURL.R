#' Match Ensembl release to archive URL.
#'
#' @note Updated 2019-07-28.
#' @export
#'
#' @param release `integer(1)` or `character(1)`.
#'   Ensembl release (e.g. 96).
#'
#' @return `character(1)`.
#'   URL.
#'
#' @examples
#' matchEnsemblReleaseToURL(96L)
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
            stop("biomaRt timed out connecting to Ensembl.")
        }
    )
    assert(
        is.data.frame(map),
        isSubset(c("url", "version"), colnames(map))
    )
    if (!release %in% map[["version"]]) {
        stop(sprintf(
            "Supported Ensembl releases: %s.",
            toString(map[["version"]])
        ))
    }
    url <- map[["url"]][match(x = release, table = map[["version"]])]
    assert(grepl("ensembl\\.org", url))
    url
}
