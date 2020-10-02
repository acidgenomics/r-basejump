#' Download and cache a data file from DepMap into BiocFileCache
#'
#' @export
#' @note Updated 2020-10-02.
#'
#' @inheritParams acidroxygen::params
#' @param fileName `character(1)`.
#'   File name to store internally in `BiocFileCache`.
#'   Defaults to basename of URL.
#'
#' @return `character(1)`.
#'   Cached file path on disk.
#'
#' @examples
#' url <- pasteURL(
#'     basejumpTestsURL,
#'     "biocfilecache-test.txt",
#'     protocol = "none"
#' )
#' file <- cacheURL(url)
#' print(file)
cacheURL <- function(
    url,
    fileName = basename(url),
    verbose = TRUE
) {
    assert(
        isAURL(url),
        isString(fileName),
        isFlag(verbose)
    )
    if (isTRUE(verbose)) {
        items <- c("URL" = url)
        if (!identical(basename(url), fileName)) {
            items <- c(items, "File" = fileName)
        }
        cli_dl(items)
    }
    bfc <- .biocPackageCache()
    rid <- bfcquery(
        x = bfc,
        query = fileName,
        field = "rname",
        exact = TRUE
    )[["rid"]]
    if (!hasLength(rid)) {
        if (isTRUE(verbose)) {
            cli_alert(sprintf(
                "Caching {.file %s} at {.path %s}.",
                fileName, bfccache(bfc)
            ))
        }
        rid <- names(bfcadd(
            x = bfc,
            rname = fileName,
            fpath = url,
            download = TRUE
        ))
    }
    if (!isFALSE(bfcneedsupdate(x = bfc, rids = rid))) {
        bfcdownload(x = bfc, rid = rid, ask = FALSE)
    }
    out <- unname(bfcrpath(x = bfc, rids = rid))
    assert(isAFile(out))
    out
}



#' Prepare BiocFileCache for package
#'
#' @note Updated 2020-10-02.
#' @noRd
.biocPackageCache <- function(pkg = packageName()) {
    BiocFileCache(
        cache = user_cache_dir(appname = pkg),
        ask = TRUE
    )
}
