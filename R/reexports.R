# If we use re-exports approach here, these will break pkgdown links.

#' @name colData
#' @importFrom SummarizedExperiment colData
#' @export
#' @noRd
NULL

#' @name DataFrame
#' @importFrom S4Vectors DataFrame
#' @export
#' @noRd
NULL


#' @name rowRanges
#' @importFrom SummarizedExperiment rowRanges
#' @export
#' @noRd
NULL

#' @name SummarizedExperiment
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @export
#' @noRd
NULL

#' @name tibble
#' @importFrom tibble as_tibble
#' @export
#' @noRd
NULL



#' @importFrom brio assignAndSaveData
#' @export
brio::assignAndSaveData

#' @importFrom brio basenameSansExt
#' @export
brio::basenameSansExt

#' @importFrom brio dots
#' @export
brio::dots

#' @importFrom brio export
#' @export
brio::export

#' @importFrom brio extPattern
#' @export
brio::extPattern

#' @importFrom brio import
#' @export
brio::import

#' @importFrom brio initDir
#' @export
brio::initDir

#' @importFrom brio loadData
#' @export
brio::loadData

#' @importFrom brio loadDataAsName
#' @export
brio::loadDataAsName

#' @importFrom brio loadRemoteData
#' @export
brio::loadRemoteData

#' @importFrom brio localOrRemoteFile
#' @export
brio::localOrRemoteFile

#' @importFrom brio naStrings
#' @export
brio::naStrings

#' @importFrom brio pasteURL
#' @export
brio::pasteURL

#' @importFrom brio rdataExtPattern
#' @export
brio::rdataExtPattern

#' @importFrom brio rdataLoadError
#' @export
brio::rdataLoadError

#' @importFrom brio realpath
#' @export
brio::realpath

#' @importFrom brio sanitizeColData
#' @export
brio::sanitizeColData

#' @importFrom brio sanitizeRowData
#' @export
brio::sanitizeRowData

#' @importFrom brio sanitizeRowRanges
#' @export
brio::sanitizeRowRanges

#' @importFrom brio saveData
#' @export
brio::saveData

#' @importFrom brio transmit
#' @export
brio::transmit

#' @importFrom brio writeCounts
#' @export
brio::writeCounts



#' @importFrom goalie extractLocal
#' @export
goalie::extractLocal

#' @importFrom goalie hasLocal
#' @export
goalie::hasLocal

#' @importFrom goalie matchArgsToDoCall
#' @export
goalie::matchArgsToDoCall

#' @importFrom goalie methodFormals
#' @export
goalie::methodFormals

#' @importFrom goalie methodFunction
#' @export
goalie::methodFunction

#' @importFrom goalie standardizeCall
#' @export
goalie::standardizeCall



#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`



#' @importFrom syntactic camel
#' @export
syntactic::camel

#' @importFrom syntactic dotted
#' @export
syntactic::dotted

#' @importFrom syntactic makeDimnames
#' @export
syntactic::makeDimnames

#' @importFrom syntactic makeNames
#' @export
syntactic::makeNames

#' @importFrom syntactic snake
#' @export
syntactic::snake

#' @importFrom syntactic upperCamel
#' @export
syntactic::upperCamel



#' @export
tibble::as_tibble

#' @export
tibble::tibble



#' @importFrom transformer as.SummarizedExperiment
#' @export
transformer::as.SummarizedExperiment

#' @importFrom transformer atomize
#' @export
transformer::atomize

#' @importFrom transformer coerceS4ToList
#' @export
transformer::coerceS4ToList

#' @importFrom transformer factorize
#' @export
transformer::factorize

#' @importFrom transformer flatFiles
#' @export
transformer::flatFiles

#' @importFrom methods coerce
#' @exportMethod coerce
NULL

setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "DataFrame",
            to = "tbl_df"
        ),
        where = "transformer"
    )
)

setAs(
    from = "data.frame",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "data.frame",
            to = "tbl_df"
        ),
        where = "transformer"
    )
)

setAs(
    from = "sparseMatrix",
    to = "DataFrame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "sparseMatrix",
            to = "DataFrame"
        ),
        where = "transformer"
    )
)

setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "sparseMatrix",
            to = "data.frame"
        ),
        where = "transformer"
    )
)

setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = getMethod(
        f = "coerce",
        signature(
            from = "tbl_df",
            to = "DataFrame"
        ),
        where = "transformer"
    )
)

setAs(
    from = "GRanges",
    to = "tbl_df",
    def = getMethod(
        f = "coerce",
        signature(
            from = "GRanges",
            to = "tbl_df"
        ),
        where = "transformer"
    )
)
