#' @name markdown
#' @examples
#' data(rse)
#' markdown(rse)
NULL



# markdown =====================================================================
markdown.SummarizedExperiment <-  # nolint
    function(object) {
        sampleData(object) %>%
            as.data.frame() %>%
            kable()
    }



#' @rdname markdown
#' @export
setMethod(
    f = "markdown",
    signature = signature("SummarizedExperiment"),
    definition = markdown.SummarizedExperiment
)



# markdownHeader ===============================================================
#' Markdown Header
#'
#' Generate a Markdown header (levels 1-7) in any R Markdown code block. When
#' calling inside an `asis` chunk, set `asis = TRUE`.
#'
#' @inheritParams params
#' @export
#'
#' @param text `string`. Header text.
#' @param level `scalar integer`. Header level (1-7).
#' @param tabset `boolean`. Include tabset marker.
#' @param asis `boolean`. Set this to `TRUE` when using the function inside a
#'   loop or inside an R Markdown chunk with '`results = "asis"`' enabled.
#'
#' @seealso
#' [Markdown Syntax](https://daringfireball.net/projects/markdown/syntax).
#'
#' @return
#' - "`asis = TRUE`": [knitr::asis_output()].
#' - "`asis = FALSE`": [base::writeLines()].
#'
#' @examples
#' markdownHeader("Header", level = 2L)
#' markdownHeader("Header", tabset = TRUE)
#' markdownHeader("Header", asis = TRUE)
markdownHeader <- function(
    text,
    level = 2L,
    tabset = FALSE,
    asis = FALSE
) {
    assert_is_a_string(text)
    assert_all_are_non_missing_nor_empty_character(text)
    assertIsHeaderLevel(level)
    assert_is_a_bool(tabset)
    assert_is_a_bool(asis)

    # Add the header level
    header <- paste(str_dup("#", level), text)

    # Append tabset label
    if (isTRUE(tabset)) {
        header <- paste(header, "{.tabset}")
    }

    # Return
    if (isTRUE(asis)) {
        writeLines(c("", "", header, ""))
    } else {
        header %>%
            # Ensure trailing line break
            paste0("\n") %>%
            # Specify that output should be handled as Markdown text
            structure(format = "markdown") %>%
            asis_output()
    }
}



#' @rdname markdownHeader
#' @export
mdHeader <- markdownHeader



# markdownLink =================================================================
#' Markdown Hyperlink
#'
#' For use in `asis` blocks only.
#'
#' @inherit markdownHeader
#' @author Rory Kirchner, Michael Steinbaugh
#' @export
#'
#' @param url `string`. URL.
#' @param title `string` or `NULL`. Link title attribute. This will appear in a
#'   mouse-over pop-up box.
#'
#' @return Markdown-formatted link.
#'
#' @examples
#' markdownLink(
#'     text = "R",
#'     url = "https://www.r-project.org",
#'     title = "The R Project for Statistical Computing"
#' )
markdownLink <- function(
    text,
    url,
    title = NULL
) {
    assert_is_a_string(text)
    assert_is_a_string(url)
    assertIsAStringOrNULL(title)
    x <- paste0("[", text, "](", url, ")")
    if (!is.null(title)) {
        x <- paste0(x, ": ", title)
    }
    writeLines(x)
}



#' @rdname markdownLink
#' @export
mdLink <- markdownLink



# markdownList =================================================================
#' Markdown List
#'
#' Include a Markdown-formatted list, either ordered or unordered. This function
#' works in any R Markdown code block. When calling from inside an `asis` chunk,
#' set `asis = TRUE`.
#'
#' @inherit markdownHeader
#' @export
#'
#' @param ordered `boolean`. Ordered ("`TRUE`") or unordered ("`FALSE`").
#'
#' @examples
#' groceries <- c("milk", "eggs")
#' markdownList(groceries)
#' markdownList(groceries, ordered = TRUE)
#' markdownList(groceries, asis = TRUE)
markdownList <- function(
    text,
    ordered = FALSE,
    asis = FALSE
) {
    assert_is_atomic(text)
    text <- as.character(text)
    assert_all_are_not_na(text)
    assert_all_are_non_missing_nor_empty_character(text)
    assert_is_a_bool(ordered)
    assert_is_a_bool(asis)

    list <- vapply(
        X = seq_along(text),
        FUN = function(a) {
            if (isTRUE(ordered)) {
                prefix <- paste0(a, ".")
            } else {
                prefix <- "-"
            }
            paste(prefix, text[[a]])
        },
        FUN.VALUE = character(1L)
    )

    if (isTRUE(asis)) {
        writeLines(c("", list, ""))
    } else {
        list %>%
            # Add a trailing line break
            paste0("\n") %>%
            # Specify that output should be handled as Markdown text
            structure(format = "markdown") %>%
            asis_output()
    }
}



#' @rdname markdownList
#' @export
mdList <- markdownList



# markdownPlotlist =============================================================
#' Markdown Plotlist
#'
#' Supports using a named `list` containing multiple `ggplot` objects, which
#' can be used in an R Markdown report, separated by headers. Internally, the
#' headers are generated with the [markdownHeader()] function.
#'
#' @export
#'
#' @inheritParams params
#'
#' @return Graphical output of plots, separated by Markdown headers.
#'
#' @seealso [cowplot::plot_grid()].
#'
#' @examples
#' loadRemoteData(url = file.path(basejumpCacheURL, "plotlist.rda"))
#' names(plotlist)
#' markdownPlotlist(plotlist)
markdownPlotlist <- function(
    plotlist,
    headerLevel = 2L
) {
    assert_is_list(plotlist)
    assert_has_names(plotlist)
    assertIsHeaderLevel(headerLevel)
    invisible(mapply(
        name = names(plotlist),
        plot = plotlist,
        MoreArgs = list(headerLevel = headerLevel),
        FUN = function(name, plot, headerLevel) {
            assert_is_a_string(name)
            markdownHeader(name, level = headerLevel, asis = TRUE)
            show(plot)
            plot
        },
        SIMPLIFY = FALSE
    ))
}



#' @rdname markdownPlotlist
#' @export
mdPlotlist <- markdownPlotlist



# markdownTables ===============================================================
#' Create Multiple Tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' Knit multiple tables in a single R Markdown chunk.
#'
#' @export
#'
#' @param list Named `list`. Column data that can be coerced to `data.frame`.
#' @param captions `character` or `NULL`. Table captions. If `NULL`, the names
#'   of the list will be used automatically as captions.
#' @param force `boolean`. Force knit output using [knitr::asis_output()].
#'   Recommended for development and unit testing only.
#'
#' @return `asis_output` if in a knit call or `list`.
#'
#' @seealso
#' - [knitr::kable()].
#' - [Stack Overflow post](https://stackoverflow.com/a/35149103/3911732).
#'
#' @examples
#' list <- list(mpg = head(ggplot2::mpg), mtcars = head(datasets::mtcars))
#' captions <- c(
#'     mpg = "Miles per gallon",
#'     mtcars = "Motor Trend car road tests"
#' )
#' markdownTables(list = list, captions = captions)
markdownTables <- function(
    list,
    captions = NULL,
    force = FALSE
) {
    assert_is_list(list)
    assert_is_any_of(captions, classes = c("character", "NULL"))
    if (is.null(captions)) {
        assert_has_names(list)
        captions <- names(list)
    }
    assert_is_character(captions)
    assert_are_same_length(list, captions)
    assert_is_a_bool(force)
    output <- opts_knit[["get"]]("rmarkdown.pandoc.to")
    if (!is.null(output) || isTRUE(force)) {
        tables <- mapply(
            x = list,
            caption = captions,
            FUN = function(x, caption) {
                kable(x = as.data.frame(x), caption = caption)
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        asis_output(tables)
    } else {
        # Return the unmodified list if not in a knit call.
        list
    }
}



# prepareTemplate ==============================================================
# We're covering the code below in bcbioRNASeq and bcbioSingleCell.
# nocov start



#' Prepare R Markdown Template File
#'
#' If the required template dependency files aren't present, copy them from the
#' requested package. Existing files are not overwritten by default. This
#' function will copy dependency files from a requested package inside the
#' `rmarkdown/shared` directory. If a package doesn't contain this
#' subdirectory, the function will return an error.
#'
#' This code is used internally by:
#'
#' - `bcbioRNASeq::prepareRNASeqTemplate()`.
#' - `bcbioSingleCell::prepareSingleCellTemplate()`.
#'
#' @export
#'
#' @inheritParams params
#'
#' @param package `string`. Name of package containing the R Markdown template.
#' @param overwrite `boolean`. Should existing destination files be overwritten?
#' @param sourceDir `string`. File path to shared source file directory.
#'   Normally this can be left `NULL` when working in a standard interactive
#'   session, but is necessary when developing code in a devtools package
#'   environment loaded with `devtools::load_all()`.
#'
#' @return Invisible `logical`. Was the file copied?.
#'
#' @examples
#' ## RNA-seq template.
#' \dontrun{
#' # prepareTemplate(package = "bcbioRNASeq")
#' }
#'
#' ## Single-cell RNA-seq template.
#' \dontrun{
#' # prepareTemplate(package = "bcbioSingleCell")
#' }
prepareTemplate <- function(
    package,
    overwrite = FALSE,
    sourceDir = NULL,
    ...
) {
    assert_is_a_string(package)
    assert_is_subset(package, rownames(installed.packages()))
    assertIsAStringOrNULL(sourceDir)
    assert_is_a_bool(overwrite)

    # Shared file source directory. Keeping the `sourceDir` argument because
    # devtools attempts to intercept `system.file`, and this can cause path
    # issues during development.
    if (is.null(sourceDir)) {
        sourceDir <- system.file(
            "rmarkdown/shared",
            package = package,
            mustWork = TRUE
        )
    }
    assert_all_are_dirs(sourceDir)

    # Get vector of all shared files.
    files <- list.files(sourceDir, full.names = TRUE)
    assert_is_non_empty(files)

    # Copy files to working directory.
    copied <- vapply(
        X = files,
        FUN = function(file) {
            file.copy(
                from = file,
                to = basename(file),
                overwrite = overwrite
            )
        },
        FUN.VALUE = logical(1L)
    )
    names(copied) <- basename(files)

    invisible(copied)
}



# nocov end
