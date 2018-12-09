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
#' - "`asis = TRUE`": `knitr::asis_output()`.
#' - "`asis = FALSE`": `writeLines()`.
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
    assertString(text)
    assert_all_are_non_missing_nor_empty_character(text)
    assertIsHeaderLevel(level)
    assertFlag(tabset)
    assertFlag(asis)

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
#' @usage NULL
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
    assertString(text)
    assertString(url)
    assertIsStringOrNULL(title)
    x <- paste0("[", text, "](", url, ")")
    if (!is.null(title)) {
        x <- paste0(x, ": ", title)
    }
    writeLines(x)
}



#' @rdname markdownLink
#' @usage NULL
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
    assertAtomic(text)
    text <- as.character(text)
    assert_all_are_not_na(text)
    assert_all_are_non_missing_nor_empty_character(text)
    assertFlag(ordered)
    assertFlag(asis)

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
#' @usage NULL
#' @export
mdList <- markdownList



# markdownPlots ================================================================
#' Markdown Plots
#'
#' Supports using a named `list` containing multiple `ggplot` objects, which
#' can be used in an R Markdown report, separated by headers. Internally, the
#' headers are generated with the `markdownHeader()` function.
#'
#' @inheritParams params
#' @export
#'
#' @param list `list`. Named list containing `ggplot` objects.
#'
#' @return Graphical output of plots, separated by Markdown headers.
#'
#' @seealso `cowplot::plot_grid()`.
#'
#' @examples
#' loadRemoteData(url = file.path(basejumpCacheURL, "plotlist.rda"))
#' names(plotlist)
#' markdownPlots(list = plotlist)
markdownPlots <- function(list, headerLevel = 2L) {
    assert_is_list(list)
    assert_has_names(list)
    assertIsHeaderLevel(headerLevel)
    invisible(mapply(
        name = names(list),
        plot = list,
        MoreArgs = list(headerLevel = headerLevel),
        FUN = function(name, plot, headerLevel) {
            assertString(name)
            markdownHeader(name, level = headerLevel, asis = TRUE)
            show(plot)
            plot
        },
        SIMPLIFY = FALSE
    ))
}



#' @rdname markdownPlots
#' @usage NULL
#' @export
mdPlots <- markdownPlots



# markdownTables ===============================================================
#' Create Multiple Tables in LaTeX, HTML, Markdown and reStructuredText
#'
#' Knit multiple tables in a single R Markdown chunk.
#'
#' @note `knitr::kable()` now supports multiple tables as a `list` for the `x`
#'   argument, but it still only supports a single caption. `markdownTables()`
#'   extends this functionality, but supporting captions for each table.
#'
#' @export
#'
#' @param list Named `list`. Column data that can be coerced to `data.frame`.
#' @param captions `character` or `NULL`. Table captions. If `NULL`, the names
#'   of the list will be used automatically as captions.
#' @param force `boolean`. Force knit output using `knitr::asis_output()`.
#'   Recommended for development and unit testing only.
#'
#' @return `asis_output` if in a knit call or `list`.
#'
#' @seealso
#' - `knitr::kable()`.
#' - [Stack Overflow post](https://stackoverflow.com/a/35149103/3911732).
#'
#' @examples
#' list <- list(
#'     mpg = head(ggplot2::mpg),
#'     mtcars = head(datasets::mtcars)
#' )
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
    assertAnyClass(captions, classes = c("character", "NULL"))
    if (is.null(captions)) {
        assert_has_names(list)
        captions <- names(list)
    }
    assertCharacter(captions)
    assert_are_same_length(list, captions)
    assertFlag(force)
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




#' @rdname markdownTables
#' @usage NULL
#' @export
mdTables <- markdownTables
