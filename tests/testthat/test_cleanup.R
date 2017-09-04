context("Cleanup Utilities")

# fixNA ====
test_that("fixNA", {
    # character vector
    expect_equal(
        fixNA(c(1L, "x", "", "NA")),
        c("1", "x", NA, NA))

    # data.frame
    expect_equal(
        data.frame(a = c("foo", ""),
                   b = c(NA, "bar")) %>%
            fixNA,
        data.frame(a = c("foo", NA),
                   b = c(NA, "bar")))

    # DataFrame
    expect_equal(
        DataFrame(a = c("foo", ""),
                  b = c(NA, "bar")) %>%
            fixNA,
        DataFrame(a = c("foo", NA),
                  b = c(NA, "bar")))

    # tbl_df
    expect_equal(
        tibble(a = c("foo", ""),
               b = c(NA, "bar")) %>%
            fixNA,
        tibble(a = c("foo", NA),
               b = c(NA, "bar")))

    # ANY (list)
    expect_equal(
        fixNA(list(a = 1L)),
        list(a = 1L))
})



# makeNames ====
# Named character vector
named <- c(Item.A = "hello world", Item.B = "HELLO WORLD")

# Data frame
df <- head(mtcars)

# Tibble
tbl <- head(starwars)

# List
lst <- list(Item.A = c(1L, 2L),
            Item.B = c(3L, 4L))



test_that("camel", {
    # Unnamed character vector
    expect_equal(camel("hello world"), "helloWorld")
    expect_equal(camel("HELLO WORLD"), "helloWorld")
    expect_equal(camel("RNAi clones"), "rnaiClones")
    expect_equal(camel("worfdbHTMLRemap"), "worfdbHtmlRemap")
    expect_equal(camel(123L), 123L)
    expect_equal(camel(NA), NA)
    expect_error(camel())

    # Named character vector
    expect_equal(
        camel(named),
        c(itemA = "hello world",
          itemB = "HELLO WORLD"))

    # data.frame (with rownames coverage)
    expect_equal(
        camel(df, rownames = TRUE) %>%
            rownames %>%
            .[[1L]],
        "mazdaRx4")
    expect_equal(
        snake(df, rownames = TRUE) %>%
            rownames %>%
            .[[1L]],
        "mazda_rx4")
    expect_equal(
        dotted(df, rownames = TRUE) %>%
            rownames %>%
            .[[1L]],
        "mazda.rx4")
    # Unset rownames (ignore in `.checkRownames()`)
    expect_equal(
        mtcars %>%
            set_rownames(NULL) %>%
            camel(rownames = TRUE) %>%
            rownames,
        as.character(1L:nrow(mtcars)))

    # tibble
    expect_equal(
        tbl[, 1L:5L] %>%
            camel %>%
            colnames,
        c("name", "height", "mass", "hairColor", "skinColor"))

    # list
    expect_equal(
        camel(lst),
        list(itemA = c(1L, 2L),
             itemB = c(3L, 4L)))
})



test_that("dotted", {
    # Unnamed character vector
    expect_equal(dotted("hello world"), "hello.world")
    expect_equal(dotted("HELLO WORLD"), "hello.world")
    expect_equal(dotted("RNAi clones"), "rnai.clones")
    expect_equal(dotted("worfdbHTMLRemap"), "worfdb.html.remap")
    expect_equal(dotted(123L), 123L)
    expect_equal(dotted(NA), NA)
    expect_error(dotted())

    # Named character vector
    expect_equal(
        dotted(named),
        c(item.a = "hello world",
          item.b = "HELLO WORLD"))

    # Tibble
    expect_equal(
        tbl[, 1L:5L] %>%
            dotted %>%
            colnames,
        c("name", "height", "mass", "hair.color", "skin.color"))

    # List
    expect_equal(
        dotted(lst),
        list(item.a = c(1L, 2L),
             item.b = c(3L, 4L)))
})



test_that("snake", {
    # Unnamed character vector
    expect_equal(snake("hello world"), "hello_world")
    expect_equal(snake("HELLO WORLD"), "hello_world")
    expect_equal(snake("RNAi clones"), "rnai_clones")
    expect_equal(snake("worfdbHTMLRemap"), "worfdb_html_remap")
    expect_equal(snake(123L), 123L)
    expect_equal(snake(NA), NA)
    expect_error(snake())

    # Named character vector
    expect_equal(
        snake(named),
        c(item_a = "hello world",
          item_b = "HELLO WORLD"))

    # Tibble
    expect_equal(
        tbl[, 1L:5L] %>%
            snake %>%
            colnames,
        c("name", "height", "mass", "hair_color", "skin_color"))

    # List
    expect_equal(
        snake(lst),
        list(item_a = c(1L, 2L),
             item_b = c(3L, 4L)))
})



# removeNA ====
test_that("removeNA", {
    # data.frame
    expect_equal(
        data.frame(a = c("A", NA, "C"),
                   b = c(NA, NA, NA),
                   c = c("B", NA, "D")) %>% removeNA,
        data.frame(a = c("A", "C"),
                   c = c("B", "D"),
                   row.names = c(1L, 3L)))

    # vector
    #' # Support for vectors (using `stats::na.omit()`)
    expect_equal(
        removeNA(c("hello", "world", NA)) %>%
            as.character,
        c("hello", "world"))
    expect_equal(
        removeNA(c(1L, 2L, NA)) %>%
            as.integer,
        c(1L, 2L))
})



# sortUnique ====
test_that("sortUnique", {
    expect_equal(
        sortUnique(c("milk", "eggs", "eggs", NA)),
        c("eggs", "milk"))
})
