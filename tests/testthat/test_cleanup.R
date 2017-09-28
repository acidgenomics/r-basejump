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
            fixNA(),
        data.frame(a = c("foo", NA),
                   b = c(NA, "bar")))

    # DataFrame
    expect_equal(
        DataFrame(a = c("foo", ""),
                  b = c(NA, "bar")) %>%
            fixNA(),
        DataFrame(a = c("foo", NA),
                  b = c(NA, "bar")))

    # tbl_df
    expect_equal(
        tibble(a = c("foo", ""),
               b = c(NA, "bar")) %>%
            fixNA(),
        tibble(a = c("foo", NA),
               b = c(NA, "bar")))

    # ANY (list)
    expect_equal(
        fixNA(list(a = 1L)),
        list(a = 1L))
})



# makeNames ====
# character vector
vec <- c("G2MScore",
         "hello world",
         "HELLO WORLD",
         "RNAi clones",
         "tx2gene",
         "TX2GeneID",
         "worfdbHTMLRemap")

# named character vector
named <- c(Item.A = "hello world", Item.B = "HELLO WORLD")

# data.frame
df <- head(mtcars)

# tibble
tbl <- head(starwars)

# list
lst <- list(Item.A = c(1L, 2L),
            Item.B = c(3L, 4L))



test_that("camel", {
    expect_equal(
        camel(123L),
        123L)
    expect_equal(
        camel(NA),
        NA)
    expect_error(camel())

    # character vector
    expect_equal(
        camel(vec, strict = TRUE),
        c("g2mScore",
          "helloWorld",
          "helloWorld",
          "rnaiClones",
          "tx2gene",
          "tx2GeneId",
          "worfdbHtmlRemap"))
    expect_equal(
        camel(vec, strict = FALSE),
        c("g2mScore",
          "helloWorld",
          "helloWORLD",
          "rnaiClones",
          "tx2gene",
          "tx2GeneID",
          "worfdbHTMLRemap"))

    # named character vector
    expect_equal(
        camel(named, strict = TRUE),
        c(itemA = "hello world",
          itemB = "HELLO WORLD"))

    # data.frame with rownames
    expect_equal(
        camel(df, rownames = TRUE, strict = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "mazdaRx4")

    # data.frame with unset rownames (ignore in `.checkRownames()`)
    expect_equal(
        mtcars %>%
            set_rownames(NULL) %>%
            camel(rownames = TRUE, strict = TRUE) %>%
            rownames(),
        as.character(1L:nrow(mtcars)))

    # tibble
    expect_equal(
        tbl[, 1L:5L] %>%
            camel(strict = TRUE) %>%
            colnames(),
        c("name", "height", "mass", "hairColor", "skinColor"))

    # list
    expect_equal(
        camel(lst, strict = TRUE),
        list(itemA = c(1L, 2L),
             itemB = c(3L, 4L)))
})



test_that("dotted", {
    expect_equal(
        dotted(123L),
        123L)
    expect_equal(
        dotted(NA),
        NA)
    expect_error(dotted())

    # character vector
    expect_equal(
        dotted(vec, strict = TRUE),
        c("g2m.score",
          "hello.world",
          "hello.world",
          "rnai.clones",
          "tx2gene",
          "tx2.gene.id",
          "worfdb.html.remap"))
    expect_equal(
        dotted(vec, strict = FALSE),
        c("G2MScore",
        "hello.world",
        "HELLO.WORLD",
        "RNAi.clones",
        "tx2gene",
        "TX2GeneID",
        "worfdbHTMLRemap"))

    # named character vector
    expect_equal(
        dotted(named, strict = TRUE),
        c(item.a = "hello world",
          item.b = "HELLO WORLD"))

    # data.frame with rownames
    expect_equal(
        dotted(df, rownames = TRUE, strict = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "mazda.rx4")

    # tibble
    expect_equal(
        tbl[, 1L:5L] %>%
            dotted(strict = TRUE) %>%
            colnames(),
        c("name", "height", "mass", "hair.color", "skin.color"))

    # list
    expect_equal(
        dotted(lst, strict = TRUE),
        list(item.a = c(1L, 2L),
             item.b = c(3L, 4L)))
})



test_that("snake", {
    expect_equal(
        snake(123L),
        123L)
    expect_equal(
        snake(NA),
        NA)
    expect_error(snake())

    # character vector
    expect_equal(
        snake(vec),
        c("g2m_score",
          "hello_world",
          "hello_world",
          "rnai_clones",
          "tx2gene",
          "tx2_gene_id",
          "worfdb_html_remap"))

    # named character vector
    expect_equal(
        snake(named),
        c(item_a = "hello world",
          item_b = "HELLO WORLD"))

    # data.frame with rownames
    expect_equal(
        snake(df, rownames = TRUE) %>%
            rownames() %>%
            .[[1L]],
        "mazda_rx4")

    # tibble
    expect_equal(
        tbl[, 1L:5L] %>%
            snake() %>%
            colnames(),
        c("name", "height", "mass", "hair_color", "skin_color"))

    # list
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
                   c = c("B", NA, "D")) %>%
            removeNA(),
        data.frame(a = c("A", "C"),
                   c = c("B", "D"),
                   row.names = c(1L, 3L)))

    # vector
    #' # Support for vectors (using `stats::na.omit()`)
    expect_equal(
        removeNA(c("hello", "world", NA)) %>%
            as.character(),
        c("hello", "world"))
    expect_equal(
        removeNA(c(1L, 2L, NA)) %>%
            as.integer(),
        c(1L, 2L))
})



# sortUnique ====
test_that("sortUnique", {
    expect_equal(
        sortUnique(c("milk", "eggs", "eggs", NA)),
        c("eggs", "milk"))
})
