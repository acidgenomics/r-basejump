context("Cleanup Utilities")

# Named character vector
namedVec <- c(Item.A = "hello world", Item.B = "HELLO WORLD")

# Data frame
df <- head(mtcars)

# Tibble
tbl <- head(starwars)

# List
lst <- list(Item.A = c(1, 2),
            Item.B = c(3, 4))



test_that("camel", {
    # Unnamed character vector
    expect_equal(camel("hello world"), "helloWorld")
    expect_equal(camel("HELLO WORLD"), "helloWorld")
    expect_equal(camel("RNAi clones"), "rnaiClones")
    expect_equal(camel("worfdbHTMLRemap"), "worfdbHtmlRemap")
    expect_equal(camel(123), 123)
    expect_equal(camel(NA), NA)
    expect_error(camel())

    # Named character vector
    expect_equal(
        camel(namedVec),
        c(itemA = "hello world",
          itemB = "HELLO WORLD"))

    # Tibble
    expect_equal(
        tbl[, 1L:5L] %>% camel %>% colnames,
        c("name", "height", "mass", "hairColor", "skinColor"))

    # List
    expect_equal(
        camel(lst),
        list(itemA = c(1, 2),
             itemB = c(3, 4)))
})



test_that("snake", {
    # Unnamed character vector
    expect_equal(snake("hello world"), "hello_world")
    expect_equal(snake("HELLO WORLD"), "hello_world")
    expect_equal(snake("RNAi clones"), "rnai_clones")
    expect_equal(snake("worfdbHTMLRemap"), "worfdb_html_remap")
    expect_equal(snake(123), 123)
    expect_equal(snake(NA), NA)
    expect_error(snake())

    # Named character vector
    expect_equal(
        snake(namedVec),
        c(item_a = "hello world",
          item_b = "HELLO WORLD"))

    # Tibble
    expect_equal(
        tbl[, 1L:5L] %>% snake %>% colnames,
        c("name", "height", "mass", "hair_color", "skin_color"))

    # List
    expect_equal(
        snake(lst),
        list(item_a = c(1, 2),
             item_b = c(3, 4)))
})



test_that("dotted", {
    # Unnamed character vector
    expect_equal(dotted("hello world"), "hello.world")
    expect_equal(dotted("HELLO WORLD"), "hello.world")
    expect_equal(dotted("RNAi clones"), "rnai.clones")
    expect_equal(dotted("worfdbHTMLRemap"), "worfdb.html.remap")
    expect_equal(dotted(123), 123)
    expect_equal(dotted(NA), NA)
    expect_error(dotted())

    # Named character vector
    expect_equal(
        dotted(namedVec),
        c(item.a = "hello world",
          item.b = "HELLO WORLD"))

    # Tibble
    expect_equal(
        tbl[, 1L:5L] %>% dotted %>% colnames,
        c("name", "height", "mass", "hair.color", "skin.color"))

    # List
    expect_equal(
        dotted(lst),
        list(item.a = c(1, 2),
             item.b = c(3, 4)))
})
