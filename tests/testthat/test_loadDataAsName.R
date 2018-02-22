context("loadDataAsName")

download.file(
    url = "http://basejump.seq.cloud/multi.rda",
    destfile = "multi.rda")
download.file(
    url = "http://basejump.seq.cloud/mtcars.rda",
    destfile = "mtcars.rda")
download.file(
    url = "http://basejump.seq.cloud/starwars.rda",
    destfile = "starwars.rda")

test_that("Dot object key-value pair method", {
    loaded <- loadDataAsName(
        newName1 = "mtcars",
        newName2 = "starwars",
        dir = "."
    )
    expect_is(loaded, "character")
    expect_true(exists("newName1", inherits = FALSE))
    expect_true(exists("newName2", inherits = FALSE))
})

test_that("Missing files", {
    expect_error(
        loadDataAsName(newName = "mtcars.rda"),
        "is_existing_file : "
    )
    expect_error(
        loadDataAsName(newName = "XXXXXX"),
        "is_existing_file : "
    )
})

test_that("Legacy named character method", {
    loaded <- loadDataAsName(c(test = "mtcars"))
    expect_is(loaded, "character")
    expect_true(exists("test", inherits = FALSE))
})

test_that("Multiple objects in single file", {
    expect_error(
        loadDataAsName(newName = "multi"),
        "is_a_string : loaded has length 2, not 1."
    )
})

test_that("Invalid arguments", {
    expect_error(
        loadDataAsName(newName = "mtcars", dir = "XXX"),
        "is_dir : "
    )
    expect_error(
        loadDataAsName(newName = "mtcars", envir = "XXX"),
        "is_environment : envir"
    )
})

file_delete(c("multi.rda", "mtcars.rda", "starwars.rda"))
